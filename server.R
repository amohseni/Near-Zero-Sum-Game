# RATIONALITY AND PREDICTION IN
# NEAR-ZERO-SUM GAMES OF UNCERTAIN INFORMATION
# << SERVER >>
# by Aydin Mohseni


# Load packages
library(shiny)
library(ggplot2)
library(ggthemes)

options(shiny.sanitize.errors = FALSE)

# Define server logic
shinyServer(function(input, output, session) {
  computeDynamics <- reactive({
    # Activate the simulation when RUN REPATED GAME button is pressed
    input$simulatePlay
    
    # Import payoff values a, b, c, d
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    c <- as.numeric(input$c)
    d <- as.numeric(input$d)
    
    # for the 2x2 zero-sum game
    #         H       T
    #   H | a, -a | b, -b |
    #   T | c, -c | d, -d |
    
    # Assign benchmark game payoffs for both players
    p1Payoff0 <- data.frame(c(a, b), c(c, d))
    rownames(p1Payoff0) <- c("H", "T")
    colnames(p1Payoff0) <- c("H", "T")
    p2Payoff0 <- data.frame(c(-a,-b), c(-c, -d))
    rownames(p2Payoff0) <- c("H", "T")
    colnames(p2Payoff0) <- c("H", "T")
    
    # v-Perturbation of payoffs
    vPertub <- function() {
      z <- rbeta(1, 1, 1) - (1 / 2)
      return(z)
    }
    
    # Perturb payoffs
    payoffPerturbations <- as.numeric(input$payoffPerturbations)
    for (i in 1:2) {
      for (j in 1:2) {
        p1Payoff0[i, j] <- p1Payoff0[i, j] + payoffPerturbations * vPertub()
        p2Payoff0[i, j] <-
          p2Payoff0[i, j] + payoffPerturbations * vPertub()
      }
    }
    
    # Expected payoff functions
    expPayoff <- function(p, y) {
      # Use the appropriate payoff table for the player
      if (y == 1) {
        w <- p1Payoff0
      } else {
        if (y == 2) {
          w <- p2Payoff0
        } else {
          print("expPayoff: input not a player")
        }
      }
      # Create empty payoffs
      z <- c(NA, NA)
      # Calculate expected payoff for playing Heads
      z[1] <- p * w[1, 1] + (1 - p) * w[1, 2]
      # Calculate expected payoff for playing Tails
      z[2] <- p * w[2, 1] + (1 - p) * (w[2, 2])
      # Return the expected payoffs for each H and T
      return(z)
    }
    
    LogitBestResponse <- function(x, y) {
      # For the first round of play, choose a random action
      if (x == 1) {
        return(c(ifelse(rbinom(1, 1, .5) == 1, "H", "T"), 0))
      }
      if (x > 1) {
        # For all other rounds of play, calculate the best response for the given player
        # to the empirical distribution of the other player's past play
        hTable <- table(factor(h[, (3 - y)], levels = c("H", "T")))
        freqOfHeads <- as.numeric(hTable[1] / sum(hTable))
        payoffs <- expPayoff(freqOfHeads, y)
        mixedStrategy <-
          exp(λ     * payoffs[1]) / (exp(λ     * payoffs[1]) + exp(λ     * payoffs[2]))
        return(c(
          ifelse(runif(1, 0, 1) < mixedStrategy, "H", "T"),
          mixedStrategy,
          freqOfHeads
        ))
      }
    }
    
    # Determing number of rounds of play in simulation
    Duration <- as.numeric(input$roundsOfPlay)
    
    # Create blank history to record play
    h <-
      data.frame(
        P1 = character(Duration),
        P2 = character(Duration),
        P1predictionOfP2 = numeric(Duration),
        P2predictionOfP1 = numeric(Duration),
        P1beliefOfP2 = numeric(Duration),
        P2beliefOfP1 = numeric(Duration),
        stringsAsFactors = FALSE
      )
    
    ### Simulate game for Logit dynamics
    
    # Initialize progress loader
    withProgress(message = 'Computing:', value = 0, {
      # Set error/smoothing/rationality parameter λ
      λ   <- as.numeric(input$errorParameter)
      
      for (i in 1:Duration) { # Each round of play
        # Derive the actions for each player 1, 2
        # given the history of play thus far
        LBR1 <- LogitBestResponse(i, 1) 
        LBR2 <- LogitBestResponse(i, 2)
        # Record the agent ACTIONS in the history of play
        h[i, 1] <- LBR1[1]
        h[i, 2] <- LBR2[1]
        # Record the agent intended STRATEGIES in the history
        h[i, 3] <- round(as.numeric(LBR1[2]), 4)
        h[i, 4] <- round(as.numeric(LBR2[2]), 4)
        # Record the agent BELIEFS anbout one another's intended strategies in the history of play
        h[i, 5] <- round(as.numeric(LBR2[3]), 4)
        h[i, 6] <- round(as.numeric(LBR1[3]), 4)
        
        # Increment the progress bar, and update the detail text.
        incProgress(1 / Duration, detail = paste("Round", i, sep = " "))
      }
      
    })
    
    # OUTPUT the history of play to be accessed by other reactive contexts
    h[1, c(3:6)] <- 0.5
    return(h)
    
  })
  
  # GRAPH: PREDICTION of OPPONENT STRATEGIES
  output$predictionPlotOutput <- renderPlot({
    # Determing number of rounds of play in simulation
    Duration <- as.numeric(input$roundsOfPlay)
    
    # Import relevant variables
    h <- computeDynamics() # Transition matrix
    
    # plot the predictions
    dfBelief <-
      data.frame(c(1:Duration), c(as.character(rep(1, Duration)), as.character(rep(2, Duration))), c(h[, 5], h[, 6]))
    colnames(dfBelief) <- c("Round", "Player", "Prediction")
    predictionPlot <- ggplot(dfBelief,
                             aes(
                               x = Round,
                               y = Prediction,
                               group = Player,
                               colour = Player
                             )) +
      geom_path(alpha = 1, size = 0.8) +
      ggtitle("Prediction of Opponent Strategy") +
      labs(x = "Round Number", y = "Prediction") +
      ylim(0:1) +
      scale_color_manual(values = c("gray30", "red2"), labels = c("P1", "P2")) +
      theme_few() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 20, unit = "pt"),
          lineheight = 1.15
        ),
        axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        text = element_text(size = 16)
      )
    # Plot the graph
    print(predictionPlot)
  })
  
  
  # GRAPH: AGENT STRATEGIES
  output$strategyPlotOutput <- renderPlot({
    # Determing number of rounds of play in simulation
    Duration <- as.numeric(input$roundsOfPlay)
    # Import relevant variables
    h <- computeDynamics() # Transition matrix
    
    # plot the STRATEGIES
    dfStrategy <-
      data.frame(c(1:Duration), c(as.character(rep(1, Duration)), as.character(rep(2, Duration))), c(h[, 3], h[, 4]))
    colnames(dfStrategy) <- c("Round", "Player", "Strategy")
    strategyPlot <- ggplot(dfStrategy,
                           aes(
                             x = Round,
                             y = Strategy,
                             group = Player,
                             colour = Player
                           )) +
      geom_path(alpha = 1, size = 0.8) +
      ggtitle("Individual Strategy") +
      labs(x = "Round Number", y = "Strategy") +
      ylim(0:1) +
      scale_color_manual(values = c("gray30", "red2"), labels = c("P1", "P2")) +
      theme_few() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(b = 20, unit = "pt"),
          lineheight = 1.15
        ),
        axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        text = element_text(size = 16)
      )
    # Plot the graph
    print(strategyPlot)
  })
  
})

### EOD ###