# Prediction & Rationality
# Response to Foster & Young (2000)
# 2x2 Zero Sum Game

# Load packages from library
library(ggplot2)
library(ggthemes)

# Set benchmark game payoffs
a = b = c = d = 1

# for the 2x2 zero sum game
#         H       T
#   H | a, -a | b, -b |
#   T | c, -c | d, -d |

# Assign benchmark game payoffs for both players
p1Payoff0 <- data.frame(c(a,-b), c(-c, d))
rownames(p1Payoff0) <- c("H", "T")
colnames(p1Payoff0) <- c("H", "T")
p2Payoff0 <- data.frame(c(-a, b), c(c,-d))
rownames(p2Payoff0) <- c("H", "T")
colnames(p2Payoff0) <- c("H", "T")

# v-Perturbation of payoffs
vPertub <- function() {
  z <- rbeta(1, 1, 1) - (1 / 2)
  return(z)
}

# Perturb payoffs
p1Payoff0 <- p1Payoff0 + .1 * vPertub()
p2Payoff0 <- p2Payoff0 + .1 * vPertub()

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

# # Learning process: Empirical best response
# BestResponse <- function(x, y) {
#   # For the first round of play, choose a random action
#   if (x == 1) {
#     z <- c(ifelse(rbinom(1, 1, .5) == 1, "H", "T"), 0)
#     return(z)
#   }
#   if (x > 1) {
#     # For all other rounds of play, calculate the best response for the given player
#     # to the empirical distribution of the other player's past play
#     hTable <- table(factor(h[, (3 - y)], levels = c("H", "T")))
#     freqOfHeads <- as.numeric(hTable[1] / sum(hTable))
#     payoffs <- expPayoff(freqOfHeads, y)
#     if (payoffs[1] > payoffs[2]) {
#       # If the best response is heads, play heads
#       return(c("H", freqOfHeads))
#     } else {
#       if (payoffs[1] < payoffs[2]) {
#         # If the best response is tails, play tails
#         return(c("T", freqOfHeads))
#       } else {
#         # Otherwise, randomly choose a strategy
#         return(c(ifelse(rbinom(1, 1, .5) == 1, "H", "T"), freqOfHeads))
#       }
#     }
#   }
# }

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
      exp(λ  * payoffs[1]) / (exp(λ  * payoffs[1]) + exp(λ  * payoffs[2]))
    return(c(
      ifelse(runif(1, 0, 1) < mixedStrategy, "H", "T"),
      mixedStrategy,
      freqOfHeads
    ))
  }
}

# Determing number of rounds of play in simulation
T <- 1000
# Create blank history to record play
h <-
  data.frame(
    P1 = character(T),
    P2 = character(T),
    P1predictionOfP2 = numeric(T),
    P2predictionOfP1 = numeric(T),
    P1beliefOfP2 = numeric(T),
    P2beliefOfP1 = numeric(T),
    stringsAsFactors = FALSE
  )

### Simulate game for BR dynamics
# for (i in 1:T) {
#   h[i, 1] <- BestResponse(i, 1)[1]
#   h[i, 2] <- BestResponse(i, 2)[1]
#   h[i, 3] <- round(as.numeric(BestResponse(i, 1)[2]), 4)
#   h[i, 4] <- round(as.numeric(BestResponse(i, 2)[2]), 4)
# }

### Simulate game for Logit dynamics
λ  <- 1 # Set rationality parameter λ
for (i in 1:T) {
  LBR1 <- LogitBestResponse(i, 1)
  LBR2 <- LogitBestResponse(i, 2)
  h[i, 1] <- LBR1[1]
  h[i, 2] <- LBR2[1]
  h[i, 3] <- round(as.numeric(LBR1[2]), 4)
  h[i, 4] <- round(as.numeric(LBR2[2]), 4)
  h[i, 5] <- round(as.numeric(LBR2[3]), 4)
  h[i, 6] <- round(as.numeric(LBR1[3]), 4)
}

# Output history of play
h[1, c(3:6)] <- 0.5
print(h)


dfStrategy <-
  data.frame(c(1:T), c(as.character(rep(1, T)), as.character(rep(2, T))), c(h[, 3], h[, 4]))
colnames(dfStrategy) <- c("Round", "Player", "Strategy")

ggplot(dfStrategy,
       aes(
         x = Round,
         y = Strategy,
         group = Player,
         colour = Player
       )) +
  geom_path(alpha = 1, size = 0.8) +
  ggtitle("Individual (Logit Choice) Strategy") +
  labs(x = "Round Number", y = "Strategy") +
  ylim(0:1) +
  scale_color_manual(values = c("gray30", "red2")) +
  theme_few()

dfBelief <-
  data.frame(c(1:T), c(as.character(rep(1, T)), as.character(rep(2, T))), c(h[, 5], h[, 6]))
colnames(dfBelief) <- c("Round", "Player", "Prediction")

ggplot(dfBelief,
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
  scale_color_manual(values = c("gray30", "red2")) +
  theme_few()
