# RATIONALITY AND PREDICTION IN
# NEAR-ZERO-SUM GAMES OF UNCERTAIN INFORMATION
# << UI >>
# by Aydin Mohseni


# Load the shiny GUI library
library(shiny)
library(ggplot2)
library(ggthemes)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Title
  titlePanel("Rationality and Prediction in Near-Zero-Sum Games of Uncertain Information"),
  
  # Load MathJax 
  withMathJax(),
  
  fluidRow(
    style = "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px", 
    column(
      width = 4,
      # Introduction text:
      p(
        tags$b("Premise:"),
        "The Nash equilibrium concept has come to be central to game theory, economics, and other social sciences. A perennial open question is the extent and condition under which we can expect rational agents to play according to the Nash equilibria of games."
      )
    ),
    column(
      width = 4,
      # Introduction text:
      p(
        "In ''The Impossibility of Predicting the Behavior of Rational Agents'' (2001) Foster & Young provide an impossibility theorem demonstrating that, in near-zero-sum of uncertain information, agents cannot be both rational and also accurately predict the strategies of other agents, and so cannot play the Nash equilibria of such games."
        )
      ),
    column(
      width = 4,
      p(
        tags$b("Result:"),
        "In contrast to Foster & Young's result, this model demonstrates how, when rational agents are less-than-perfect in their implementations their plans, they can converge (in the limit) to accurate prediction, and once again learn to play Nash."
      )
    )
    ),
  
  # Sidebar for Parameter Input
  sidebarLayout(
      
      sidebarPanel(

        strong("Logit-Choice Function:"),
        
        withMathJax(),
        p('$$P_{ij}=\\cfrac{ e^{\\lambda \\ EU_{ij}(P_{-i})} }{ \\sum_k e^{\\lambda \\ EU_{ik}(P_{-i})} }$$'),
        
        # Simulate Single Population Button
        tags$head(tags$script(src = "message-handler.js")),
        p(actionButton("simulatePlay", "RUN REPEATED GAME"), align = "center"),
        
        # 2X2 Game Payoff Input Fields
        strong("Zero-Sum Base Game:"),
        
          # 1st Row: Types
          fluidRow(
            column(4),
            column(4,
                   p("H", align="center", style="margin-top:7px")
                   ),
            column(4,
                   p("T", align="center", style="margin-top:7px")
            )),

          # 2nd Row: Payoffs to Type A
          fluidRow(
            column(4,
                   p("H", align="right", style="margin-top:7px;")
                   ),
            column(4,
                  numericInput("a", 
                                label = NULL, 
                                value = 1,
                                min = -100)
            ),
            column(4,
                   numericInput("b", 
                                label = NULL, 
                                value = -1,
                                min = -100)
            )),
        
          # 3rd Row: Payoffs to Type B
          fluidRow(
            column(4,
                   p("T", align="right", style="margin-top:7px;")
                  ),
            column(4,      
                   numericInput("c", 
                                label = NULL, 
                                value = -1,
                                min = -100)
            ), 
            column(4,
                   numericInput("d", 
                                label = NULL, 
                                value = 1,
                                min = -100)
            )),
        
        # Error rate
        withMathJax(),
        sliderInput("errorParameter",
                    "Smoothing \\(\\lambda\\):",
                    min = 0,
                    max = 100,
                    value = 50),
        
        # Payoff Perturbation Size Slider
        sliderInput("payoffPerturbations",
                    "Payoff Perturbations:",
                    min = 0,
                    max = 1,
                    value = 0.2),
        

        # Simulation Time Slider
        sliderInput("roundsOfPlay",
                    "Rounds Of Play:",
                    min = 1,
                    max = 1000,
                    value = 500)
        
      ),
  
  # Main Panel with Stationary Distribution + Simulation & Stats Panels
    mainPanel(
      plotOutput("predictionPlotOutput"),
      plotOutput("strategyPlotOutput")
    )
  )
))