#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Schelling Model"),
  # Sidebar with a slider input for number of bins 
  # sidebarLayout(
     # sidebarPanel(<br>),
      fluidRow(align = "center",
        column(3, 
         sliderInput("Nr",
                     "Population Red",
                     min = 0,
                     max = 1000,
                     value = 100)
         ),
        column(3,
         sliderInput("Ng",
                     "Population Green",
                     min = 0,
                     max = 1000,
                     value = 100)
         ),
        column(3,
         sliderInput("Nb",
                     "Population Blue",
                     min = 0,
                     max = 1000,
                     value = 100)
         )
        ),
      
      fluidRow( align = "center",
        column(3,
         sliderInput("m.r",
                     "Nearest Neighbors by % of Total Pop: Red",
                     min = 0,
                     max = 100,
                     value = 8)),
        column(3,
         sliderInput("m.g",
                     "Nearest Neighbors by % of Total Pop: Green",
                     min = 0,
                     max = 100,
                     value = 8)),
        column(3,
         sliderInput("m.b",
                     "Nearest Neighbors by % of Total Pop: Blue",
                     min = 0,
                     max = 100,
                     value = 8))
        ),
      
      fluidRow( align = "center",
        column(3,
         sliderInput("j.r",
                     "Min. # of Like Neighbors by % of Nearest Neighbors: Red",
                     min = 0,
                     max = 100,
                     value = 50)),
        column(3,
         sliderInput("j.g",
                     "Min. # of Like Neighbors by % of Nearest Neighbors: Green",
                     min = 0,
                     max = 100,
                     value = 50)),
        column(3,
         sliderInput("j.b",
                     "Min. # of Like Neighbors by % of Nearest Neighbors: Blue",
                     min = 0,
                     max = 100,
                     value = 50)),
        column(3,
         checkboxInput("interm",
                       "Show Intermediate Plots",
                       value = FALSE))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      align = "center",
        # uiOutput("test")
       plotOutput("schelling"),
       tableOutput("metrics")
    )
  # )
))
