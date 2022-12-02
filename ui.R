## UI for HolyApp

library(shiny)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(ggdendro)


# define frontend
ui <- fluidPage(
  
  tabsetPanel(
    # create tabs  
    # tab (1) food of choice
    tabPanel(
      
      # show user input
      uiOutput("showtext"),
      
      # place plot    
      plotOutput("plot"),
      tags$br(),
      
      # show data ref
      uiOutput("dataref"),
      tags$br(),
      uiOutput("normref")    
      
    ),
    
    # tab (2) heatmap
    tabPanel(
      # slider for scaling data
      sliderInput("heatscaler", "heatscaler", min = 2, max = 100, value = 30),
      
      # dropDown for ordering
      selectInput("heatselector", "Ordering:", 
                  c("",
                    "highest holy Ratio (TRP/PHE + TYR)" = "highHoly",
                    "lowest holy Ratio (TRP/PHE + TYR)" = "lowHoly",
                    "highest Riboflavin (B2) content" = "highB2",
                    "lowest Riboflavin (B2) content" = "lowB2",
                    "highest Pyridoxin (B6) content" = "highB6",
                    "lowest Pyridoxin (B6) content" = "lowB6",
                    "highest folic acid (B9) content" = "highfolic",
                    "lowest folic acid (B9) content" = "lowfolic",
                    "highest Sugar content" = "highsugar",
                    "lowest Sugar content" = "lowsugar"
                  )),
      
      # place heatmap 
      ggiraphOutput("heatmap", width = "80%", height = "80%"),
      tags$br(),
      # show data ref
      uiOutput("dataref2"),
      tags$br(),
      uiOutput("normref2")      
    ),
    tabPanel("Science",
        # show background information
        uiOutput("literature"),
        # place image
        tags$br(),
        imageOutput("image")
    )
  )
)
