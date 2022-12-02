## HolyFood Server

library(shiny)
library(reshape2)
library(ggplot2)
library(dplyr)
library(ggiraph)
library(ggdendro)


accept <- function(x){
  gsub("'", "&#39;", x )
}

# define backend
server <- function(input, output){
  # define palette
  palette <- colorRampPalette(c("white", "yellow", "red"))(n = 299)
  food <- read.table("food_table_normalized_2022-09-19.csv", sep = ";")[,1:9]
  # rename column-names
  colnames(food)<- c( "food",
                      "Folate, total",
                      "Phenylalanine",
                      "Riboflavin (B2)",
                      "sugars",                      
                      "Tryptophan",
                      "Tyrosine",
                      "Pyridoxin (B6)",
                      "TRP/PHE + TYR"
  )
  heatedfood <- food
  #heatedfood <- heatedfood[rowSums(is.na(heatedfood)) == 0, ] 
  
  # render plot of choosen food
  output$plot = renderPlot({
    choosefood = head(food[grep(input$choosefood, food$food, perl = TRUE, ignore.case = TRUE),], n = 1)
    meltchoosefood <- reshape2::melt(choosefood, id = c("food"), na.rm = TRUE)
    # change column names
    colnames(meltchoosefood)[2] <- c("nutrition")
    colnames(meltchoosefood)[3] <- c("amount")
    
    ggplot(meltchoosefood, aes(nutrition, amount)) + 
      geom_bar(stat="identity", fill="#CC3300") +
      # gray26
      ggtitle(choosefood[,1]) +
      theme_minimal() +
      theme(
        legend.position = "right",
        panel.grid.minor = element_line(color = "transparent"),
        panel.grid.major = element_line(color = "transparent"),
        axis.ticks.length   = unit(2, units = "mm"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title = element_text(size = 9, colour = "gray30"),
        axis.text.y = element_text(hjust = 1, size = 5, colour = "gray40"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8, colour = "gray40"))
  })
  
  # show input from user of foood of choice
  # define output element
  output$showtext <- renderUI({
    tagList(
      textInput("choosefood",
                "choose your food",
                ""), 
      renderText({
        paste0("selected food: ", input$choosefood)
      })
    )
  })
  
  # link of datasources
  url <- a(" U.S. DEPARTMENT OF AGRICULTURE", href = "https://fdc.nal.usda.gov/download-datasets.html")
  urlnorm <- a("NIH National Institutes of Health", href = "https://ods.od.nih.gov/factsheets")
  urlamino <- a("Recommended Dietary Allowances: 10th Edition. National Research Council (US) Subcommittee on the Tenth Edition of the Recommended Dietary Allowances.
Washington (DC): National Academies Press (US); 1989.", href = "https://www.ncbi.nlm.nih.gov/books/NBK234922/table/ttt00008/?report=objectonly")
  urlsugar <- a("World Health Organization", href = "https://www.who.int/news-room/detail/04-03-2015-who-calls-on-countries-to-reduce-sugars-intake-among-adults-and-children")
  output$normref <- renderUI({
    tagList(renderText({"Data Sources for normalization (daily dose):"}),
    tagList("Daily doses of Vitamins:", urlnorm),    
    tags$br(),
    tagList("Daily doses of amino acids:", urlamino),
    tags$br(),
    tagList("Max daily sugar intake:", urlsugar)
    )
  })
  output$normref2 <- renderUI({
    tagList(renderText({"Data Sources for normalization (daily dose):"}),
            tagList("Daily doses of Vitamins:", urlnorm),    
            tags$br(),
            tagList("Daily doses of amino acids:", urlamino),
            tags$br(),
            tagList("Max daily sugar intake:", urlsugar)
    )
  })
  output$dataref <- renderUI({
    tagList("Source of Data:", url)
  })
  output$dataref2 <- renderUI({
    tagList("Source of Data:", url)
  })
  
  # link to paper
  paper1 <- a("'Aztec Cannibalism and Maize Consumption: The Serotonin Deficiency Link' September 2002; The Mankind Quarterly XLIII(1):3DOI:10.46469/mq.2002.43.1.1", href="https://www.researchgate.net/publication/349574563_Aztec_Cannibalism_and_Maize_Consumption_The_Serotonin_Deficiency_Link")
  paper2 <- a("'Serotonin availability in rat colon is reduced during a Western diet model of obesity' 
              R. L. Bertrand*, S. Senadheera*, A. Tanoto, K. L. Tan, L. Howitt, H. Chen, T. V. Murphy, S. L. Sandow, L. Liu , and P. P. Bertrand;
              01 Aug 2012", href="https://doi.org/10.1152/ajpgi.00048.2012")
  paper3 <- a("'The dopaminergic system and aggression in laying hens' R L Dennis 1, H W Cheng Affiliations expand PMID: 22010227 DOI: 10.3382/ps.2011-01513", href="https://pubmed.ncbi.nlm.nih.gov/22010227/")
  output$literature <- renderUI({
    tagList(  renderText({"Background Information: "}),
      tagList("", paper1),
      tags$br(),
      tagList("", paper2),
      tags$br(),
      tagList("", paper3)
    )
  })  
  
  
  heatdata = reactive({
    if(input$heatselector == "highHoly"){
      # ordering holy Ratio
      heatedfood <- heatedfood[order(-heatedfood[,9]),]
    }
    # lowest holy Ratio
    if(input$heatselector == "lowHoly"){
      # ordering holy Ratio
      heatedfood <- heatedfood[order(heatedfood[,9]),] 
    }
    
    # highest Riboflavin (B2)
    if(input$heatselector == "highB2"){
      # ordering Riboflavin (B2)
      heatedfood <- heatedfood[order(-heatedfood[,4]),] 
    }
    # lowest Riboflavin (B2)
    if(input$heatselector == "lowB2"){
      # ordering Riboflavin (B2)
      heatedfood <- heatedfood[order(heatedfood[,4]),] 
    }
    
    # highest Pyridoxin (B6)
    if(input$heatselector == "highB6"){
      # ordering Pyridoxin (B6)
      heatedfood <- heatedfood[order(-heatedfood[,8]),]  
      #row.names(Food) <- Food[,1]
    }
    
    # lowest Pyridoxin (B6)
    if(input$heatselector == "lowB6"){
      # ordering Pyridoxin (B6)
      heatedfood <- heatedfood[order(heatedfood[,8]),] 
    }
    
    # highest folic Acid
    if(input$heatselector == "highfolic"){
      # odering folic acid
      heatedfood <- heatedfood[order(-heatedfood[,2]),] 
    }
    # lowest folic Acid
    if(input$heatselector == "lowfolic"){
      # odering folic acid
      heatedfood <- heatedfood[order(heatedfood[,2]),] 
    }

    # highest Sugar content" = "highsugar
    if(input$heatselector == "highsugar"){
      # odering folic acid
      heatedfood <- heatedfood[order(-heatedfood[,5]),] 
    }
    
    #lowest Sugar content" = "lowsugar"
    if(input$heatselector == "lowsugar"){
      # odering folic acid
      heatedfood <- heatedfood[order(heatedfood[,5]),] 
    }
    
        
    # if (input$heatselector == ""){
    # 
    # }
    
    heatfood <- reshape2::melt(head(heatedfood, n = input$heatscaler, id = c("food"), na.rm = TRUE))
    heatfood[is.na(heatfood)] <- 0

    # change column names
    colnames(heatfood)[2] <- c("nutrition")
    colnames(heatfood)[3] <- c("amount")
    
    # add tooltip
    food_exp <- heatfood %>% 
      mutate( 
        tooltip = sprintf("food: %s<br/>nutrition: %s<br/>amount: %.02f", 
                          food, nutrition, amount) ,
        data_id = sprintf("%s_%s", food, nutrition)
      )
    
    # shorten food names
    food_exp$food <- substr(food_exp$food , start = 1, stop = 50)   
    food_exp   
  })

  # render heatmap
  output$heatmap = renderggiraph({
    p <- ggplot(heatdata(), aes(nutrition, food)) +
      geom_tile_interactive(aes(fill = amount, tooltip = accept(tooltip), data_id = accept(data_id)), colour = "white") +
      scale_fill_gradient(low = "white", high = "#BC120A") +
      geom_segment(
        #data = data_c,
        mapping = aes(x = 1, y = 1, xend = 1, yend = 1),
        colour = "gray20", size = .2) +
      geom_segment(
        #data = data_r,
        mapping = aes(x = 1, y = 1, xend = 1, yend = 1),
        colour = "gray20", size = .2) #+
      #coord_equal()
    
    # cosmetics
    p <- p + theme_minimal() +
      theme(
        legend.position = "right",
        panel.grid.minor = element_line(color = "transparent"),
        panel.grid.major = element_line(color = "transparent"),
        axis.ticks.length = unit(2, units = "mm"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
        axis.title = element_text(size = 9, colour = "gray30"),
        axis.text.y = element_text(hjust = 1, size = 5, colour = "gray40"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5, colour = "gray40"),
        legend.title=element_text(face = "bold", hjust = 0.5, size=8),
        legend.text=element_text(size=6)
      )

    ggiraph(ggobj = p)
    
  })
  
  output$image <- renderImage({
    # load PNG
    # image <- readPNG("/home/clara/Projekte/R/shiny_treasure/HolyMe/HolyFoodHeatmap_app/feelinggraph.png")
    filename <- "feelinggraph.png"
    
    # Return a list containing the filename
    list(src = filename, 
         width = "90%",
         heigth = "90%",
         align = "text-align",
         alt = "Serotonin Biosynthesis")
  }, deleteFile = FALSE)
}
