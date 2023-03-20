#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(gganimate)
library(dplyr)
library(plotly)
library(stringr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    
    sidebarPanel(
      fileInput("file", label = h3("File input")),
      uiOutput("postload")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gif",imageOutput("gifplot"))
      )
    )
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  theinput <- reactive({
    req(input$file)
    read.table(input$file$datapath)
    # readLines("../input.txt")
  })
  
  part_1=reactive({
    
    req(theinput())
    
    acc=0
    
    # Compute Total # of parenthesis 
    total <- nchar(as.character(theinput()$V1))
    
    # Compute Total # of Open parenthesis 
    count_o <- stringr::str_count(as.character(theinput()$V1), fixed('('))
    
    # Compute Total # of Close parenthesis
    count_c <- stringr::str_count(as.character(theinput()$V1), fixed(')'))
    
    # Subtract to get the answer 
    ans_1 <- as.numeric(count_o - count_c)
    
    ans_1
    
  })
  
  
  part_2=reactive({
    
    req(theinput())
    
    # Split into Vector
    split_x <- unlist(strsplit(as.character(theinput()$V1), split=character(0)))
    
    # Convert to Numeric Values [Up Floor = 1, Down Floor = -1]
    split_num <- ifelse(split_x == "(", 1, 
                        ifelse(split_x == ")", -1, NA))
    
    # Find first time the value went to -1
    ans_2 = which(cumsum(split_num) == -1)[1]
  
    ans_2
    
  })
  
  output$part1=renderText({
    req(part_1())
    paste0("What Floor The instructions take Santa: ", part_1())
    
  })
  output$part2=renderText({
    req(part_1())
    paste0("What is the position of the character that causes Santa to first enter the basement?: ", part_2())
  })
  
  output$gifplot=renderImage({
    
    req(theinput())
    
    # Split into Vector
    split_x <- unlist(strsplit(as.character(theinput()$V1), split=character(0)))
    
    # Convert to Numeric Values [Up Floor = 1, Down Floor = -1]
    split_num <- ifelse(split_x == "(", 1, 
                        ifelse(split_x == ")", -1, NA))
    
    split_num <- data.frame(split_num)
    colnames(split_num) <- "parenthesis"
    
    split_num <- split_num %>%
      mutate(floor_pos = cumsum(parenthesis),
             present_num = row_number())
    
    g <- 
      ggplot(split_num, aes(x = present_num, y = floor_pos)) +
      geom_line(color = "blue", 
                show.legend = FALSE) +
      geom_text(aes(label = paste0("Floor: ", floor_pos)),
                vjust = -1) +
      geom_hline(yintercept = -1,
                 color = "red",
                 linetype = "dashed") +
      geom_text(aes(0, 0,
                    label = paste0("Santa first enter the basement: ", which(floor_pos == -1)[1]), 
                    vjust = 2,
                    hjust = -.05)) +
      labs(title = "AoC 2015 Day 1 Puzzle",
           y = "Floor in the Apartment",
           x = "Number of Present") +
      transition_reveal(present_num) 
    
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext='.gif')
    
    anim_save("outfile.gif", animate(g)) # New
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )

  }, deleteFile = TRUE)
  
  output$postload=renderUI({
    req(theinput(),input$file)
    div(
      p("Part 1:", textOutput(outputId = "part1", inline=T)),
      p("Part 2:", textOutput(outputId = "part2", inline=T))
    )
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)