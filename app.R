library(shiny)
library(leaflet)
library(markdown)
library(ggplot2)
library(viridis)
# getwd()
# setwd("~/Sync/public_html/shiny_cv/")

##modified from https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
createLink <- function(val) {
  ifelse(is.na(val) == T, sprintf('<a href = "#"></a>'), sprintf('<a href="%s" target="_blank" class="btn btn-primary">Link</a>',val))
}

ui <- fluidPage(
  theme = "bootstrap-flatly.css",
  title = "Shiny CV",
  
  # Adjust for the fixed-top navbar
  tags$style(type="text/css", "body {padding-top: 70px;}"),
  
  ##navbar at top
  navbarPage(
    position = c("fixed-top"),
    
    title = "RUSSELL FRASER", 
    
    tabPanel("About", value = "nav_about", includeMarkdown("docs/test.md")),
    
    tabPanel("Education", includeHTML("docs/test.html")), 
    
    tabPanel("Publications",
             fluidRow(
               
               ##sidebar for publications
               column(2, offset=0,
                      selectInput("pub_type","Select a Publication Type:",c("All"="all", "Journal Articles"="jour","Conferences"="conf", "Other"="text"), selected="all")
               ),
               
               ##main panel for pubs
               column(10,
                  # paste("Select from the options on the left"),
                  conditionalPanel(
                    condition = "input.pub_type == 'all'",
                    paste("Please select a publication type from the drop down menu on the left")
                  ),
                  conditionalPanel(
                    condition = "input.pub_type == 'conf'",
                    leafletOutput("mymap") # TODO adjust dimensions of the map
                  ),
                  conditionalPanel(
                    condition = "input.pub_type == 'jour'",
                    paste("Journal Articles\n\n"),
                    dataTableOutput("journal_table")
                  ),
                  conditionalPanel(
                    condition = "input.pub_type == 'text'",
                    paste("Other")
                  )
               )
             )
          ), 
    tabPanel("Awards", plotOutput("awards_graph", click = "awards_click", hover = "awards_hover"), htmlOutput("test"), verbatimTextOutput("selected_rows")), 
    tabPanel("Work Experience"), 
    tabPanel("PDF", icon = icon("download", "fa-1x")),
    collapsible = T
  )
)

server <- function(input, output, session) {

  # Conference publications map
  output$mymap <- renderLeaflet({
    
      #import data, and do a little cleaning
      pubs <- read.table("./data/pubs.txt", h=T, sep="\t", na.strings="na")
      pubs$meeting_location <- as.character(pubs$meeting_location)
      pubs$lat <- as.numeric(as.character(pubs$lat))
      pubs$long <- as.numeric(as.character(pubs$long))
      
      #make the map
      n <- leaflet(pubs)
      n <- addTiles(n)
      n <- addMarkers(n, lat= ~lat,lng= ~long, popup=paste(sep="</br>", pubs$meeting_name, pubs$year, pubs$meeting_location, paste("Presented by:", pubs$presented_by)), clusterOptions = markerClusterOptions())
      n
  })

  #Journals table
  output$journal_table <- renderDataTable({
    journals <- read.table("data/pubs.txt", h = T, na.strings = "na", sep = "\t")
    journals <- subset(journals, Type == "journal")
    journals <- journals[,c(2,3,6:9,17)]
    journals$link <- createLink(journals$url)
    journals <- journals[,c(1:6,8)]
    journals <- journals[order(journals$year, decreasing = T),]
    colnames(journals) <- c("Authors", "Year", "Title", "Journal", "Volume", "Pages", "Link")
    
    return(journals)
  }, escape = F, options = list(pageLength=10))
  
  # Awards graph
  output$awards_graph <- renderPlot({
    awards <- read.table("./data/awards.txt", h=T, na.strings="na", sep="\t")
    awards$duration <- as.numeric(awards$duration)
    awards$date <- as.Date(awards$date)
    awards$date_end <- as.Date(awards$date_end)
    awards$duration <- awards$date_end - awards$date
    
    single <- subset(awards, is.na(awards$date_end))
    
    awards <- awards[order(awards$date, decreasing = T),]
    awards$rank <- c(1:nrow(awards))
    awards$award_name <- factor(awards$award_name, levels = awards$award_name)
    ggplot(awards, aes(fill = amount, x = award_name, y = date + duration/2)) + 
      geom_tile(aes(height = duration, width = 0.9)) +
      geom_point(data = single, aes(x = award_name, y = date, size = 10, colour = amount)) + 
      scale_color_viridis(discrete = F) + 
      scale_fill_viridis(discrete = F) +
      coord_flip() +
      theme_classic() + 
      theme(legend.position = "none") +
      xlab("") +
      ylab("") +
      scale_y_date(date_breaks = "1 year", date_labels = "%Y")
  })
  
  # Click information from awards graph
  output$test <- renderPrint({
    nearPoints(m, input$awards_click, yvar = rank, maxpoints = 1, threshold = 10)
  })
  
  output$test <- renderText({
    if (is.null(input$awards_click$x)) return("")
    else {
      lvls <- levels(awards$award_name)
      name <- lvls[round(input$awards_click$y)]
      HTML("Here's some more info on", name, "<br>Value: $", awards[awards$award_name == name,2], 
           "<br>Description:", awards[awards$award_name == name,7])
    }
  })
  
  # Print the rows of the data frame which match the x value
  output$selected_rows <- renderPrint({
    if (is.null(input$awards_click$y)) return()
    else {
      keeprows <- round(input$awards_click$y) == as.numeric(awards$award_name)
      awards[keeprows,7]
    }
  })
  
}

shinyApp(ui, server)
