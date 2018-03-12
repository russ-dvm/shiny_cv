library(shiny)
library(leaflet)
library(markdown)
library(ggplot2)
library(viridis)
library(visNetwork)

##modified from https://stackoverflow.com/questions/28117556/clickable-links-in-shiny-datatable
createLink <- function(val) {
  ifelse(is.na(val) == T, sprintf('<a href = "#"></a>'), sprintf('<a href="%s" target="_blank" class="btn btn-primary">Link</a>',val))
}

## Create Awards DF.
awards <- read.table("data/awards.txt", h=T, na.strings="na", sep="\t", stringsAsFactors = F)
awards$duration <- as.numeric(awards$duration)
awards$date <- as.Date(awards$date)
awards$date_end <- as.Date(awards$date_end)
awards$duration <- awards$date_end - awards$date

## Reorder by date, and add ranking for clicking later.
awards <- awards[order(awards$date, decreasing = T),]
awards$rank <- c(1:nrow(awards))
awards$award_name <- factor(awards$award_name, levels = awards$award_name)


ui <- fluidPage(
  theme = "bootstrap-flatly.css",
  title = "Shiny CV",
  
  # Adjust for the fixed-top navbar
  tags$style(type="text/css", "body {padding-top: 70px;}"),
  
  ##navbar at top
  navbarPage(
    position = c("fixed-top"),
    
    title = "RUSSELL FRASER", 
    
    tabPanel("About", value = "nav_about", includeHTML("docs/index.html")),
    
    tabPanel("Education", includeHTML("docs/test.html")), 
    
    tabPanel("Publications",
             fluidRow(
               
               ##sidebar for publications
               column(2, offset=0,
                      selectInput("pub_type","Select a Publication Type:",c("Select"="all", "Journal Articles"="jour","Conferences"="conf", "Other"="text"), selected="all")
               ),
               
               ##main panel for pubs
               column(10,
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
    tabPanel("Awards", 
             fluidRow(
               column(2, offset = 0, 
                      sliderInput("awardDateSlider", label = "Select a range of dates", min = min(awards$date), max = max(awards$date), value = c(min(awards$date),max(awards$date)), timeFormat = "%b %Y")),
               column(10, 
                plotOutput("awards_graph", click = "awards_click", hover = "awards_hover"), htmlOutput("awardName"))
               )
             ), 
    tabPanel("Work Experience"),
    tabPanel("Network Analysis", 
             fluidRow(
              column(2, offset = 0)),
              column(10,
                    visNetworkOutput("visNet"))
             ),

    tabPanel("PDF", icon = icon("download", "fa-1x"), uiOutput('pdfviewer')),
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
      n <- addMarkers(n, lat= ~lat,lng= ~long, popup=paste(sep="</br>", paste("\"", pubs$title, "\"", sep = ""), pubs$meeting_name, pubs$year, pubs$meeting_location, paste("Presented by:", pubs$presented_by)), clusterOptions = markerClusterOptions())
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
    
    minDate <- input$awardDateSlider[1]
    maxDate <- input$awardDateSlider[2]
    
    awards2 <- subset(awards, date >= minDate & (date_end <= maxDate | is.na(date_end)))
    
    single <- subset(awards, is.na(awards$date_end))
    single2 <- subset(single, date >= minDate & date <= maxDate)
    
    ggplot(awards2, aes(fill = rank, x = award_name, y = date + duration/2)) + 
      geom_tile(aes(height = duration, width = 0.9)) +
      geom_point(data = single2, aes(x = award_name, y = date, size = 10, colour = rank)) +
      scale_color_viridis() +
      scale_fill_viridis() +
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
  
  output$awardName <- renderText({
    if (is.null(input$awards_click$x)) return("")
    else {
      lvls <- levels(awards$award_name)
      name <- lvls[round(input$awards_click$y)]
      HTML("Here's some more info on", name, "<br>Value: $", awards[awards$award_name == name,2], 
           "<br>Description:", as.vector(awards[round(input$awards_click$y),7])
    
    )}
  })
  
  ##Network analysis with visNetwork
  output$visNet <- renderVisNetwork({
    
    ## Build DF
    Net <- read.table("data/network.txt", h = T, stringsAsFactors = F, sep = "\t")
    
    ##Define nodes
    nodes <- data.frame(node = unique(Net$node))
    nodes$group <- "degree"
    nodes <- rbind(nodes, data.frame(node = unique(Net$edge), group = Net$group))
    nodes$id <- c(1:nrow(nodes))
    nodes <- select(nodes, id, node, group)
    colnames(nodes) <- c("id", "label", "group")
    
    ##Create edgelist
    Net <- select(Net, node, edge)
    NetA <- left_join(Net, nodes, by = c("node" = "label")) %>%  rename(from = id) %>% select(node, edge, from)
    NetB <- left_join(NetA, nodes, by = c("edge" = "label")) %>% rename(to = id)
    NetC <- select(NetB, from, to)
    ##Manually define education nodes - avoids creating duplicate degree nodes
    NetC <- rbind(NetC, c(4, 2), c(2,3), c(3,1))
    
    ##build the network analysis
    visNetwork(nodes, NetC, height = "800px") %>% 
      visGroups(groupname = "award", shape = "icon", icon = list(code = "f091", color = "green")) %>% 
      visGroups(groupname = "teaching", shape = "icon", icon = list(code = "f086", color = "purple")) %>% 
      visGroups(groupname = "work", shape = "icon", icon = list(code = "f0f1", color = "red")) %>% 
      visGroups(groupname = "publication", shape = "icon", icon = list(code = "f15c", color = "orange")) %>% 
      visGroups(groupname = "degree", shape = "icon", icon = list(code = "f19d")) %>% 
      addFontAwesome() %>% 
      visOptions(selectedBy = list(variable = "group"))
    
  })
  
  ##CV PDF
  output$pdfviewer <- renderUI({
    tags$iframe(style="height:600px; width:100%", src="RFraser_current.pdf")
  })
}

shinyApp(ui, server)
