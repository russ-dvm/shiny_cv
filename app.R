library(shiny)
library(leaflet)
library(markdown)
library(tidyverse)
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
                    leafletOutput("mymap", height = 600) # TODO adjust dimensions of the map
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
                      # TODO: Get the slider working with the different geoms. Currently single-point prizes don't filter properly.
                      # sliderInput("awardDateSlider", label = "Select a range of dates", min = min(awards$date), max = max(awards$date), value = c(min(awards$date),max(awards$date)), timeFormat = "%b %Y"),
                      radioButtons("awardColour", "Colour scheme", choices = list("Magma" = "A", "Inferno" = "B", "Plasma" = "C", "Viridis" = "D")),
                      sliderInput("awardColourSlider", label = "Adjust hue", min = 0, max = 1, value = c(0,1))
                      ),
               column(10, 
                plotOutput("awards_graph", click = "awards_click", hover = "awards_hover"), htmlOutput("awardName"))
               ),
             fluidRow(
               column(10, offset = 2,
                      HTML("</br><p>Above is a figure outlining the various awards, scholarships, and bursarsies I've been fortunate enough to receive since 2012. Points indicate a one-time award, while the bars (tiles) indicate scholarships awards over a number of years. Clicking a point or a bar will bring up more detailed information, including a description and the value of the award/scholarship.</p>
                          <p>The plot is constructed using <a href = \"https://ggplot2.tidyverse.org/\">ggplot2</a>, specifically geom_point and geom_tile. The colour scheme is from the <a href = \"https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html\">Viridis package</a>.</p>
                          <p>The colour schemes included in Viridis are designed to be colour-blind friendly. Have a click through the options to visualize the different schemes. The hue can be adjusted using the slide bar. </p>"))
             )
             ), 
    tabPanel("Work Experience"),
    tabPanel("Network Analysis", 
            fluidRow(
              column(10, offset = 0,
                    visNetworkOutput("visNet", height = 600)),
              column(2, offset = 0,
                     HTML("<br><br>Click and drag to manipulate the diagram.<br><br>Data points are individually clickable and dragable - have fun!<br><br>Scroll to zoom."))
              ),
            fluidRow(column(10, offset = 1,
                HTML("<p>This network analysis provides a view of how CV data points are related to one another, specifically using degrees as anchoring nodes. This gives a general idea of the temporal relationship of the various CV items. Data points are classified broadly as noted in the drop down menu, which can be used to focus or isolate specific categories. More details on each category can be found in one of the more in depth visualizations found on other tabs.</p>
                     <p>The network analysis was constructed using the <a href = \"https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html\">visNetwork</a> package, which is awesome.</p>
                     <p><em>Note: I'm aware that the labels overlap with nodes and other labels, occasionally making them difficult to read. I have not yet found a way to repel the labels to make them more legible. For now, it is relatively easy to simply click a node and drag it, revealing the label.</em></p>")))
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
<<<<<<< HEAD
    ## DATE SLIDER STUFF    
=======

    
>>>>>>> 95903ba36c82c804ed18678f6085550754e27324
    # minDate <- input$awardDateSlider[1]
    # maxDate <- input$awardDateSlider[2]
    # awards2 <- subset(awards, !is.na(awards$date_end))
    # awards2 <- subset(awards, date >= minDate & (date_end <= maxDate | is.na(date_end)))
    # single2 <- subset(single, date >= minDate & date <= maxDate)
    
    ## Graph
    single <- subset(awards, is.na(awards$date_end))
    
    ggplot(awards, aes(fill = Value, x = award_name, y = date + duration/2)) + 
      geom_tile(aes(height = duration, width = 0.9)) +
      geom_point(data = single, aes(x = award_name, y = date, size = 10, colour = Value)) +
      scale_color_viridis(option = input$awardColour, begin = input$awardColourSlider[1], end = input$awardColourSlider[2], limits = c(min(awards$Value, na.rm = T), max(awards$Value, na.rm = T))) +
      scale_fill_viridis(option = input$awardColour, begin = input$awardColourSlider[1], end = input$awardColourSlider[2], limits = c(min(awards$Value, na.rm = T), max(awards$Value, na.rm = T))) +
      guides(size = F) +
      coord_flip() +
      theme_classic() + 
      # theme(legend.position = "none") +
      xlab("") +
      ylab("") +
      scale_y_date(date_breaks = "1 year", date_labels = "%Y")
  })
  
  ## Click information from awards graph
  output$test <- renderPrint({
    nearPoints(m, input$awards_click, yvar = rank, maxpoints = 1, threshold = 10)
  })
  
  output$awardName <- renderText({
    if (is.null(input$awards_click$x)) return("")
    else {
      lvls <- levels(awards$award_name)
      name <- lvls[round(input$awards_click$y)]
      HTML("<b>Award/scholarship</b>:", name, "<br><b>Value</b>: $", awards[awards$award_name == name,2], 
           "<br><b>Description</b>:<em>", as.vector(awards[round(input$awards_click$y),7])
    
    )}
  })
  
  ##Network analysis with visNetwork
  output$visNet <- renderVisNetwork({
    
    ## Build DF
    Net <- read.table("data/network.txt", h = T, stringsAsFactors = F, sep = "\t")
    subNet <- select(Net, edge, group)
    subNet <- subNet[!duplicated(subNet),]
    subNet <- rbind(subNet, c("BSc", "degree"))
    ##Define nodes
    nodes <- data.frame(node = unique(Net$node))
    # nodes$group <- "degree"
    nodes <- rbind(nodes, data.frame(node = unique(Net$edge)))
    nodes <- unique(nodes)
    nodes$id <- c(1:nrow(nodes))
    nodes <- left_join(subNet, nodes, by = c("edge" = "node"))
    nodes <- select(nodes, id, edge, group)
    
    colnames(nodes) <- c("id", "label", "group")
    
    ##Create edgelist
    Net <- select(Net, node, edge)
    NetA <- left_join(Net, nodes, by = c("node" = "label")) %>%  rename(from = id) %>% select(node, edge, from)
    NetB <- left_join(NetA, nodes, by = c("edge" = "label")) %>% rename(to = id)
    NetC <- select(NetB, from, to)
    ##Manually define education nodes - avoids creating duplicate degree nodes
    # NetC <- rbind(NetC, c(4, 2), c(2,3), c(3,1))
    
    ##build the network analysis
    visNetwork(nodes, NetC) %>% 
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
