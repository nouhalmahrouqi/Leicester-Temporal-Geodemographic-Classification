#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(rvest)
#library(geojsonio)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(htmltools)
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")

#setwd("/home/nmkam1/Dissertation/Data/Shiny/LTOAC")

clusters_all <-
  sf::read_sf("clusters_all_10.shp") %>%
  st_transform(4326)


# Define UI for application that draws a histogram

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Temporal Geodemgraphic Classification for Leicester</a>'), id="nav",
             windowTitle = "Temporal Classification",
             
             tabPanel("Classification Maps",
                      div(class="outer",
                          tags$head(includeCSS("styles.css")),
                          leafletOutput("mymap", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        span(tags$i(h6("Geodemographic change in Leicester, 1991-2011.")), style="color:#045a8d"),
                                        
                                        radioButtons("radio", h3("Select the year"),
                                                     choices = list("1991" = "1991", "2001" = "2001", "2011" = "2011"),
                                                     selected = "1991")
                                        
                          )
                          
                          
                      )
             ),
             absolutePanel(id = "logo", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                           tags$a(href='https://le.ac.uk/', tags$img(src='leicester logo.png',height='40',width='80')))
             )
)




# Define server
server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lat = 52.63, lng = -1.1, zoom = 12.45) %>%
      addMiniMap(
        tiles = providers$Esri.WorldImagery,position = 'bottomleft',minimized = T,
        toggleDisplay = T)
  })
  
  
 
  #pal <- colorFactor(c('#878787', '#f781bf', '#6a3d9a','#fb9a99','#ff7f00', '#a6cee3','#cab2d6','#b2df8a','#e31a1c','#33a02c','#1f78b4','#ffff99'), domain = clusters_all$class)
  pal <- colorFactor(c('#33a02c', '#b2df8a', '#6a3d9a','#cab2d6','#f781bf', '#1f78b4','#a6cee3','#ff7f00','#e31a1c','#fb9a99','#878787'), domain = clusters_all$class)

  labels<- paste(
    "Cluster: ", clusters_all$class,"<br/>", 
    "Code: ", clusters_all$code, 
    sep="") %>%
    lapply(htmltools::HTML)
  
  
  observe({
    if (input$radio %in% '1991'){
      
      filteredData <- reactive({
        clusters_all %>% filter(year == input$radio)
      })
      
      filtered1991 <- clusters_all %>% filter(year == input$radio)
      
      labels<- paste(
        "Cluster: ", filtered1991$class,"<br/>", 
        "Code: ", clusters_all$code, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      
      leafletProxy("mymap", data = clusters_all %>% filter(year == input$radio)) %>%
        clearShapes() %>%
        clearControls()%>% 
        addPolygons(
          fillColor = ~pal(class),
          fillOpacity = .40,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.00001,
          smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
        addLegend(pal=pal, values=~class,
                  opacity=0.9, title = "Geodemographic clusters", position = "bottomright" )
      
    } else if (input$radio %in% '2001') {
      filteredData <- reactive({
        clusters_all %>% filter(year == input$radio)
      })
      
      filtered2001 <- clusters_all %>% filter(year == input$radio)
      
      labels<- paste(
        "Cluster: ", filtered2001$class,"<br/>", 
        "Code: ", clusters_all$code, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      leafletProxy("mymap", data = clusters_all %>% filter(year == input$radio)) %>%
        clearShapes() %>%
        clearControls()%>%
        addPolygons(
          fillColor = ~pal(class),
          fillOpacity = .40,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.000000001,
          smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
        addLegend(pal=pal, values=~class,
                  opacity=0.9, title = "Geodemographic clusters", position = "bottomright" )
      
    } else {
      filteredData <- reactive({
        clusters_all %>% filter(year == input$radio)
      })
      
      filtered2011 <- clusters_all %>% filter(year == input$radio)
      
      labels<- paste(
        "Cluster: ", filtered2011$class,"<br/>", 
        "Code: ", clusters_all$code, 
        sep="") %>%
        lapply(htmltools::HTML)
      
      leafletProxy("mymap", data = filteredData()) %>%
        clearShapes() %>%
        clearControls()%>%
        addPolygons(
          fillColor = ~pal(class),
          fillOpacity = .40,
          opacity = 0.90,
          color = "black",stroke = T,weight = 0.00000001,
          smoothFactor = 0.5,
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")
        ) %>%
        addLegend(pal=pal, values=~class,
                  opacity=0.9, title = "Geodemographic clusters", position = "bottomright" )
    }
    
  })
  
}





# Run the application 
shinyApp(ui = ui, server = server)

