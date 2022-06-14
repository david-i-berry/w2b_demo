# load libraries
library(shiny)
library(sf)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(dplyr)
library(tidyr)

# set timezone to use
Sys.setenv(TZ='UTC')

# constants
# base URLs for different data sources
OSCARURL <- "https://oscar.wmo.int/surface/#/search/station/stationReportDetails/"  # OSCAR surface
#BASEURL<- 'http://malawi.wis2box.wis.wmo.int/oapi'  # WIS2box, now behind auth. 
BASEURL <- 'http://localhost:8999/oapi'  # local host
# get laucnh time, used to set range on time slider
LAUNCHTIME <- Sys.time()
LAUNCHTIME <- format.POSIXct(LAUNCHTIME,"%Y-%m-%d %H:00:00 UTC")
LAUNCHTIME <- as.POSIXct(LAUNCHTIME)


###################################################################################
# set color palatte to use, 25 colors (RAG, need more color blind friendly scale) #
###################################################################################
pal <- colorRampPalette(c('red','yellow','green'))(5)


# helper functions
# funciton to get list of known stations from box
load_stations <- function(){
  relPath <- '/collections/stations/items?'
  maxRecords <- paste0('limit=100')
  format=''
  filter=''
  the_data <- read_sf(paste0(BASEURL,relPath,maxRecords,format,filter))
  return(the_data)  
}

# function to fetch data from wis2box
load_data <- function(date){
  relPath <- '/collections/data.core.observations-surface-land.mw.FWCL.landFixed/items?'
  maxRecords <- paste0('limit=100000')
  format <- ''
  startDate <- format.POSIXct(date-86400, "%Y-%m-%dT%H:%M:%SZ")
  endDate   <- format.POSIXct(date, "%Y-%m-%dT%H:%M:%SZ")                            
  filter <- paste0("&datetime=",startDate,"/",endDate,"&name=non_coordinate_pressure")
  the_data <- read_sf(paste0(BASEURL,relPath,maxRecords,format,filter))
  if( nrow(the_data) == 0 )the_data <-NULL
  return(the_data)
}

# setup UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "#map {height: calc(100vh - 0px) !important;}"),
  leafletOutput("map"),
  absolutePanel(
    style = "background-color: white; opacity: 0.8",
    bottom = 50, right = "25%", draggable = FALSE, width = "50%",
    div(
    sliderInput("datetime","Select hour", min=as.POSIXct("2022-03-11 12:00 UTC"),
                max = LAUNCHTIME,
                value = LAUNCHTIME,
                step = 3600*3, width="100%",
                ticks=FALSE, animate = animationOptions(interval = 250))
    )
  ),
  absolutePanel(
    style = "background-color: white; opacity: 0.9",
    top = 50, right = "25%", draggable = FALSE, width = "50%",
    div(style="width:100%; text-align: center", htmlOutput("title") )
  )
  
)

# setup back end
server <- function(input, output, session) {
  # get the stations the WIS2box knows about
  stations <- load_stations()
  
  # reactive function to grab data when slider changes
  observations <- reactive({
    the_data <- load_data(input$datetime)
  })
  
  # generate base map
  output$map <- renderLeaflet({
    m <- leaflet() %>% addTiles() %>% addScaleBar(position = "bottomleft") %>%
      setView(lat = -13.8, lng = 34.5, zoom = 7)  %>%
      addLegend("bottomright",colors=pal, labels=c("0-3","4-5","6-7","8-23","24"), title="Count per <br/>24 hour period", opacity = 0.6)
    m
  })
  
  #observe({
    output$title <- renderText(paste0('<h3 text-align="center">Number of observations during<br/>',format.POSIXct(input$datetime-86400,"%Y-%m-%d %H:00:00 UTC")," to ",
                                      format.POSIXct(input$datetime,"%Y-%m-%d %H:00:00 UTC"), "</h3>"))
  #})
  
  # observe and update map as slider changes
  observe({
    the_data <- observations()
    if( !is.null(the_data) ){
      ss <- subset(the_data, name == "non_coordinate_pressure")
      nobs <- table(ss$wigos_station_identifier)
      nobs <- data.frame(wigos_id = names(nobs), count = as.numeric(nobs))
      nobs <- merge(stations,nobs,by="wigos_id", all.x = TRUE)
      nobs$count <- ifelse( is.na(nobs$count), 0, nobs$count)
      nobs <- nobs[order(nobs$count),]
    }else{
      nobs <- stations
      nobs$count <- 0
    }
    # set color of the points to plot
    cind <- cut(nobs$count,c(0,3,5,7,23,47), include.lowest = TRUE,right = TRUE,labels=seq(1,5))
    popups <- paste0(
      'Station: <a href="',OSCARURL,nobs$wigos_id,'">',nobs$wigos_id,'</a><br/>Number of observations: ',nobs$count,'</a>'
    )
    # now update the map
    proxy <- leafletProxy("map") %>% clearShapes()
    proxy <- proxy %>% addCircles(data=nobs, radius = 7.5E3,  # each circle is 7.5km in radius
                                  color='black', opacity = 0.6, 
                                  fillColor = pal[cind], fillOpacity = 0.6, 
                                  weight=0.5, layerId=nobs$wigos_id, 
                                  popup = popups)
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
