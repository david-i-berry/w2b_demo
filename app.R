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
pal <- colorRampPalette(c('red','yellow','green'))(25)

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
    top = 50, right = "25%", draggable = FALSE, width = "50%",
    sliderInput("datetime","Select hour", min=as.POSIXct("2022-03-11 12:00"),
                max = LAUNCHTIME,
                value = LAUNCHTIME,
                step = 3600*3, width="100%",
                ticks=FALSE, animate = animationOptions(interval = 250))
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
      setView(lat = -13.8, lng = 34.5, zoom = 7) 
    m
  })
  
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
    cind <- nobs$count + 1
    # now update the map
    proxy <- leafletProxy("map") %>% clearShapes()
    proxy <- proxy %>% addCircles(data=nobs, radius = 7.5E3,  # each circle is 7.5km in radius
                                  color='black', opacity = 0.6, 
                                  fillColor = pal[cind], fillOpacity = 0.6, 
                                  weight=0.5, layerId=nobs$wigos_id, 
                                  popup = paste0('<h4><a href="',OSCARURL,nobs$wigos_id,'">',nobs$wigos_id,': ',nobs$count,'</a></h4>'))
  })

}


# Run the application 
shinyApp(ui = ui, server = server)
