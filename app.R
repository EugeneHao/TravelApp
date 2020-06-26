library(shiny);library(shinycssloaders);library(shinydashboard);library(shinyWidgets)
library(plotly)
library(leaflet);library(leaflet.extras)
library(dplyr);library(sp);library(rgdal)
require(RCurl)



setwd("~/GitHub/TravelApp/")
source("BaseFun.R")
TripInfor <- readRDS("TripInfor.rds")
RouteLine <- readRDS("RouteLine.rds")
NP <- read.csv("nps_parks.csv")
plot_path <- "~\\GitHub\\TravelApp\\www\\photo"
plot_path2 <-  "C:/Users/eugen/Dropbox/My/My App/TravelApp/www/photo"




ui <- dashboardPage(skin = "blue",
 dashboardHeader(title = "My Apps", titleWidth = 550, disable = FALSE),
 
 dashboardSidebar(
   tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
   sidebarMenu(
     # Setting id makes input$tabs give the tabName of currently-selected tab
     id = "tabs",
     menuItem(text = "Travel Histroy", icon = icon("th"), tabName = "widgets", # badgeLabel = "new",
              badgeColor = "green")
      )
 ),
 
 dashboardBody(
     tabItems(

       
       tabItem(tabName = "widgets",
               fluidRow(
                 textInput("dep_ads", "Departure Address: ", "644 Squaw Creek Drive, Ames"), 
                 textInput("dest_ads", "Destination Address: ", "Yellow Stone"), 
                 
                 box(width = 15,
                     leafletOutput("TravelMap", width = "100%", height = 450),
                     absolutePanel(top = 20, left = 50, 
                                   column(width = 2, actionButton("draw_route", "GetRoute"))
                                   )
                     ),
                 hr(),
                 box(title = "Gallery", status = "success", solidHeader = TRUE, width = 15, 
                     fluidRow(width = 10,
                              uiOutput("plot_gallery",width = "100%")
                     ))
                 
               )
       )
     )
 )
)
  

##############################################################################

server <- function(input, output, session)
{

  # the map of states, 
  output$TravelMap <- renderLeaflet({
    DrawRoute(NULL, NULL, TripInfor) -> p
    p <- p %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",                               #icon: start with "fa-"
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")
        )) %>%
      #JS: Mark character strings as literal JavaScript code
      addSearchOSM() %>%                                                     #search using OSM Geocoder
      setView(-93.65, 42.0285, zoom = 12) %>%
      addDrawToolbar(
        polylineOptions = FALSE,
        polygonOptions = F,
        rectangleOptions = F,
        circleOptions = F,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        # markerOptions = drawMarkerOptions(markerIcon = myMarkerIcon(2)),
        singleFeature = TRUE,
        editOptions = editToolbarOptions()
      )
    p
  })

  selectmarker <- reactiveValues(name = "Key_West")
 
  # get the selected marker's name
  observeEvent(input$TravelMap_marker_click, { # update the location selectInput on map clicks
    point <- input$TravelMap_marker_click
    sp <- data.frame(name = point$id %>% as.numeric())
    if(inner_join(sp, TripInfor) %>% dim() %>% "["(1) > 0)
    {
      selectmarker$name <- sp$name
    }
  })
  
  
  # plot my travel figure
  output$plot_gallery <- renderUI(
    {
    loc <- selectmarker$name 
    loc_path <- paste("https://raw.githubusercontent.com/EugeneHao/TravelApp/master/www/photo", 
                        loc, "showplot.JPG", sep = "/")
    checkurl <- RCurl::url.exists(loc_path)
    if(checkurl == TRUE)
    {
        img(src = loc_path, width = 900, height = 500)
    }
    else
      {
        warning("No Picture Founded")
      }
  })
  
# show the route from input departure to input destination
  route_record <- reactiveValues(lng = list(), 
                                 lat = list(), 
                                 hours = list(), 
                                 miles = list())
  
  
  observeEvent(input$draw_route, {
    dep_ads <- input$dep_ads
    dest_ads <- input$dest_ads
    # if(exists("dep_ads") == 0)
    #   dep_ads = "644 Squaw Creek Drive, Ames"
    # if(exists("dest_ads") == 0)
    #   dest_ads <- "Yellow Stone"
    route1 <- route(dep_ads, dest_ads, mode = "driving", structure = "route")
    route_name <- names(route1)
    route_name[8] <- "lng"
    names(route1) <- route_name
    
    draw_route = input$draw_route
    route_record$lng[[draw_route]] <- route1$lng
    route_record$lat[[draw_route]] <- route1$lat
    route_record$hours[[draw_route]] <- route1$hours
    route_record$miles[[draw_route]] <- route1$miles
    
    leafletProxy("TravelMap") %>% 
      addPolylines(lng = route_record$lng[[draw_route]], 
                   lat = route_record$lat[[draw_route]], weight = 3.5) %>%
      addAwesomeMarkers(lng = route_record$lng[[draw_route]], 
                        lat = route_record$lat[[draw_route]],
                        layerId = route_record$hours[[draw_route]],
                        label = route_record$miles[[draw_route]],
                        icon =makeAwesomeIcon(icon = "car", 
                                              markerColor = "blue", 
                                              iconColor = "white", library = "fa")
                        )
      
  })

# add national park markers 
  output$TravelMap <- renderLeaflet({
    DrawRoute(NULL, RouteLine, TripInfor) -> map
    map %>%
      addCircleMarkers(lat = NP$Latitude,
                       lng = NP$Longitude,
                       layerId = NP$Name,
                       popup = NP$Name,
                       color = "green",
                       stroke = FALSE,
                       radius = 6,
                       fillOpacity = 0.8) -> map

    map 
   }) 
   

}


shinyApp(ui, server)