library(shiny);library(shinycssloaders);library(shinydashboard);library(shinyWidgets)
library(plotly);library(DT)
library(leaflet);library(leaflet.extras)
library(dplyr);library(sp);library(rgdal)
#library(viscover)

setwd("C:/Users/eugen/Dropbox/My/My App/TravelApp/")
source("BaseFun.R")
TripInfor <- readRDS("TripInfor.rds")
RouteLine <- readRDS("RouteLine.rds")
NP <- read.csv("nps_parks.csv")
plot_path <- "C:\\Users\\eugen\\Dropbox\\My\\My App\\TravelApp\\www\\photo"
plot_path2 <-  "C:/Users/eugen/Dropbox/My/My App/TravelApp/www/photo"
ui <- dashboardPage(skin = "blue",
 dashboardHeader(title = "My Apps", titleWidth = 550, disable = FALSE),
 
 dashboardSidebar(
   tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
   sidebarMenu(
     # Setting id makes input$tabs give the tabName of currently-selected tab
     id = "tabs",
     menuItem(text = "US Map", tabName = "dashboard", icon = icon("list-alt"),
              badgeColor = "yellow"),
     menuItem(text = "Travel Histroy", icon = icon("th"), tabName = "widgets", # badgeLabel = "new",
              badgeColor = "green"),
     menuItem(text = "Design Travel Route", icon = icon("bar-chart-o"), 
              tabname = "Plan",
              badgeColor = "blue")
              #menuSubItem("Sub-item 1", tabName = "subitem1"),
              #menuSubItem("Sub-item 2", tabName = "subitem2")
      ),
   sidebarSearchForm(label = "Departure Address", 
                     textId = "searchText", 
                     buttonId = "searchButton"),
   sidebarSearchForm(label = "Destination Address", 
                     textId = "searchText", 
                     buttonId = "searchButton")

 ),
 
 dashboardBody(
     tabItems(
       tabItem(tabName = "dashboard",
               fluidRow(
                 box(width = 15,
                     leafletOutput("statemap", width = "100%", height = 750)
                    )
                 )
       ),
       
       
       tabItem(tabName = "widgets",
               fluidRow(
                 box(width = 10,
                     leafletOutput("TravelMap", width = "100%", height = 450)),
                 hr(),
                 box(title = "Gallery", status = "success", solidHeader = TRUE, width = 10, 
                     fluidRow(width = 10,
                              plotOutput("plot_gallery", height = "100%") %>% withSpinner()
                     )),
                 hr(), hr(),
                 tags$head(tags$script(src = "http://www.elevateweb.co.uk/wp-content/themes/radial/jquery.elevatezoom.min.js")),
                 actionButton("myBtn", "Press Me for zoom!"), 
                 uiOutput("myImage"),
                 singleton(
                   tags$head(tags$script('Shiny.addCustomMessageHandler("testmessage",
  function(message) {
    $("#myImage img").elevateZoom({scrollZoom : true});
  }
);')))
               ),
       ),
       
       tabItem(tabName = "Plan")

     )
 )
)
  

##############################################################################

server <- function(input, output, session)
{

  # the map of states, 
  output$statemap <- renderLeaflet({
    DrawRoute(NULL, NULL, TripInfor) %>%
      addTiles(options = tileOptions(opacity = 0.8)) -> p
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

  # selectmarker <- reactiveValues(name = "Key West")
  myplace_marker <- 
    observeEvent(input$TravelMap_marker_click, { # update the location selectInput on map clicks
    point <- input$TravelMap_marker_click
    mymarker <- NULL
    if(sum((TripInfor$lat == point$lat) * (TripInfor$lng == point$lng )) > 0)
      mymarker <- TripInfor %>% filter(lng == point$lng, lat == point$lat) 
    selectmarker <- data.frame(name = "Key West")
    if(is.null(mymarker) == F)
    {
      selectmarker$name <- mymarker$name
    }
   # print(selectmarker)
    selectmarker$name
    # State <-  latlong2state(data.frame(x = point$lng, y = point$lat))
  })
  
  # loc = reactiveValues(name = "Key West")
  output$plot_gallery <- renderImage(
    {# loc <- ifesle(is.null(myplace_marker()), "Key West", myplace_marker())
    loc = "Key West"
    oneplot = paste(plot_path, loc, "showplot.JPG", sep = "\\")
    list(src = oneplot,
         width = 400,
         height = 300,
         alt = "This is alternate text")
    }, deleteFile = FALSE
  )
  
  output$myImage <- renderUI({
    loc = "Key West"
    oneplot = paste(plot_path2, loc, "showplot.JPG", sep = "/")
    img(src = "https://raw.githubusercontent.com/XiaodanLyu/viscover/master/figures/overlay-CDL2018.png",  
        "data-zoom-image" = "https://raw.githubusercontent.com/XiaodanLyu/viscover/master/figures/overlay-CDL2018.png")
  })
  
  observe({
    if(input$myBtn > 0){
      session$sendCustomMessage(type = 'testmessage',
                                message = list())             
    }
  })

  # add point infor record
  # point_marker <- reactiveValues(lng = NULL, lat = NULL, Id = NULL)
  # observeEvent(input$statemap_click,{
  #   # Respond to "event-like" reactive inputs, values, and expressions.
  #   click <- input$statemap_click                        # click point, the name is called as "xx_click" 
  #   clng <- click$lng
  #   clat <- click$lat
  #   Id <- Sys.time() %>% as.character()
  #   StateValue = latlong2state(data.frame(x = clng, y = clat))
  # 
  #   point_marker$lng = c(point_marker$lng, clng)
  #   point_marker$lat = c(point_marker$lat, clat)
  #   point_marker$Id = c(point_marker$Id, Id)
  #   point_marker$state = c(point_marker$state, StateValue)
  # 
  #   leafletProxy("statemap") %>%
  #   addAwesomeMarkers(
  #     lng = clng, lat = clat, icon = makeAwesomeIcon(icon = "heart", markerColor = "orange"), layerId = Id, clusterId = "C",
  #     label = HTML(
  #       sprintf("<style>td{padding: 5px} </style>
  #                  <table style = 'background:rgb(255,255,255)' border>
  #                  <tr><td>Location</td><td>(%.4f, %.4f)</td></tr>
  #                  <tr><td>State</td><td>%s</td></tr>
  #                  </table>", clng, clat, StateValue )),
  #     labelOptions = labelOptions(
  #       offset = c(-80, -80), opacity = 1,
  #       textOnly = T, textsize = "12px",
  #       style = list("font-weight" = "bold",
  #                    "float" = "left",
  #                    "width" = "100%"))
  #   )
  # })
  # 
  # # remove the clicked marker 
  # observeEvent(input$statemap_marker_click,{
  #   Id.rm_c <- which(point_marker$Id == input$statemap_marker_click$id)
  #   if(length(Id.rm_c)){
  #     point_marker <- lapply(point_marker, function(x) x[-Id.rm_c])
  #   }
  #   
  #   leafletProxy("statemap") %>% removeMarker(layerId = input$statemap_marker_click$id)
  # })
  


  output$TravelMap <- renderLeaflet({
    DrawRoute(NULL, RouteLine, TripInfor) -> map
    map %>% addAwesomeMarkers(lng = NP$Longitude, lat = NP$Latitude, 
                              popup = NP$Name, label = NP$Name,
                              icon =makeAwesomeIcon(icon = "tree", markerColor = "green", 
                                iconColor = 'white', library = "fa")) -> map

    map 
   }) 
   

}


shinyApp(ui, server)