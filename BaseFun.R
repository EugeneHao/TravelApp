library(tidyverse)
library(leaflet)
library(osrm)
library(sf)
library(RColorBrewer)
# Load Trip Information Data and Add new ones
AddTrip <- function(olddata, newloc, travelname, save = FALSE)
{
  if(is.null(olddata) == T)
    olddata <- readRDS("~/GitHub/TravelApp/TripInfor.rds")
  temp <- ggmap::geocode(newloc)
  newdata <- data.frame(name = newloc, 
                        lng = temp$lon, lat = temp$lat, 
                        travel = rep(travelname, length(newloc)))
  comb <- rbind(olddata, newdata)
  if(save == TRUE)
  {
    saveRDS(comb, "~/GitHub/TravelApp/TripInfor.rds")
    for(i in 1:length(newloc))
    {
      paths = paste("~/GitHub/TravelApp/www/photo/", newloc, sep = "")
      if(dir.exists(paths[i]) == 0)
        dir.create(paths[i])
    }
  }
  
  return(comb)
}

GetRouteline <- function(TripInfor, save = FALSE)
{
  SplitTrip <- TripInfor %>% split(., TripInfor$travel)
  Route <- SplitTrip %>% 
    purrr::map(., .f = function(x) osrmTrip(x, returnclass="sf", overview = "full")) %>% 
    purrr::map(., .f = function(x) x[[1]]$trip %>% sf::st_coordinates() %>% data.frame())
  if(save == TRUE)
    saveRDS(Route, "~/GitHub/TravelApp/RouteLine.rds")
  return(Route)
}



DrawRoute <- function(map, Route, TripInfor)
{
  if(is.null(map) == T)
  {
    mapStates = maps::map("state", fill = TRUE, plot = FALSE)
    mymap <- leaflet(mapStates) %>% addTiles() %>% 
      addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = T,
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.2,
                  highlight = highlightOptions(
                    weight = 2,
                    color = "grey",
                    dashArray = "",
                    fillOpacity = 0),
                  label = mapStates$names,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "20px",
                    direction = "auto")) %>%
      setView(-93.65, 42.0285, zoom = 5) 
  }
  if(is.null(Route) == F)
  {
    SplitTrip <- TripInfor %>% split(., TripInfor$travel)
    for(i in 1:length(SplitTrip))
    {
      mymap %>% addPolylines(lng = Route[[i]]$X, lat = Route[[i]]$Y, weight = 3) %>%
        addAwesomeMarkers(lat = SplitTrip[[i]]$lat,
                          lng = SplitTrip[[i]]$lng,
                          layerId = SplitTrip[[i]]$name,
                          popup = SplitTrip[[i]]$name,
                          icon =makeAwesomeIcon(icon = "car", 
                                                markerColor = c("orange", "yellow", "green", "cadetblue", 
                                                                "blue", "darkblue", "purple", "pink", "grey")[i%%9], 
                                                iconColor = "white", library = "fa")) -> mymap
    }
  }
  return(mymap)
}


# this function give the state value if we know its longitude and latitude 
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- maptools::map2SpatialPolygons(states, IDs=IDs,
                                             proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- sp::SpatialPoints(pointsDF, 
                                proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- sp::over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

# combine the JPG files togethoer and save the picture under the same folder
# if the picture already exist, then skip

#   combinepicture(path = "~/GitHub/TravelApp/www/photo")
combinepicture <- function(path)
{
  sf_name <- list.files(path)
  for(i in 1:length(sf_name))
  {
    subpath <- paste(path, sf_name[i], sep = "/")
    files <- c(list.files(path= subpath, pattern="*.JPG", all.files=T, full.names=T),
               list.files(path= subpath, pattern="*.jpg", all.files=T, full.names=T))
    if(files %>% length() > 0)
    {
      files <- files[paste0(basename((files))) != "showplot.JPG"]  # remove the show plot 
      #height = ifelse(length(files) %% 3 ==0, length(files) %/% 3, length(files) %/% 3+1) *480
      height = 960
      width = 1920
      jpeg(paste(subpath, "showplot.JPG", sep = "/"), width = width, height = height)
      filelist <- lapply(files, jpeg::readJPEG)
      names(filelist) <- paste0(basename((files)))
      list2env(filelist, envir=.GlobalEnv)
      
      par(mar=rep(0,4))
      
      layout(matrix(1: (height/480*3), ncol=3, byrow=TRUE))
      
      for(i in 1:length(files)) {
        img <- jpeg::readJPEG(files[i])
        plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
        rasterImage(img,0,0,1,1)
      }
      if(length(files) < 6)
        for(i in (length(files)+1):6)
          plot(NA,xlim=0:1,ylim=0:1,xaxt="n",yaxt="n",bty="n")
      dev.off()
    }
  }
}


#TripInfor <- AddTrip(NULL,  newloc = newloc, travelname = "2018Winter", save = TRUE)


#DrawRoute(NULL, TripInfor)
