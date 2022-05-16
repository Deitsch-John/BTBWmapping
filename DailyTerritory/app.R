#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tidyverse)
library(ggspatial)
library(terra)

<<<<<<< HEAD
setwd("C:/Users/jfdei/OneDrive/Desktop/BTBWmapping")

=======
>>>>>>> 1c7df19ab17f5e652279c32e4dcac9162099531c
#load HB shape-files 
hbef.roads = read_sf(layer = "hbef_roads", dsn = "./spatialdata")
hbef.streams = read_sf(layer = "hbef_hydro", dsn = "./spatialdata")
hbef.elevation10 = read_sf(layer = "hbef_cont10", dsn = "./spatialdata")
HBEFrast = rast("./spatialdata/hbef_dem.tif")

obs_to_coords <- function(df, coord_col, crs_add){
  coords.sfg <- df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)
  
  df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)
  
}

BTBW = read.csv("BTBW.csv", header = TRUE)
BTBW2 = BTBW %>%
  rowwise()%>%
  mutate(coords = list(st_point(c(long,lat))))  #create list column

BTBW2.sf <- obs_to_coords(BTBW2, coords, 4269)

st_crs(hbef.streams) = "EPSG:26919"

hbef.streams.4269 = hbef.streams %>%
  st_transform(crs = 4269)
hbef.roads.4269 = hbef.roads %>%
  st_transform(crs = 4269)
hbef.elevation10.4269 = hbef.elevation10 %>%
  st_transform(crs = 4269)

HBEFrast2 <- project(HBEFrast, "EPSG:4269")
ext <- terra::ext(BTBW2.sf)

birdplot.rast <- HBEFrast2[ext,]
birdplot.cont <- as.contour(birdplot.rast)

plot(birdplot.rast)
plot(birdplot.cont, add =TRUE)

ele.ext <- terra::extract(birdplot.rast, vect(BTBW2.sf))
BTBW.fin <- bind_cols(BTBW2.sf, ele.ext)

#function to plot bird territory 
BirdMap <- function(birdtomap) {
  
  BirdData <- BTBW2.sf %>%
    filter(BirdID==birdtomap)
  
  BirdTerritory <- BirdData %>%
    group_by(BirdID)%>%
    summarize(gps_points = n())%>%
    rowwise()%>%
    mutate(border = st_convex_hull(.))
  
  Extent <- st_bbox(BTBW2.sf)
  
  BirdMap <- ggplot()+
    geom_sf(data = hbef.elevation10.4269, 
            color = "grey88", size = 0.3)+
    geom_sf(data = hbef.streams.4269, 
            size = 0.8, color = "steelblue2")+
    geom_sf(data = hbef.roads.4269, 
            size = 0.8, color = "black")+
    geom_sf(data = BirdTerritory$border, 
            fill = "firebrick1", alpha = 0.2)+
    geom_sf(data = BirdData)+
    geom_sf(data = filter(BTBW2.sf, !BirdID%in%c(birdtomap)),
            alpha = 0.2)+
    coord_sf(ylim = c(Extent[2], Extent[4]), 
             xlim = c(Extent[1], Extent[3]))+
    theme(
      panel.grid.major = element_line(linetype = "dashed", 
                                      color = "grey94"),
      panel.background = element_rect(fill = "grey98"),
      panel.border = element_rect(fill = NA, size = 2))+
    annotation_scale(location="bl")+
    annotation_north_arrow(which_north = "true",
                           style = north_arrow_nautical,
                           location = "tr")+
    labs(title = birdtomap)
  
  return(BirdMap)
}
get_obs <- function(bird_id){
  obs <- BTBW.fin %>%
    st_drop_geometry()%>%
    filter(BirdID==bird_id)%>%
    select(-ID)%>%
    rename(`Bird ID` = BirdID,
           Latitude = lat,
           Longitude = long,
           `Date-Time` = timestamp,
           Observer = observer,
           Coordinates = coords,
           `Elevation (m)` = hbef_dem)%>%
    gt::gt()
  
  return(obs)
}
territory_elev <- function(bird_id){
  obs <- BTBW.fin %>%
    st_drop_geometry()%>%
    dplyr::filter(BirdID==bird_id)%>%
    dplyr::select(BirdID, hbef_dem)%>%
    dplyr::filter(!is.na(hbef_dem))%>%
    summarize(`Maximum Elevation` = max(hbef_dem),
              `Mean Elevation` = mean(hbef_dem),
              `Minimum Elevation` = min(hbef_dem),
              `Elevation Variation` = sd(hbef_dem))%>%
    mutate(`Elevation Range` = `Maximum Elevation`-`Minimum Elevation`)%>%
    pivot_longer(1:5, names_to = "Metric", values_to = "Value")%>%
    gt::gt()%>%
    gt::fmt_number(
      columns = c("Value"),
      decimals = 2)
  
  return(obs)
}

BirdList <- BTBW.fin %>%
  st_drop_geometry()%>%
  group_by(BirdID)%>%
  summarize(gps_points = n())%>%
  filter(gps_points > 4)%>%
  dplyr::select(BirdID)%>%
  distinct()%>%
  arrange(BirdID)%>%
  pull(BirdID)


#__________________________________________________

#Define UI
ui <- fluidPage(
  titlePanel("BTBW Territory Mapper"),
  fluidRow(column(3, 
                  selectInput(inputId = "Birds", label = "Bird ID", choices = BirdList),
                  tableOutput("Elevation")
  ),
          column(9, 
                  plotOutput("Map")
                 )
  ),
  fluidRow(tableOutput("Observations"))
  
)

#Define server
server <- function(input, output, session) {
  output$Map <- renderPlot(BirdMap(input$Birds))
  output$Observations <- renderTable(get_obs(input$Birds))
  output$Elevation <- renderTable(territory_elev(input$Birds))
  
}


# Run the application 
shinyApp(ui = ui, server = server)

runGitHub(repo = "BTBWmapping", username = "Deitsch-John", 
         subdir = "DailyTerritory")