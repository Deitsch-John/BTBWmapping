library(shiny)
library(sf)
library(tidyverse)
library(ggspatial)
library(terra)
library(gt)
library(units)
library(raster)

url = "https://github.com/Deitsch-John/BTBWmapping/raw/main/spatial.zip.zip"
download.file(url, "spatial.zip.zip")
unzip("spatial.zip.zip")

#load HB shape-files
hbef.roads = read_sf(layer = "hbef_roads", dsn = ".")%>%
  st_transform(crs = 32619)
hbef.streams = read_sf(layer = "hbef_hydro", dsn = ".")
hbef.elevation10 = read_sf(layer = "hbef_cont10", dsn = ".")%>%
  st_transform(crs = 32619)
HBEFrast = rast("hbef_dem.tif")
plotgrid <- read_sf(layer = "virtual_grid_megamain", dsn = ".")

obs_to_coords <- function(df, coord_col, crs_add){
  coords.sfg <- df$coords
  coords.sfc <- st_sfc(coords.sfg, crs = crs_add)
  
  df %>%
    st_as_sf(geometry = coords.sfc)%>%
    st_transform(., crs = crs_add)
  
}

BTBW = read_csv("https://raw.githubusercontent.com/Deitsch-John/BTBWmapping/841db73a2fc84d6b5209e03885ad8604c388c10a/BTBW.csv")
BTBW2 = BTBW %>%
  rowwise()%>%
  mutate(coords = list(st_point(c(long,lat))))  #create list column

BTBW.sf <- obs_to_coords(BTBW2, coords, 4269)
BTBW.sf <- st_transform(BTBW.sf, 32619)

st_crs(hbef.streams) = "EPSG:32619"
hbef.streams.4269 = hbef.streams %>%
  st_transform(crs = 4269)
# hbef.roads.4269 = hbef.roads %>%
#   st_transform(crs = 4269)
# hbef.elevation10.4269 = hbef.elevation10 %>%
#   st_transform(crs = 4269)

HBEFrast2 <- project(HBEFrast, "EPSG:32619")
ext <- terra::ext(BTBW.sf)

birdplot.rast <- HBEFrast2[ext,]

ele.ext <- terra::extract(birdplot.rast, vect(BTBW.sf))
BTBW.fin <- bind_cols(BTBW.sf, ele.ext)

plotgrid.small <- plotgrid %>%
  mutate(testing = grepl("C", alpha, fixed = TRUE))%>%
  filter(testing==TRUE)%>%
  filter(as.numeric(numeric)<101)%>%
  dplyr::select(-testing)

text.grid <- plotgrid.small %>%
  dplyr::select(-numeric, -alpha)%>%
  st_coordinates()%>%
  as_data_frame()%>%
  group_by(L2)%>%
  summarize(X.x = mean(X),
            Y.y = mean(Y))%>%
  dplyr::select(X.x, Y.y)

text.grid2 <- bind_cols(plotgrid.small, text.grid)

#function to plot bird territory 
BirdMap <- function(data, bird_id, scale) {
  BirdTerritory <- data %>%
    filter(BirdID==bird_id)%>%
    group_by(BirdID)%>%
    summarize(gps_points = n())%>%
    rowwise()%>%
    mutate(border = st_convex_hull(.))
  
  Extent <- if(scale=="plot"){
    st_bbox(BTBW.sf)
  } else if (scale=="territory") {
    st_bbox(st_buffer(BirdTerritory, 40))
  } else {
      print("wrong scale provided")
  }
  
  BirdMap <- ggplot()+
    geom_sf(data = hbef.elevation10.4269, 
            color = "grey88", size = 0.3)+
    geom_sf(data = hbef.streams.4269, 
            size = 0.8, color = "steelblue2")+
    geom_sf(data = hbef.roads.4269, 
            size = 0.8, color = "black")+
    geom_sf(data = BirdTerritory$border, 
            fill = "firebrick1", alpha = 0.2)+
    geom_sf(data = BirdTerritory$geometry)+
    geom_sf(data = filter(data, !BirdID%in%c(bird_id)),
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
    labs(title = bird_id)
  
  return(BirdMap)
}
get_obs <- function(data, bird_id){
  obs <- data %>%
    st_drop_geometry()%>%
    filter(BirdID==bird_id)%>%
    dplyr::select(-ID)%>%
    rename(`Bird ID` = BirdID,
           Latitude = lat,
           Longitude = long,
           `Date-Time` = timestamp,
           Observer = observer,
           Coordinates = coords,
           `Elevation (m)` = hbef_dem)%>%
    arrange(`Date-Time`)%>%
    dplyr::select(-Coordinates)%>%
    gt::gt()
  
  return(obs)
}
territory_elev <- function(data, bird_id, elev_raster){
  
  territory <- data %>%
    filter(BirdID==bird_id)%>%
    group_by(BirdID)%>%
    summarize(gps_points = n())%>%
    rowwise()%>%
    mutate(border = st_convex_hull(.))%>%
    pull(border)
  
  rast.extracts <- terra::extract(elev_raster, vect(territory))%>%
    summarize(`Maximum Elevation` = max(hbef_dem),
              `Mean Elevation` = mean(hbef_dem),
              `Minimum Elevation` = min(hbef_dem),
              `Elevation Variation` = var(hbef_dem))%>%
    mutate(`Elevation Range` = `Maximum Elevation`-`Minimum Elevation`)%>%
    pivot_longer(1:5, names_to = "Metric", values_to = "Value")%>%
    gt::gt()%>%
    gt::fmt_number(
      columns = c("Value"),
      decimals = 2)
  
  return(rast.extracts)
}
territory_size <- function(data, bird_id){
  BirdTerritory <- data %>%
    filter(BirdID==bird_id)%>%
    group_by(BirdID)%>%
    summarize(gps_points = n())%>%
    rowwise()%>%
    mutate(border = st_convex_hull(.))
  
  Area <- st_area(BirdTerritory$border)
  units(Area) <- as_units("ha")
  return(Area)
}
territory_rast <- function(data, bird_id, elev_raster){
  territory <- data %>%
    filter(BirdID==bird_id)%>%
    group_by(BirdID)%>%
    summarize(gps_points = n())%>%
    rowwise()%>%
    mutate(border = st_convex_hull(.))%>%
    pull(border)
  
  territory.rast <- mask(crop(elev_raster, vect(territory)), vect(territory))
  plot(territory.rast)
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

#Set UI
ui <- fluidPage(
  titlePanel("BTBW Territory Mapper"),
  tabsetPanel(
    tabPanel("Maps",
             fluidRow(column(4,
                             selectInput(inputId = "Birds", label = "Choose Bird to Map", choices = BirdList),
                             plotOutput("Map_small"),
             ),
             column(8, 
                    plotOutput("Map_big")
             )
             )),
    tabPanel("Information",
             fluidRow(column(4,
                             selectInput(inputId = "Birds2", label = "Choose Bird", choices = BirdList),
                             tableOutput("Elevation"),
                             verbatimTextOutput("Territory Size"),
                             plotOutput("Map_raster")
             ),
             column(8,
                    tableOutput("Observations")
             )
             )
    )
  )
)

#Define server
server <- function(input, output, session) {
  output$Map_big <- renderPlot(BirdMap(BTBW.sf, input$Birds, "plot"))
  output$Map_small <- renderPlot(BirdMap(BTBW.sf, input$Birds, "territory"))
  output$Observations <- renderTable(get_obs(BTBW.fin, input$Birds2))
  output$Elevation <- renderTable(territory_elev(BTBW.sf, input$Birds2, birdplot.rast))
  output$`Territory Size` <- renderPrint(territory_size(BTBW.sf, input$Birds2))
  output$Map_raster <- renderPlot(territory_rast(BTBW.sf, input$Birds2, birdplot.rast))
}

# Run the application 
shinyApp(ui = ui, server = server)

