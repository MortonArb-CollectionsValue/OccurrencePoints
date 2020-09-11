################################################################################

## 3-1_geo_refinement.R
### Authors: Shannon M. Still & Emily Beckman ### Date: 08/10/2020

### DESCRIPTION:

### DATA IN:

### DATA OUT:

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("ggplot2", "maps", "leaflet", "RColorBrewer", "dplyr")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

################################################################################
# Set working directory
################################################################################

source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
#source("scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))


################################################################################
################################################################################
# 1. Read in data
################################################################################

imls.output <- file.path(main_dir, "outputs")
path.pts <- file.path(imls.output, "spp_edited_points")
path.figs <- file.path(imls.output, "basic_maps_split_by_sp")

if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

map.world <- map_data("world")
# ------------------------------------------------------------------------------
# ---------------------------------------
# spp.test <- c("Quercus georgiana", "Quercus imbricaria", "Quercus arkansana", "Quercus falcata", "Quercus stellata", "Quercus acutissima")
spp.all <- tools::file_path_sans_ext(dir(path.pts, ".csv"))

################################################################################
# 1. Load file paths and data
################################################################################
# spp.v <- spp.test
spp.v <- spp.all

for(i in 1:length(spp.v)){
  spp.now <- spp.v[i]
  # spp.now <- gsub(spp.n)
  cat("Starting ", spp.now, ", ", i, " of ", length(spp.v), ".\n", sep="")

  dat.now <- read.csv(file.path(path.pts, paste0(spp.v[i], ".csv")))
  dat.now$decimalLatitude <- as.numeric(dat.now$decimalLatitude)
  dat.now$decimalLongitude <- as.numeric(dat.now$decimalLongitude)
  # summary(dat.now[dat.now$decimalLatitude<0,])
  summary(dat.now)

  if(nrow(dat.now[!is.na(dat.now$decimalLatitude),])==0) next

  png(file.path(path.figs, paste0(spp.now, "_raw.png")), height=6, width=10, units="in", res=180)
  print(
    ggplot() +
      coord_equal() +
      ggtitle(sub("_", " ", spp.now)) +
      geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
      geom_point(data=dat.now, aes(x=decimalLongitude, y=decimalLatitude), color="red", size=2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_minimal()
  )
  dev.off()

    cat("\tEnding ", spp.now, ", ", i, " of ", length(spp.v), ".\n\n", sep="")
}
# ---------------------------------------
rm(data.now)

################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################

# list of test species
# spp.test <- c("Quercus_boyntonii","Quercus_dalechampii","Quercus_georgiana",
#               "Quercus_imbricaria","Quercus_arkansana","Quercus_falcata","Quercus_stellata",
#               "Quercus_acutissima","Quercus_palmeri")

# spp.v <- spp.test
# spp.v <- spp.all

# function for mapping points
map.pts <- function(pts){
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    ## addPolygon() -- country level distibution from GTS
    addCircleMarkers(
      data = pts,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      popup = ~paste(
        "Species name:",taxon_name_full,"(",list,")","<br/>",
        "Database:",database,"<br/>",
        "Dataset name:",datasetName,"<br/>",
        #"All source databases:",source_databases,"<br/>",
        "Year:",year,"<br/>",
        "Basis of record:",basisOfRecord,"<br/>",
        "Establishment means:",establishmentMeans,"<br/>",
        "Coordinate uncertainty:",coordinateUncertaintyInMeters,"<br/>",
        "ID:",UID),
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(database)) %>%
    addControl(
      pts$species_name_acc[1],
      position = "topright") %>%
    addLegend(
      pal = palette,
      values = unique(dat.now$database),
      title = "Source database",
      position = "bottomright",
      opacity = 0.6)
  return(map)
}

## run through species and save maps
for(i in 1:length(spp.v)){
    cat("Starting ", spp.v[i], ", ", i, " of ", length(spp.v), ".\n", sep="")

  # read file
  dat.now <- read.csv(file.path(path.pts,
                                paste0(spp.v[i], ".csv")), colClasses = "character")
                                # paste0(spp.all[1], ".csv")), colClasses = "character")
  # lat and long to numeric
  dat.now$decimalLatitude <- as.numeric(dat.now$decimalLatitude)
  dat.now$decimalLongitude <- as.numeric(dat.now$decimalLongitude)
  # set database as factor and order appropriately
  dat.now$database <- factor(dat.now$database,
                             levels = c("FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN"))
  dat.now <- dat.now %>% arrange(desc(database))
  # create color palette
  colors <- c("#f00e99","#d91818","#cc671b","#bf9c1f","#28a822","#238b99",
              "#234691","#622180")
  palette <- colorFactor(palette=colors, levels=unique(dat.now$database))
  # create map
  map_final <- map.pts(dat.now); map_final
  # save map
  htmlwidgets::saveWidget(map_final, file.path(imls.output,
                                               "interactive_maps_split_by_sp", paste0(spp.v[i], "_leaflet_map.html")))

      cat("\tEnding ", spp.v[i], ", ", i, " of ", length(spp.v), ".\n\n", sep="")

}








  palette <- colorFactor(c("red","navy"), domain = c("FALSE","TRUE"))
## CENTROIDS
  eo.post3$.cen <- as.factor(eo.post3$.cen)
  eo.post3 <- eo.post3 %>% arrange(desc(.cen))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.cen)) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points within 500 meters of country or state centroids"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagCENTROIDS","_leaflet_map.html")))
## URBAN
  eo.post3$.urb <- as.factor(eo.post3$.urb)
  eo.post3 <- eo.post3 %>% arrange(desc(.urb))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.urb)) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points within urban areas (rnaturalearth layer)"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagURBAN","_leaflet_map.html")))
## INSTITUTIONS
  eo.post3$.inst <- as.factor(eo.post3$.inst)
  eo.post3 <- eo.post3 %>% arrange(desc(.inst))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.inst)) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points within 100 meters of biodiversity institutions"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagINSTITUTIONS","_leaflet_map.html")))
## MISMATCH COUNTRIES
  eo.post3$.con <- as.factor(eo.post3$.con)
  eo.post3 <- eo.post3 %>% arrange(desc(.con))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.con)) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points where given country doesn't match point country"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagCOUNTRY","_leaflet_map.html")))
## OUTLIERS
  eo.post3$.outl <- as.factor(eo.post3$.outl)
  eo.post3 <- eo.post3 %>% arrange(desc(.outl))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.outl)) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points are outliers (CoordinateCleaner 'quantile' method)"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagOUTLIERS","_leaflet_map.html")))
## GTS NATIVE
  eo.post3$.gtsnative <- as.factor(eo.post3$.gtsnative)
  eo.post3 <- eo.post3 %>% arrange(desc(.gtsnative))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.gtsnative)) %>%
    addPolygons(data = x2,
			color = "black",
			weight = 2,
			opacity = 0.6,
			fillOpacity = 0) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points outside native countries (GlobalTreeSearch)"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagGTSNATIVE","_leaflet_map.html")))
## RL NATIVE
  eo.post3$.rlnative <- as.factor(eo.post3$.rlnative)
  eo.post3 <- eo.post3 %>% arrange(desc(.rlnative))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.rlnative)) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points outside native countries (IUCN Red List)"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagRLNATIVE","_leaflet_map.html")))
## RL INTRODUCED
  eo.post3$.rlintroduced <- as.factor(eo.post3$.rlintroduced)
  eo.post3 <- eo.post3 %>% arrange(desc(.rlintroduced))
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post3,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = ~palette(.rlintroduced)) %>%
    addControl(
      paste0(eo.post3$species_name_acc[1],": Flagged points in introduced countries (IUCN Red List)"),
      position = "topright")
  map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
    paste0(eo.post3$species_name_acc[1],
      "_flagRLINTRODUCED","_leaflet_map.html")))

eo.post_s <- eo.post3 %>%
  filter(.cen == "TRUE") %>%
  filter(.inst == "TRUE") %>%
  filter(.urb == "TRUE") %>%
  filter(.con == "TRUE") %>%
  filter(.outl == "TRUE") #%>%
  #filter(.gtsnative == "TRUE") %>%
  #filter(.rlnative == "TRUE")
  map <- leaflet() %>%
    addProviderTiles("CartoDB.PositronNoLabels") %>%
    addCircleMarkers(
      data = eo.post_s,
      lng = ~decimalLongitude,
      lat = ~decimalLatitude,
      radius = 5,
      fillOpacity = 0.6,
      stroke = F,
      color = "navy") %>%
    addControl(
      paste0(eo.post_s$species_name_acc[1],": "),
      position = "topright")
  map
