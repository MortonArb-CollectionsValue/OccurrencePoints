################################################################################

## 4-0_plot_occurrence_points.R
### Authors: Emily Beckman & Christy Rollinson ### Date: 09/11/2020

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
# Use leaflet package to create interactive maps to explore (html)
################################################################################

# set up file paths
imls.output <- file.path(main_dir, "outputs")
path.pts <- file.path(imls.output, "spp_edited_points")
spp.test <- c("Quercus_boyntonii","Quercus_dalechampii","Quercus_georgiana",
              "Quercus_imbricaria","Quercus_arkansana","Quercus_falcata",
              "Quercus_stellata","Quercus_acutissima","Quercus_palmeri")
spp.all <- tools::file_path_sans_ext(dir(path.pts, ".csv"))

# create folder for maps, if not yet created
if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

# cycle through each species file and create map
for(i in 1:length(spp.test)){

  # read in records
  spp.now <- read.csv(file.path(main_dir,"outputs","spp_edited_points",
    paste0(spp.test[4], ".csv")))

  # set database as factor and order appropriately
  spp.now$database <- factor(spp.now$database,
    levels = c("Ex_situ","FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN"))
  spp.now <- spp.now %>% arrange(desc(database))
  # create color palette... used http://medialab.github.io/iwanthue/
  colors <- c("#B88F4F","#B5AF3C","#83B53C","#33A874","#259699","#2F74B0","#8B59BD")
  database.pal <- colorFactor(palette=colors, levels=unique(spp.now$database))

  # create map
  map <- leaflet() %>%
    # Base layer groups
      addProviderTiles(providers$CartoDB.PositronNoLabels, group = "CartoDB.PositronNoLabels") %>%
      #addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      #addProviderTiles(providers$Esri.WorldTopoMap, group = "Esri.WorldTopoMap") %>%
      #addProviderTiles(providers$Stamen.Watercolor, group = "Stamen.Watercolor") %>%
    addControl(spp.test[4], position = "topright") %>%
    # Color by database
    addCircleMarkers(data = spp.now, ~decimalLongitude, ~decimalLatitude,
      popup = ~paste(
        "Taxon name:",taxon_name_full,"(",list,")","<br/>",
        "Database:",database,"<br/>",
        "Dataset name:",datasetName,"<br/>",
        "All source databases:",all_source_databases,"<br/>",
        "Year:",year,"<br/>",
        "Basis of record:",basisOfRecord,"<br/>",
        "Establishment means:",establishmentMeans,"<br/>",
        "Coordinate uncertainty:",coordinateUncertaintyInMeters,"<br/>",
        "ID:",UID),
      color = ~database.pal(database),radius = 5,fillOpacity = 0.6,stroke = F) %>%
    # Overlay groups (can toggle)
    addCircleMarkers(data = spp.now %>% filter(!.cen), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "Within 500m of country/state centroid (.cen)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.urb), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "In urban area (.urb)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.inst), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "Within 100m of biodiversity institution (.inst)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.con), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "Not in reported country (.con)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.outl), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "Geographic outlier (.outl)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.gtsnative), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "Outside GTS native country (.gtsnative)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.rlnative), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "Outside IUCN RL native country (.rlnative)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.rlintroduced), ~decimalLongitude, ~decimalLatitude,
      radius = 5, fillOpacity = 0.8, stroke = F, color = "red",
      group = "In IUCN RL introduced country (.rlintroduced)") %>%
    # Layers control
    addLayersControl(
      #baseGroups = c("CartoDB.PositronNoLabels",
      #               "CartoDB.Positron",
      #               "Esri.WorldTopoMap",
      #               "Stamen.Watercolor"),
      overlayGroups = c("Within 500m of country/state centroid (.cen)",
                        "In urban area (.urb)",
                        "Within 100m of biodiversity institution (.inst)",
                        "Not in reported country (.con)",
                        "Geographic outlier (.outl)",
                        "Outside GTS native country (.gtsnative)",
                        "Outside IUCN RL native country (.rlnative)",
                        "In IUCN RL introduced country (.rlintroduced)"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    #hideGroup("Within 500m of country/state centroid (.cen)") %>%
    #hideGroup("In urban area (.urb)") %>%
    #hideGroup("Within 100m of biodiversity institution (.inst)") %>%
    #hideGroup("Not in reported country (.con)") %>%
    #hideGroup("Geographic outlier (.outl)") %>%
    #hideGroup("Outside GTS native country (.gtsnative)") %>%
    #hideGroup("Outside IUCN RL native country (.rlnative)") %>%
    #hideGroup("In IUCN RL introduced country (.rlintroduced)") %>%
    addLegend(pal = database.pal, values = unique(spp.now$database),
      title = "Source database", position = "bottomright", opacity = 0.6)
  map
  # save map
  htmlwidgets::saveWidget(map, file.path(main_dir,"outputs",
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
