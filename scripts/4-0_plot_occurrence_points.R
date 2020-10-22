################################################################################

## 4-0_plot_occurrence_points.R
### Authors: Emily Beckman & Christy Rollinson ### Date: 09/11/2020

### DESCRIPTION:
  # Creates interactive (HTML) occurrence point map for each target species,
  #   for exploring. Includes toggles that show points flagged in
  #   3-1_refine_occurrence_points.R
  #   Also creates two fixed basic (PNG) maps for each target species: one with
  #   all valid occurrence points (output from 3-0_compile_occurrence_points.R)
  #   and another with all flagged points removed (output from
  #   3-1_refine_occurrence_points.R)

### DATA IN:
  # Occurrence points from 3-1_refine_occurrence_points.R

### DATA OUT:
  # spp_interactive_maps folder with HTML map for each target species
  #   (e.g., Quercus_lobata_leafet_map.html), which can be downloaded and opened
  #   in your browser for exploring
  # spp_basic_maps folder with PNG maps for each target species, one with
  #   all valid points (e.g., Quercus_lobata_raw.png) and one with unflagged
  #   points only (e.g., Quercus_lobata_filtered.png)

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
path.figs <- file.path(imls.output, "spp_interactive_maps")
              # Christy/Murphy's target species
spp.all <- c("Quercus_dalechampii","Quercus_imbricaria","Quercus_falcata",
              "Quercus_stellata","Quercus_acutissima","Quercus_palmeri",
              # Sean's target species
            "Quercus_acerifolia","Quercus_arkansana","Quercus_austrina",
            "Quercus_boyntonii","Quercus_georgiana","Quercus_havardii",
            "Quercus_oglethorpensis"
            #"Quercus_ajoensis",#"Quercus_carmenensis","Quercus_graciliformis",
            #"Quercus_cedrosensis","Quercus_dumosa","Quercus_engelmannii",
            #"Quercus_hinckleyi","Quercus_pacifica","Quercus_robusta",
            #"Quercus_tardifolia","Quercus_tomentella"
            )
#spp.all <- tools::file_path_sans_ext(dir(path.pts, ".csv"))

# create folder for maps, if not yet created
if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

# cycle through each species file and create map
for(i in 1:length(spp.all)){

  # read in records
  spp.now <- read.csv(file.path(path.pts, paste0(spp.all[i], ".csv")))

  ## palette based on database
  # set database as factor and order appropriately
  spp.now$database <- factor(spp.now$database,
    levels = c("Ex_situ","FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN"))
  spp.now <- spp.now %>% arrange(desc(database))
  # create color palette
  colors <- c("#cf8d5f","#d4a93d","#c1c46c","#73ad2b","#0aa3a6","#2e46c9",
    "#a86abd")
  database.pal <- colorFactor(palette=colors,
    levels=c("Ex_situ","FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN"))

  # create map
    map <- leaflet() %>%
    # Base layer groups
    #addProviderTiles(providers$CartoDB.PositronNoLabels,
    #  group = "CartoDB.PositronNoLabels") %>%
    addProviderTiles(providers$CartoDB.Positron,
      group = "CartoDB.Positron") %>%
    addControl(paste0("<b>",spp.all[i]), position = "topright") %>%
    addControl(
      "Toggle the checkboxes below on/off to view flagged points (colored red) in each category.</br>
      If no points turn red when box is checked, there are no points flagged in that category.</br>
      Click each point for more information about the record.",
      position = "topright") %>%
    # Color by database
    addCircleMarkers(data = spp.now, ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      color = ~database.pal(database),radius = 5,
      fillOpacity = 0.6,stroke = F) %>%
    # Overlay groups (can toggle)
    addCircleMarkers(data = spp.now %>% filter(!.cen),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Within 500m of country/state centroid (.cen)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.urb),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "In urban area (.urb)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.inst),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Within 100m of biodiversity institution (.inst)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.con),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Not in reported country (.con)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.outl),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Geographic outlier (.outl)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.gtsnative),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Outside GTS native country (.gtsnative)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.rlnative),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Outside IUCN RL native country (.rlnative)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.rlintroduced),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "In IUCN RL introduced country (.rlintroduced)") %>%
    addCircleMarkers(data = spp.now %>%
      filter(basisOfRecord == "FOSSIL_SPECIMEN" |
        basisOfRecord == "LIVING_SPECIMEN"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "FOSSIL_SPECIMEN or LIVING_SPECIMEN (basisOfRecord)") %>%
    addCircleMarkers(data = spp.now %>%
      filter(establishmentMeans == "INTRODUCED" |
        establishmentMeans == "MANAGED" |
        establishmentMeans == "INVASIVE"),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "INTRODUCED, MANAGED, or INVASIVE (establishmentMeans)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.yr1950),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Recorded prior to 1950 (.yr1950)") %>%
    addCircleMarkers(data = spp.now %>% filter(!.yr1980),
      ~decimalLongitude, ~decimalLatitude,
      popup = ~paste0(
        "<b>Accepted species name:</b> ",species_name_acc,"<br/>",
        "<b>Verbatim taxon name:</b> ",taxon_name_full,"<br/>",
        "<b>Source database:</b> ",database,"<br/>",
        "<b>All databases with duplicate record:</b> ",all_source_databases,"<br/>",
        "<b>Year:</b> ",year,"<br/>",
        "<b>Basis of record:</b> ",basisOfRecord,"<br/>",
        "<b>Dataset name:</b> ",datasetName,"<br/>",
        "<b>Establishment means:</b> ",establishmentMeans,"<br/>",
        "<b>Coordinate uncertainty:</b> ",coordinateUncertaintyInMeters,"<br/>",
        "<b>ID:</b> ",UID),
      radius=5,stroke=T,color="black",weight=2,fillColor="red",fillOpacity=0.8,
      group = "Recorded prior to 1980 (.yr1980)") %>%
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
                        "In IUCN RL introduced country (.rlintroduced)",
                        "FOSSIL_SPECIMEN or LIVING_SPECIMEN (basisOfRecord)",
                        "INTRODUCED, MANAGED, or INVASIVE (establishmentMeans)",
                        "Recorded prior to 1950 (.yr1950)",
                        "Recorded prior to 1980 (.yr1980)"),
      options = layersControlOptions(collapsed = FALSE)) %>%
    #hideGroup("Within 500m of country/state centroid (.cen)") %>%
    hideGroup("In urban area (.urb)") %>%
    hideGroup("Within 100m of biodiversity institution (.inst)") %>%
    hideGroup("Not in reported country (.con)") %>%
    hideGroup("Geographic outlier (.outl)") %>%
    hideGroup("Outside GTS native country (.gtsnative)") %>%
    hideGroup("Outside IUCN RL native country (.rlnative)") %>%
    hideGroup("In IUCN RL introduced country (.rlintroduced)") %>%
    hideGroup("FOSSIL_SPECIMEN or LIVING_SPECIMEN (basisOfRecord)") %>%
    hideGroup("INTRODUCED, MANAGED, or INVASIVE (establishmentMeans)") %>%
    hideGroup("Recorded prior to 1950 (.yr1950)") %>%
    hideGroup("Recorded prior to 1980 (.yr1980)") %>%
    addLegend(pal = database.pal, values = unique(spp.now$database),
      title = "Source database", position = "bottomright", opacity = 0.6) %>%
    addControl(
      "See https://github.com/MortonArb-CollectionsValue/OccurrencePoints
      for information about data sources and flagging methodology.",
      position = "bottomleft")
  map

  # save map
  htmlwidgets::saveWidget(map, file.path(path.figs,
    paste0(spp.all[i], "_leaflet_map.html")))

  cat("\tEnding ", spp.all[i], ", ", i, " of ", length(spp.all), ".\n\n", sep="")
}

################################################################################
# Basic fixed maps
################################################################################

path.figs <- file.path(imls.output, "spp_basic_maps")
spp.all <- tools::file_path_sans_ext(dir(path.pts, ".csv"))

if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

map.world <- map_data("world")

for(i in 1:length(spp.all)){
  # get data
  spp.now <- spp.all[i]
  dat.now <- read.csv(file.path(path.pts, paste0(spp.all[i], ".csv")))
  dat.now$decimalLatitude <- as.numeric(dat.now$decimalLatitude)
  dat.now$decimalLongitude <- as.numeric(dat.now$decimalLongitude)
  summary(dat.now)

  if(nrow(dat.now[!is.na(dat.now$decimalLatitude),])==0) next

  # map of raw data
  png(file.path(path.figs, paste0(spp.now, "_raw.png")), height=6,
    width=10, units="in", res=180)
  print(
    ggplot() +
      coord_equal() +
      ggtitle(sub("_", " ", spp.now)) +
      geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
      geom_point(data=dat.now, aes(x=decimalLongitude, y=decimalLatitude),
        color="red", size=2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_minimal()
  )
  dev.off()

  # map with all flagged points removed
  dat.now2 <- dat.now %>%
    filter(.cen & .urb & .inst & .con & .outl & .yr1950 & .yr1980 &
      (.gtsnative | is.na(.gtsnative)) &
      (.rlnative  | is.na(.rlnative)) &
      (.rlintroduced | is.na(.rlintroduced)) &
      basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
      establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
      establishmentMeans != "INVASIVE")

  png(file.path(path.figs, paste0(spp.now, "_filtered.png")), height=6,
    width=10, units="in", res=180)
  print(
    ggplot() +
      coord_equal() +
      ggtitle(sub("_", " ", spp.now)) +
      geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
      geom_point(data=dat.now2, aes(x=decimalLongitude, y=decimalLatitude),
        color="green", size=2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_minimal()
  )
  dev.off()

  cat("\tEnding ", spp.now, ", ", i, " of ", length(spp.all), ".\n\n", sep="")
}
