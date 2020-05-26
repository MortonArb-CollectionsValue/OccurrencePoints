# Script to quickly map occurrence points for all species

library(ggplot2); library(maps)

# ---------------------------------------
# Set up file paths etc.
# ---------------------------------------

#source('scripts/set_workingdirectory.R')

imls.output <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs"
path.pts <- file.path(imls.output, "split_by_sp")
path.figs <- file.path(imls.output, "basic_maps_split_by_sp")

if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

map.world <- map_data("world")
# ---------------------------------------



# ---------------------------------------
#
# ---------------------------------------
# spp.test <- c("Quercus georgiana", "Quercus imbricaria", "Quercus arkansana", "Quercus falcata", "Quercus stellata", "Quercus acutissima")

spp.all <- dir(path.pts, ".csv")

for(i in 1:length(spp.all)){
  spp.now <- strsplit(spp.all[i], "[.]")[[1]][1]
  # spp.now <- gsub(spp.n)

  dat.now <- read.csv(file.path(path.pts, spp.all[i]))
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
}
# ---------------------------------------


################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################

library(leaflet); library(RColorBrewer)

# list of test species
spp.test <- c("Quercus_boyntonii","Quercus_dalechampii","Quercus_georgiana",
  "Quercus_imbricaria","Quercus_arkansana","Quercus_falcata","Quercus_stellata",
  "Quercus_acutissima","Quercus_palmeri")

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
        "ID:",unique_id),
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

# run through species and save maps
for(i in 1:length(spp.test)){
  # read file
  dat.now <- read.csv(file.path(imls.output, "split_by_sp",
    paste0(spp.test[1], ".csv")), colClasses = "character")
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
    "interactive_maps_split_by_sp",paste0(spp.test[i],"_leaflet_map.html")))
}
