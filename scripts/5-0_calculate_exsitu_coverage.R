################################################################################

## 5-0_calculate_exsitu_coverage.R
### Author: Emily Beckman ### Date: 05/11/2021

### DESCRIPTION:
  # This script creates circular buffers around in situ points (wild occurrence
	#		records) and ex situ points (wild locations where seeds were collected for
	#		cultivation in botanic gardens or arboreta) to calculate the geographic
	#		(area within buffers) and ecological (number of ecoregions within buffers)
	#		diversity conserved in ex situ living collections. A map showing 50km
	#		buffers around in situ and ex situ points is also created.

### DATA IN:
	## Global country boundaries
	#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via ArcGIS Hub
	# 		Shapefile
	#			https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0
	## In situ occurrence points (latitude and longitude in decimal degrees)
	# 	Can use the output from 3-1_refine_occurrence_points.R
	# 		https://github.com/MortonArb-CollectionsValue/OccurrencePoints/tree/master/scripts
	# 		Each file has data for only one species and is named "Genus_species.csv"
	# 		You can read in data for mult. species in one file but need to edit the
	#			code to split after reading in
	## Ex situ wild localities (latitude and longitude in decimal degrees)
	# 	Can use the output from 3-1_refine_occurrence_points.R, which has a
	#			"database" column that has "Ex_situ" to distinguish the ex situ records
	#			from the rest of the in situ records
	## Global Ecoregions
	# 	Terrestrial Ecoregions of the World, via WWF (Olson et al., 2001)
	#			https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
	## U.S. EPA Level III and Level IV Ecoregions
	#		Level III (us_eco_l3) -> https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip
	#		Level IV (us_eco_l4)-> https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip

### DATA OUT:
  ## Table of geographic and ecological coverage based on three buffer sizes
	#		(10km, 50km, and 100km) and both ecoregion levels (III and IV)
	## Leaflet map with 50 km buffers around in situ and ex situ points, with
	#		ecoregions in background

################################################################################
# Load libraries
################################################################################

## [code chunk from Shannon M. Still]
my.packages <- c("leaflet","raster","sp","rgeos","dplyr","rgdal","Polychrome",
	"RColorBrewer","GeoRange")
#install.packages (my.packages) # turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Consortia/R Training/occurrence_points"
#script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
#source("scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))

# format text in cell for output table
format.cell <- function(ex_result,in_result,final_result){
	cell <- paste0(round(final_result,2),"%","\n",
								 "(",format(round(ex_result,0),format="d",big.mark=",")," / ",
								 format(round(in_result,0),format="d",big.mark=","),")")
	return(cell)
}

# create buffers around points, using specified projection
create.buffers <- function(df,radius,pt_proj,buff_proj){
	# select coordinate columns
	latlong <- df %>% select(decimalLongitude,decimalLatitude)
	# turn occurrence point data into a SpatialPointsDataFrame
	sp_df <- SpatialPointsDataFrame(latlong, df, proj4string = pt_proj)
	# reproject SpatialPointsDataFrame to specified projection
	proj_df <- spTransform(sp_df,buff_proj)
	# place buffer around each point
	buffers <- buffer(proj_df,width=radius,dissolve=T)
	# return buffer polygons
	return(buffers)
}

# create buffers around in situ and ex situ spatial points, calculate areas,
#		then compare to calculate percent coverage
compare.buff.area <- function(insitu,exsitu,radius,pt_proj,buff_proj){
	# create buffers
	buffer_insitu <- create.buffers(insitu,radius,pt_proj,buff_proj)
	buffer_exsitu <- create.buffers(exsitu,radius,pt_proj,buff_proj)
	# calculate buffer area
	print(paste("Based on ",radius/1000," km radius..."))
	area_exsitu <- buffer_exsitu@polygons[[1]]@area/1000000
	print(paste("Area covered by ex situ buffers:", round(area_exsitu,0),"km²"))
	area_insitu <- buffer_insitu@polygons[[1]]@area/1000000
	print(paste("Area covered by in situ buffers:", round(area_insitu,0),"km²"))
	# calculate difference between in situ and ex situ buffer areas (% coverage)
	area_diff_percent <- (area_exsitu/area_insitu)*100
	print(paste0("Percent geographic coverage: ", round(area_diff_percent,2), "%"))
	txt <- format.cell(area_exsitu,area_insitu,area_diff_percent)
	return(txt)
}

# create data frame with ecoregion data extracted for area covered by buffers
intersect.eco.buff <- function(df,radius,pt_proj,buff_proj,eco){
	# create buffers
	buffers <- create.buffers(df,radius,pt_proj,buff_proj)
	# make sure ecoregions are in same projection as buffers
	eco_proj <- spTransform(eco,buff_proj)
	# intersect buffers with ecoregions
	buff_join_eco <- raster::intersect(buffers,eco_proj)
	return(buff_join_eco)
}

# create data frame with ecoregion data extracted for area covered by buffers,
#		for both in situ and ex situ points, then compare count of ecoregions
compare.eco.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create data frame of ecoregion-buffer intersection
	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
	# count number of ecoregions under buffers
	print(paste("Based on ",radius/1000," km radius..."))
	count_exsitu <- nrow(eco_exsitu@data %>% distinct(ECO_ID))
	print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
	count_insitu <- nrow(eco_insitu@data %>% distinct(ECO_ID))
	print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
	txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
	return(txt)
}

# create data frame with Level III ecoregion data extracted for area covered by
#		buffers, for both in situ and ex situ points, then compare ecoregion counts
#compare.ecol3.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
#	# create data frame of ecoregion-buffer intersection
#	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
#	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
#	# count number of ecoregions under buffers
#	print(paste("Based on ",radius/1000," km radius..."))
#	count_exsitu <- nrow(eco_exsitu@data %>% distinct(US_L3CODE))
#	print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
#	count_insitu <- nrow(eco_insitu@data %>% distinct(US_L3CODE))
#	print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
#	# calculate difference in number of ecoregions
#	eco_diff_percent <- (count_exsitu/count_insitu)*100
#	print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
#	txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
#	return(txt)
#}

# create data frame with Level IV ecoregion data extracted for area covered by
#		buffers, for both in situ and ex situ points, then compare ecoregion counts
compare.ecol4.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create data frame of ecoregion-buffer intersection
	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
	# count number of ecoregions under buffers
	print(paste("Based on ",radius/1000," km radius..."))
	count_exsitu <- nrow(eco_exsitu@data %>% distinct(US_L4CODE))
	print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
	count_insitu <- nrow(eco_insitu@data %>% distinct(US_L4CODE))
	print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
	txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
	return(txt)
}

# clip points by boundary so only in target area
# (helpful if focusing on one country/region)
clip.by.boundary <- function(pts,pt_proj,boundary){
	# select coordinate columns
	latlong <- pts %>% select(decimalLongitude,decimalLatitude)
	# turn occurrence point data into a SpatialPointsDataFrame
	spatial_pts <- SpatialPointsDataFrame(latlong, pts, proj4string = pt_proj)
	# clip by boundary created earlier
	spatial_pts <- spatial_pts[boundary, ]
	# keep just the data (not the spatial info you added)
	pts_new <- spatial_pts@data
	return(pts_new)
}

################################################################################
################################################################################
# Set up workspace
################################################################################

# define projection of points
#		WGS84
wgs.proj <- sp::CRS(SRS_string="EPSG:4326")
# define projection for calculations (meters must be the unit)
#		Equal Earth Projection
#		Warning shouldn't cause any errors; metadata says they're "overly cautious"
aea.proj <- sp::CRS(SRS_string="EPSG:8857")

################################################################################
# Choose target species
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list",
  "target_species_with_syn.csv"),
  header = T, na.strings = c("","NA"),colClasses = "character")
head(taxon_list)

# read in manual edits to target species maps
pt_edits <- read.csv(file.path(main_dir, "inputs", "known_distribution",
  "manual_point_edits.csv"),
  header = T, na.strings = c("","NA"),colClasses = "character")
head(pt_edits)

# select threatened (CR, EN, VU) and Near Threatened species
#target_sp <- taxon_list %>% filter(rl_category == "CR" | rl_category == "EN" |
#																	 rl_category == "VU" | rl_category == "NT")
#target_sp <- unique(target_sp$species_name_acc)
# randomly select Least Concern species
# 	out of... 8 Malus ; 83 Quercus ; 16 Tilia ; 15 Ulmus
#lc <- taxon_list %>% filter(is.na(map_flag) & list == "desiderata")
#lc_sp <- lc[sample(nrow(lc), 29), ]
#lc_sp <- lc_sp$species_name_acc
	# add Matt's selection
#lc_sp <- c(lc_sp,"Ulmus bergmaniana")
#sort(lc_sp)
# select target species
target_sp <- taxon_list %>% filter(grepl("^MAP",map_flag))
target_sp <- sort(gsub(" ","_",target_sp$species_name_acc))
length(target_sp)
#target_sp <- target_sp[c(1:2,4:11,13:15,17:19,21:53)]
# get native dist information to see if using RL (default) or GTS
native_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
  "target_taxa_with_native_dist.csv"), header = T, na.strings = c("","NA"),
  colClasses = "character")
native_dist <- native_dist %>% dplyr::select(species_name_acc,rl_native_dist,gts_native_dist)

################################################################################
# Read in polygon data
################################################################################

# read in shapefile of global ecoregions
ecoregions <- readOGR(file.path(main_dir,"inputs","gis_data",
	"official","wwf_terr_ecos.shp"))

# read in shapefile of U.S. EPA Level III ecoregions
#ecol3 <- readOGR(file.path(main_dir, "us_eco_l3/us_eco_l3.shp"))

# read in shapefile of U.S. EPA Level IV ecoregions
ecol4 <- readOGR(file.path(local_dir,
	"us_eco_l4_state_boundaries/us_eco_l4.shp"))

# read in shapefile of country boundaries
world_countries <- readOGR(file.path(local_dir,
	"UIA_World_Countries_Boundaries-shp/World_Countries__Generalized_.shp"))
target_iso <- c("US")
target_countries <- world_countries[world_countries@data$ISO %in% target_iso,]
	## create polygon for clipping points later, one in each projection
target_countries.wgs <- spTransform(target_countries,wgs.proj)
boundary.wgs <- aggregate(target_countries.wgs,dissolve = TRUE)
#target_countries.aea <- spTransform(target_countries,aea.proj)
#boundary.aea <- aggregate(target_countries.aea,dissolve = TRUE)

################################################################################
## Calculate geographic and ecological coverage of ex situ collections
################################################################################

### START SUMMARY TABLE

# we add each target species as we go along
summary_tbl <- data.frame(
	species = "start",
	geo_10 = "start", geo_50 = "start",	geo_100 = "start", geo_500 = "start",
	eco_10 = "start", eco_50 = "start", eco_100 = "start",
	eco_usl4_10 = "start", eco_usl4_50 = "start", eco_usl4_100 = "start",
	EOO = "start",
	dist_filter = "start",
	stringsAsFactors=F)

### CYCLE THROUGH TARGET SPECIES TO CALCULATE EX SITU COVERAGE

for(sp in 1:length(target_sp)){

	# print progress
  cat("\tStarting ", target_sp[sp])

	### READ IN AND PREP POINT DATA

	## read in occurrence points (includes ex situ)
	insitu_raw <- read.csv(file.path(main_dir,"outputs","spp_edited_points",
		paste0(target_sp[sp],".csv")), na.strings=c("","NA"), stringsAsFactors = F)
		nrow(insitu_raw)
	 spp.rl.dist <- native_dist[which(native_dist$species_name_acc == gsub("_"," ",target_sp[sp])),]
	## filter as desired
	 insitu <- insitu_raw %>%
	   filter(database == "Ex_situ" |
	     (.cen & .inst & .con & .outl & #.urb & .yr1950 & .yr1980 & .yrna &
	     #(.gtsnative | is.na(.gtsnative)) &
	     #(.rlnative  | is.na(.rlnative)) &
	     #(.rlintroduced | is.na(.rlintroduced)) &
	     basisOfRecord != "FOSSIL_SPECIMEN" & basisOfRecord != "LIVING_SPECIMEN" &
	     establishmentMeans != "INTRODUCED" & establishmentMeans != "MANAGED" &
	     establishmentMeans != "INVASIVE"))
	  if(!is.na(spp.rl.dist$rl_native_dist)){
	     insitu <- insitu %>%
	     		filter(.rlnative | is.na(.rlnative))
			 dist_filter_val <- "RL"
	  } else if(!is.na(spp.rl.dist$gts_native_dist)){
    	 insitu <- insitu %>%
	     		filter(.gtsnative | is.na(.gtsnative))
			 dist_filter_val <- "GTS"
	  } else {
			 dist_filter_val <- "N/A"
		}
		nrow(insitu)

	## check document with manual point edits to see if anything needs to be added back or removed
	manual.edit <- pt_edits[which(pt_edits$species_name_acc == gsub("_"," ",target_sp[sp])),]
		# bounding box
	if(!is.na(manual.edit$bounding_box)){
		bounds <- unlist(strsplit(manual.edit$bounding_box,"; "))
		for(i in 1:length(bounds)){
			within <- unlist(strsplit(bounds[i],", "))
			insitu <- insitu %>% filter(!(decimalLongitude > as.numeric(within[1]) &
																	  decimalLongitude < as.numeric(within[3]) &
																	 	decimalLatitude > as.numeric(within[2]) &
																		decimalLatitude < as.numeric(within[4])))
		}
	}; nrow(insitu)
		# remove
	if(!is.na(manual.edit$remove)){
		remove <- unlist(strsplit(manual.edit$remove,"; "))
		insitu <- insitu %>% filter(!(UID %in% remove))
	}; nrow(insitu)
		# add back
	if(!is.na(manual.edit$keep)){
		keep <- unlist(strsplit(manual.edit$keep,"; "))
		add <- insitu_raw %>% filter(UID %in% keep)
		insitu <- full_join(insitu,add)
	}; nrow(insitu)

	### CALCULATE EOO (convex hull)

	# uses package GeoRange to calculate area of convex hull in km2
	hull_area <- CHullAreaEarth(insitu$decimalLongitude,insitu$decimalLatitude)
	hull_area <- round(hull_area,0)
	print(paste("EOO:",hull_area,"km²"))

	## create df with just ex situ points
	exsitu <- insitu %>% filter(database == "Ex_situ")
	exsitu <- exsitu %>% filter(datasetName == "MortonArb")
		# check there are ex situ points, if not skip to end (can't do calculations)
	if(nrow(exsitu) == 0){
	  # add text results to summary table
		summary_add <- data.frame(
			species = gsub("_"," ",target_sp[sp]),
			geo_10 = NA, geo_50 = NA,	geo_100 = NA, geo_500 = NA,
			eco_10 = NA, eco_50 = NA, eco_100 = NA,
			eco_usl4_10 = NA, eco_usl4_50 = NA, eco_usl4_100 = NA,
			EOO = hull_area, dist_filter = dist_filter_val,
			stringsAsFactors=F)
		print("No ex situ points; skipping buffer calculations")
	} else {

		### CALCULATE EX SITU COVERAGE

			## Geographic coverage

		# calculate area based on 100 kilometer buffers
		geo_coverage_500 <- compare.buff.area(insitu,exsitu,500000,wgs.proj,aea.proj)
		# calculate area based on 100 kilometer buffers
		geo_coverage_100 <- compare.buff.area(insitu,exsitu,100000,wgs.proj,aea.proj)
		# calculate area based on 50 kilometer buffers
		geo_coverage_50 <- compare.buff.area(insitu,exsitu,50000,wgs.proj,aea.proj)
		# calculate area based on 10 kilometer buffers
		geo_coverage_10 <- compare.buff.area(insitu,exsitu,10000,wgs.proj,aea.proj)

			## Ecological coverage

		## Global ecoregions
		# count ecoregions under 100 km buffers
		eco_coverage_100 <- compare.eco.count(insitu,exsitu,100000,wgs.proj,aea.proj,ecoregions)
		# count ecoregions under 50 km buffers
		eco_coverage_50 <- compare.eco.count(insitu,exsitu,50000,wgs.proj,aea.proj,ecoregions)
		# count ecoregions under 10 km buffers
		eco_coverage_10 <- compare.eco.count(insitu,exsitu,10000,wgs.proj,aea.proj,ecoregions)

		## U.S. Level 4 (most specific) ecoregions
		# get just points that are in the U.S.
		us_insitu <- clip.by.boundary(insitu,wgs.proj,boundary.wgs)
		us_exsitu <- clip.by.boundary(exsitu,wgs.proj,boundary.wgs)
		# if there are in situ and ex situ points in the U.S., then calculate coverage
		if(nrow(us_exsitu) > 0 & nrow(us_insitu) > 0){
			# count ecoregions under 100 km buffers
			ecol4_coverage_100 <- compare.ecol4.count(us_insitu,us_exsitu,100000,wgs.proj,aea.proj,ecol4)
			# count ecoregions under 50 km buffers
			ecol4_coverage_50 <- compare.ecol4.count(us_insitu,us_exsitu,50000,wgs.proj,aea.proj,ecol4)
			# count ecoregions under 10 km buffers
			ecol4_coverage_10 <- compare.ecol4.count(us_insitu,us_exsitu,10000,wgs.proj,aea.proj,ecol4)
		# if there's distribution in the U.S. but no ex situ points, assign 0%
		} else if(nrow(us_exsitu) == 0 & nrow(us_insitu) > 0){
			ecol4_coverage_100 <- "0%"
			ecol4_coverage_50 <- "0%"
			ecol4_coverage_10 <- "0%"
		# if not in U.S. then NA
		} else {
			ecol4_coverage_100 <- NA
			ecol4_coverage_50 <- NA
			ecol4_coverage_10 <- NA
		}

		### SUMMARY TABLE

	  ## Add text results to summary table
	  summary_add <- data.frame(
			species = gsub("_"," ",target_sp[sp]),
			geo_10 = geo_coverage_10,
			geo_50 = geo_coverage_50,
			geo_100 = geo_coverage_100,
			geo_500 = geo_coverage_500,
			eco_10 = eco_coverage_10,
			eco_50 = eco_coverage_50,
			eco_100 = eco_coverage_100,
			eco_usl4_10 = ecol4_coverage_10,
			eco_usl4_50 = ecol4_coverage_50,
			eco_usl4_100 = ecol4_coverage_100,
			EOO = hull_area,
			dist_filter = dist_filter_val,
			stringsAsFactors=F)
	}
	summary_tbl[sp,] <- summary_add
}

## write summary table
summary_tbl
write.csv(summary_tbl, file.path(main_dir,"outputs","ExSituCoverage_BufferTable_6_30_21.csv"),
	row.names = F)


	### MAP

	# create map to visualize buffer and point data
	geo_map <- leaflet() %>%
		## background
		addProviderTiles("Esri.WorldGrayCanvas",
			options = providerTileOptions(maxZoom = 10)) %>%
		## state boundaries
		#addPolygons(data = state_bound,
		#	fillOpacity = 0, color = "#757575", weight = 1.5, opacity = 1) %>%
		## in situ buffers
		#addPolygons(data = create.buffers(insitu,50000,wgs.proj,wgs.proj),
		#	smoothFactor = 0.5,	weight = 6, color = "#de5f5f", fillOpacity = 0.35) %>%
		## in situ points
		addCircleMarkers(data = insitu, lng = ~decimalLongitude, lat = ~decimalLatitude,
			#popup = ~paste("In situ:", Pop),
			radius = 4, fillOpacity = 0.9, stroke = F, color = "#de5f5f") %>%
		## ex situ buffers
		#addPolygons(data = create.buffers(exsitu,50000,wgs.proj,wgs.proj),
		#	smoothFactor = 0.5, weight = 1, color = "#2426bd", fillOpacity = 0.35) %>%
		## ex situ points
		addCircleMarkers(data = exsitu, lng = ~decimalLongitude, lat = ~decimalLatitude,
			#popup = ~paste("Ex situ:", Pop),
			radius = 4, fillOpacity = 0.9, stroke = F, color = "#2426bd") %>%
		## title, legend, scalebar, and zoom
		addControl("Quercus havardii in situ distribution and wild collection sites of ex situ accessions",
			position = "topright") %>%
		addLegend(labels =
			c(paste0("Geographic range (occurrence points and 50 km buffers)"),
				paste0("Populations sampled for ex situ (collection locations and 50 km buffers)")),
			colors = c("#de5f5f","#2426bd"), title = "Legend",
			position = "bottomright", opacity = 0.8) %>%
		addScaleBar(position = "bottomleft",
			options = scaleBarOptions(maxWidth = 150)) %>%
		setView(-106, 36, zoom = 6)

	# view map
	geo_map

	# save map
	htmlwidgets::saveWidget(geo_map, file = "Quercus_havardii_buffer_map_50km.html")


}




# transform ecoregion polygons to WGS84, for mapping
ecol4_wgs <- spTransform(ecol4,wgs.proj)

# select only ecoregions that are within the buffers; otherwise map takes a
#		long time to load in browser and colors are not distinct enough
inter <- intersect.eco.buff(insitu,50000,wgs.proj,wgs.proj,ecol4)
codes <- unique(inter@data$US_L4CODE)
ecol4_sel <- ecol4_wgs[ecol4_wgs@data$US_L4CODE %in% codes,]

# create ecoregion color palette; can run display.brewer.all() to see options
ecol4_pal <- colorFactor(palette = "Set2",domain = ecol4_sel@data$US_L4NAME,
	reverse = F, na.color = "white")

# create map to visualze species distribution and ecoregions
# 	uncomment the section for Level III or Level IV ecoregions, depending
#			on which you're wanting to map
eco_map <- leaflet() %>%
	## background
	addProviderTiles("Esri.WorldGrayCanvas",#"Esri.WorldShadedRelief",#"Esri.WorldTerrain",
		options = providerTileOptions(maxZoom = 10)) %>%
	addPolygons(
	## EPA Level III ecoregions
	#	data = ecol3_sel,
	#	fillColor = ~ecol3_pal(ecol3_sel@data$US_L3NAME), fillOpacity = 0.6,
	#	color = ~ecol3_pal(ecol3_sel@data$US_L3NAME), weight = 1, opacity = 1) %>%
	## EPA Level IV ecoregions
		data = ecol4_sel,
		fillColor = ~ecol4_pal(ecol4_sel@data$US_L4NAME), fillOpacity = 0.6,
		color = ~ecol4_pal(ecol4_sel@data$US_L4NAME), weight = 0.5, opacity = 1) %>%
	## state boundaries
	#addPolygons(data = state_bound,
	#	fillOpacity = 0, color = "#969696", weight = 1.5, opacity = 1) %>%
	## in situ buffers
	addPolygons(data = create.buffers(insitu,50000,wgs.proj,wgs.proj),
		smoothFactor = 0.5,	weight = 2.5, color = "#1c1c1b", fillOpacity = 0.2) %>%
	## in situ points
	addCircleMarkers(data = insitu, lng = ~decimalLongitude, lat = ~decimalLatitude,
		#popup = ~paste("In situ:", Pop),
		radius = 4, fillOpacity = 1, stroke = F, color = "#1c1c1b") %>%
	## ex situ buffers
	addPolygons(data = create.buffers(exsitu,50000,wgs.proj,wgs.proj),
		smoothFactor = 0.5,	weight = 1, opacity = 1, color = "#b0f6f7", fillOpacity = 0.3) %>%
	## ex situ points
	addCircleMarkers(data = exsitu, lng = ~decimalLongitude, lat = ~decimalLatitude,
		#popup = ~paste("In situ:", Pop),
		radius = 4, fillOpacity = 1, stroke = F, color = "#b0f6f7") %>%
	## title, legend, scalebar, and zoom
	#addControl(paste0("Quercus havardii in situ distribution and U.S. EPA Level ",level," Ecoregions"),
	#	position = "topright") %>%
	addLegend(labels =
		c("Geographic range (occurrence points and 50 km buffers)",
			"Populations sampled for ex situ (collection locations and 50 km buffers)",
			"Background colors show U.S. EPA Level IV Ecoregions"),
		colors = c("#1c1c1b","#b0f6f7","#d6ccb4"), title = "Legend",
		position = "bottomright", opacity = 0.8) %>%
	addScaleBar(position = "bottomleft",
		options = scaleBarOptions(maxWidth = 150)) %>%
	setView(-104, 36, zoom = 6)
# view map
eco_map

"Background colors show U.S. EPA Level IV Ecoregions that fall within buffers")),
colors = c("#363636","#d6ccb4"), title = "Legend", position = "bottomright",
opacity = 0.8) %>%






################################################################################
# E) Create summary table of results
################################################################################

summary_tbl <- data.frame(
	Method = c("Geographic","Geographic","Geographic",
						 "Ecological, Level III","Ecological, Level III","Ecological, Level III",
						 "Ecological, Level IV","Ecological, Level IV","Ecological, Level IV"),
	Buffer_km = c("100","50","10",
								"100","50","10",
								"100","50","10"),
	Overall = c(geo_coverage_100,geo_coverage_50,geo_coverage_10,
							eco3_coverage_100,eco3_coverage_50,eco3_coverage_10,
							eco4_coverage_100,eco4_coverage_50,eco4_coverage_10),
	East = c(geo_coverage_100e,geo_coverage_50e,geo_coverage_10e,
					 eco3_coverage_100e,eco3_coverage_50e,eco3_coverage_10e,
					 eco4_coverage_100e,eco4_coverage_50e,eco4_coverage_10e),
	West = c(geo_coverage_100w,geo_coverage_50w,geo_coverage_10w,
					 eco3_coverage_100w,eco3_coverage_50w,eco3_coverage_10w,
					 eco4_coverage_100w,eco4_coverage_50w,eco4_coverage_10w),
	stringsAsFactors=F)
summary_tbl

# write table to csv
write.csv(summary_tbl, file.path(local_dir,
	"Qhavardii_GeoEco_exsitu_SummaryTbl.csv"),row.names = F)

################################################################################
# F) Map points and buffers
################################################################################

# create map to visualize buffer and point data
geo_map <- leaflet() %>%
	## background
	addProviderTiles("Esri.WorldGrayCanvas",
		options = providerTileOptions(maxZoom = 10)) %>%
	## state boundaries
	addPolygons(data = state_bound,
		fillOpacity = 0, color = "#757575", weight = 1.5, opacity = 1) %>%
	## in situ buffers
	addPolygons(data = create.buffers(insitu,50000,wgs.proj,wgs.proj),
		smoothFactor = 0.5,	weight = 6, color = "#de5f5f", fillOpacity = 0.35) %>%
	## in situ points
	addCircleMarkers(data = insitu, lng = ~longitude, lat = ~latitude,
		popup = ~paste("In situ:", Pop),
		radius = 4, fillOpacity = 0.9, stroke = F, color = "#de5f5f") %>%
	## ex situ buffers
	addPolygons(data = create.buffers(exsitu,50000,wgs.proj,wgs.proj),
		smoothFactor = 0.5, weight = 1, color = "#2426bd", fillOpacity = 0.35) %>%
	## ex situ points
	addCircleMarkers(data = exsitu, lng = ~longitude, lat = ~latitude,
		popup = ~paste("Ex situ:", Pop),
		radius = 4, fillOpacity = 0.9, stroke = F, color = "#2426bd") %>%
	## title, legend, scalebar, and zoom
	addControl("Quercus havardii in situ distribution and wild collection sites of ex situ accessions",
		position = "topright") %>%
	addLegend(labels =
		c(paste0("Geographic range (occurrence points and 50 km buffers)"),
			paste0("Populations sampled for ex situ (collection locations and 50 km buffers)")),
		colors = c("#de5f5f","#2426bd"), title = "Legend",
		position = "bottomright", opacity = 0.8) %>%
	addScaleBar(position = "bottomleft",
		options = scaleBarOptions(maxWidth = 150)) %>%
	setView(-106, 36, zoom = 6)

# view map
geo_map

# save map
htmlwidgets::saveWidget(geo_map, file = "Quercus_havardii_buffer_map_50km.html")

################################################################################
# G) Map ecoregions
################################################################################

# transform ecoregion polygons to WGS84, for mapping
ecol4_wgs <- spTransform(ecol4,wgs.proj)

# select only ecoregions that are within the buffers; otherwise map takes a
#		long time to load in browser and colors are not distinct enough
inter <- intersect.eco.buff(insitu,50000,wgs.proj,wgs.proj,ecol3)
codes <- unique(inter@data$US_L3CODE)
ecol3_sel <- ecol3_wgs[ecol3_wgs@data$US_L3CODE %in% codes,]
inter <- intersect.eco.buff(insitu,50000,wgs.proj,wgs.proj,ecol4)
codes <- unique(inter@data$US_L4CODE)
ecol4_sel <- ecol4_wgs[ecol4_wgs@data$US_L4CODE %in% codes,]

# create ecoregion color palette; can run display.brewer.all() to see options
ecol3_pal <- colorFactor(palette = "Set2",domain = ecol3_sel@data$US_L3NAME,
	reverse = F, na.color = "white")
ecol4_pal <- colorFactor(palette = "Set2",domain = ecol4_sel@data$US_L4NAME,
	reverse = F, na.color = "white")

# comment out/uncomment based on which level you're mapping
level <- "III"
#level <- "IV"

# create map to visualze species distribution and ecoregions
# 	uncomment the section for Level III or Level IV ecoregions, depending
#			on which you're wanting to map
eco_map <- leaflet() %>%
	## background
	addProviderTiles("Esri.WorldGrayCanvas",#"Esri.WorldShadedRelief",#"Esri.WorldTerrain",
		options = providerTileOptions(maxZoom = 10)) %>%
	addPolygons(
	## EPA Level III ecoregions
		data = ecol3_sel,
		fillColor = ~ecol3_pal(ecol3_sel@data$US_L3NAME), fillOpacity = 0.6,
		color = ~ecol3_pal(ecol3_sel@data$US_L3NAME), weight = 1, opacity = 1) %>%
	## EPA Level IV ecoregions
		#data = ecol4_sel,
		#fillColor = ~ecol4_pal(ecol4_sel@data$US_L4NAME), fillOpacity = 0.6,
		#color = ~ecol4_pal(ecol4_sel@data$US_L4NAME), weight = 0.5, opacity = 1) %>%
	## state boundaries
	addPolygons(data = state_bound,
		fillOpacity = 0, color = "#969696", weight = 1.5, opacity = 1) %>%
	## in situ buffers
	addPolygons(data = create.buffers(insitu,50000,wgs.proj,wgs.proj),
		smoothFactor = 0.5,	weight = 2, color = "#363636", fillOpacity = 0.2) %>%
	## in situ points
	addCircleMarkers(data = insitu, lng = ~longitude, lat = ~latitude,
		popup = ~paste("In situ:", Pop),
		radius = 4, fillOpacity = 0.9, stroke = F, color = "#363636") %>%
	## title, legend, scalebar, and zoom
	addControl(paste0("Quercus havardii in situ distribution and U.S. EPA Level ",level," Ecoregions"),
		position = "topright") %>%
	addLegend(labels =
		c("Geographic range of Quercus havardii (occurrence points and 50 km buffers)",
			paste0("Background colors show U.S. EPA Level ",level," Ecoregions that fall within buffers")),
		colors = c("#363636","#d6ccb4"), title = "Legend", position = "bottomright",
		opacity = 0.8) %>%
	addScaleBar(position = "bottomleft",
		options = scaleBarOptions(maxWidth = 150)) %>%
	setView(-104, 36, zoom = 6)

# view map
eco_map

# save map
htmlwidgets::saveWidget(eco_map, file = "Quercus_havardii_ecoregions_L3.html")
	# level IV is too big so can't save
#htmlwidgets::saveWidget(eco_map, file = "Quercus_havardii_ecoregions_L4.html")
