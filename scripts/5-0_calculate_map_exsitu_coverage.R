################################################################################

## 5-0_calculate_map_exsitu_coverage.R
### Authors: Emily Beckman Bruns
### Date: September 23, 2022

### DESCRIPTION:
  # This script creates circular buffers around in situ points (wild occurrence
	#		records) and ex situ points (wild locations where seeds were collected for
	#		cultivation in botanic gardens or arboreta) to calculate the geographic
	#		(area within buffers) and ecological (number of ecoregions within buffers)
	#		diversity conserved in ex situ living collections.
  # An interactive map is also created to view buffers and ecoregions.

### DATA IN:
	## Global country boundaries
	#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via
	#			ArcGIS Hub Shapefile
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
	#		(10km, 50km, 100km) and three ecoregion levels (US III, US IV, global)

################################################################################
# Load libraries
################################################################################

my.packages <- c("dplyr","leaflet","terra"#,"sf"
  #"ggplot2", "maps",  "RColorBrewer",
)
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#main_dir <- "/Volumes/GoogleDrive-103729429307302508433/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis/3. In situ/occurrence_points"

# or use 0-1_set_workingdirectory.R script:
source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################

# format text in cell for output table
format.cell <- function(ex_result,in_result,final_result){
	cell <- paste0(round(final_result,2),"%","\n",
								 "(",format(round(ex_result,0),format="d",big.mark=",")," / ",
								 format(round(in_result,0),format="d",big.mark=","),")")
	return(cell)
}

# create buffers around points, using specified projection
create.buffers <- function(df,radius,pt_proj,buff_proj){
	# turn occurrence point data into a SpatVector
  spat_pts <- vect(df, geom=c("decimalLongitude", "decimalLatitude"),
    crs=pt_proj)
	# reproject to specified projection
	proj_df <- project(spat_pts,buff_proj)
	# place buffer around each point, then dissolve into one polygon
	buffers <- buffer(proj_df,width=radius)
  buffers <- aggregate(buffers,dissolve = TRUE)
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
  area_exsitu <- expanse(buffer_exsitu)/1000000
	print(paste("Area covered by ex situ buffers:", round(area_exsitu,0),"km²"))
	area_insitu <- expanse(buffer_insitu)/1000000
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
	eco_proj <- project(eco,buff_proj)
	# intersect buffers with ecoregions
	buff_join_eco <- intersect(buffers,eco_proj)
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
	count_exsitu <- length(unique(eco_exsitu$ECO_ID))
	print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
	count_insitu <- length(unique(eco_insitu$ECO_ID))
	print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
	txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
	return(txt)
}

# create data frame with Level III ecoregion data extracted for area covered by
#		buffers, for both in situ and ex situ points, then compare ecoregion counts
compare.ecol3.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create data frame of ecoregion-buffer intersection
	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
	# count number of ecoregions under buffers
	print(paste("Based on ",radius/1000," km radius..."))
	count_exsitu <- length(unique(eco_exsitu$US_L3CODE))
	print(paste0("Number of ecoregions under ex situ buffers: ",count_exsitu))
	count_insitu <- length(unique(eco_insitu$US_L3CODE))
	print(paste0("Number of ecoregions under in situ buffers: ",count_insitu))
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	print(paste0("Percent ecological coverage: ", round(eco_diff_percent,2), "%"))
	txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
	return(txt)
}

# create data frame with Level IV ecoregion data extracted for area covered by
#		buffers, for both in situ and ex situ points, then compare ecoregion counts
compare.ecol4.count <- function(insitu,exsitu,radius,pt_proj,buff_proj,eco){
	# create data frame of ecoregion-buffer intersection
	eco_insitu <- intersect.eco.buff(insitu,radius,pt_proj,buff_proj,eco)
	eco_exsitu <- intersect.eco.buff(exsitu,radius,pt_proj,buff_proj,eco)
	# count number of ecoregions under buffers
	print(paste("Based on ",radius/1000," km radius..."))
	count_exsitu <- length(unique(eco_exsitu$US_L4CODE))
	print(paste0("Number of US ecoregions under ex situ buffers: ",count_exsitu))
	count_insitu <- length(unique(eco_insitu$US_L4CODE))
	print(paste0("Number of US ecoregions under in situ buffers: ",count_insitu))
	# calculate difference in number of ecoregions
	eco_diff_percent <- (count_exsitu/count_insitu)*100
	print(paste0("Percent US ecological coverage: ", round(eco_diff_percent,2), "%"))
	txt <- format.cell(count_exsitu,count_insitu,eco_diff_percent)
	return(txt)
}

# clip points by boundary so only in target area
# (helpful if focusing on one country/region)
clip.by.boundary <- function(pts,pt_proj,boundary){
	# turn occurrence point data into a SpatVector
  spat_pts <- vect(pts, geom=c("decimalLongitude", "decimalLatitude"),
    crs=pt_proj)
	# clip by boundary created earlier
	spat_pts <- crop(spat_pts,boundary)
	# keep just the data (not the spatial info you added)
	pts_new <- as.data.frame(spat_pts)
	return(pts_new)
}

################################################################################
################################################################################
# Set up workspace
################################################################################

# define projections we'll use throughout
#		points will be WGS84
pt.proj <- "+proj=longlat +datum=WGS84"
#   for calculations we need something in meters, like Equal Earth Projection
calc.proj <- "+proj=eqearth +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

################################################################################
# Choose buffer sizes to use
################################################################################

# buffer size in kilometers = value/1000
large_buff <- 100000
med_buff <- 50000
small_buff <- 20000

################################################################################
# Choose target species
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list",
  "target_taxa_with_syn.csv"),
  header = T, na.strings = c("","NA"),colClasses = "character")
head(taxon_list)

# OPTIONAL, depending on your workflow:
#		read in manual edits to target species maps
pt_edits <- read.csv(file.path(main_dir, "inputs", "known_distribution",
  "manual_point_edits.csv"),
  header = T, na.strings = c("","NA"),colClasses = "character")
head(pt_edits)

# select target species by...
# ...OPTION 1 - select threatened (CR, EN, VU) and Near Threatened species
#target_sp <- taxon_list %>% filter(rl_category == "CR" | rl_category == "EN" |
#																	 rl_category == "VU" | rl_category == "NT")
#target_sp <- unique(target_sp$species_name_acc)
# ...OPTION 2 - randomly select some Least Concern species
#lc <- taxon_list %>% filter(is.na(map_flag) & list == "desiderata")
#lc_sp <- lc[sample(nrow(lc), 29), ]
#lc_sp <- lc_sp$species_name_acc
#sort(lc_sp)
# ...OPTION 3 - manually select target species
target_sp <- c("Quercus_lobata")
               #"Quercus_acutifolia", "Quercus_costaricensis",
               #"Quercus_hirtifolia", "Quercus_mulleri")
# ...OPTION 4 - use all the species in your list!
# target_sp <- unique(taxon_list$species_name_acc)

# read in native dist information to see if using RL (default) or GTS
native_dist <- read.csv(file.path(main_dir,"inputs","known_distribution",
  "target_taxa_with_native_dist.csv"), header = T, na.strings = c("","NA"),
  colClasses = "character")
native_dist <- native_dist %>% dplyr::select(species_name_acc,rl_native_dist,
	gts_native_dist)

################################################################################
# Read in and prep polygon data
################################################################################

# read in shapefile of global ecoregions
ecoregions <- vect(file.path(main_dir,"inputs","gis_data",
	"official","wwf_terr_ecos.shp"))

# read in shapefile of U.S. EPA Level III ecoregions
ecol3 <- vect(file.path(main_dir,"inputs","gis_data",
	"us_eco_l3/us_eco_l3.shp"))

# read in shapefile of U.S. EPA Level IV ecoregions
ecol4 <- vect(file.path(main_dir,"inputs","gis_data",
	"us_eco_l4_state_boundaries/us_eco_l4.shp"))

# read in shapefile of country boundaries
world_countries <- vect(file.path(main_dir,"inputs","gis_data",
	"UIA_World_Countries_Boundaries/World_Countries__Generalized_.shp"))
# select target countries (countries with species you're mapping)
target_iso <- c("US","MX","BZ","GT","HN","PA","CR","NI","SV")
target_countries <- subset(world_countries,
  world_countries$ISO %in% target_iso,)

# create polygon for clipping points later (project to pt projection)
target_countries.pt <- project(target_countries,pt.proj)
boundary.pt <- aggregate(target_countries.pt,dissolve = TRUE)

################################################################################
## Calculate geographic and ecological coverage of ex situ collections
################################################################################

### START SUMMARY TABLE

# we add each target species as we go along
summary_tbl <- data.frame(
	species = "start",
	geo_sm = "start", geo_md = "start",	geo_lg = "start",
	eco_sm = "start", eco_md = "start", eco_lg = "start",
	eco_usl4_sm = "start", eco_usl4_md = "start", eco_usl4_lg = "start",
	EOO = "start",
	dist_filter = "start",
	#pa_coverage = "start",
	stringsAsFactors=F)

### CYCLE THROUGH TARGET SPECIES TO CALCULATE EX SITU COVERAGE

for(sp in 1:length(target_sp)){

## can test with one species first if you'd like (skip loop line above)
sp <- 1

	# print progress
  cat("\tStarting ", target_sp[sp], "\n")

	### READ IN AND PREP POINT DATA

	## read in occurrence points (includes ex situ)
	insitu_raw <- read.csv(file.path(main_dir,"outputs","spp_edited_points",
		paste0(target_sp[sp],".csv")), na.strings=c("","NA"), stringsAsFactors = F)
		nrow(insitu_raw)
	 spp.rl.dist <- native_dist[which(
     native_dist$species_name_acc == gsub("_"," ",target_sp[sp])),]
	## filter as desired
	 insitu <- insitu_raw %>%
	   filter(database == "Ex_situ" |
          # select or deselect these filters as desired:
	     (.cen & .inst & .con & .outl &
			 #.urb & .yr1950 & .yr1980 & .yrna &
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

	## check document with manual point edits to see if anything needs to be
  #   added back or removed
  if(exists("pt_edits")){
	   manual.edit <- pt_edits[which(
       pt_edits$species_name_acc == gsub("_"," ",target_sp[sp])),]
		     # bounding box
    if(!is.na(manual.edit$bounding_box)){
		     bounds <- unlist(strsplit(manual.edit$bounding_box,"; "))
		     for(i in 1:length(bounds)){
			        within <- unlist(strsplit(bounds[i],", "))
			        insitu <- insitu %>%
              filter(!(decimalLongitude > as.numeric(within[1]) &
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
		    insitu <- suppressMessages(full_join(insitu,add))
	  }; nrow(insitu)
  }

	### CALCULATE EOO (convex hull)

  # make points into vector spatial object
  spat_pts <- vect(insitu, geom=c("decimalLongitude", "decimalLatitude"),
    crs=pt.proj, keepgeom=FALSE)
  spat_pts.calc <- project(spat_pts,calc.proj)
  # calculate area of convex hull in km2 (called EOO on the IUCN Red List)
	hull_insitu <- convHull(spat_pts.calc)
	hull_area <- expanse(hull_insitu)/1000000
	print(paste("EOO:",hull_area,"km²"))

	### CALCULATE PROTECTED AREA COVERAGE

  # !! needs to be added/edited from another script

  ### CREATE DATA SUBSET WITH EX SITU ONLY

	# create df with just ex situ points
	exsitu <- insitu %>% filter(database == "Ex_situ")
	# print number of individuals ex situ with lat-long data
	print(paste("Number of ex situ individuals:",
    sum(as.numeric(exsitu$establishmentMeans))))

		# check there are ex situ points, if not skip to end (can't do calculations)
	if(nrow(exsitu) == 0){
	  # add text results to summary table
		summary_add <- data.frame(
			species = gsub("_"," ",target_sp[sp]),
			geo_sm = NA, geo_md = NA,	geo_lg = NA, geo_vlg = NA,
			eco_sm = NA, eco_md = NA, eco_lg = NA,
			eco_usl4_sm = NA, eco_usl4_md = NA, eco_usl4_lg = NA,
			EOO = hull_area,
				### turn the next row off for gap analysis of US oaks 2020
			#dist_filter = dist_filter_val,
			stringsAsFactors=F)
		print("No ex situ points; skipping buffer calculations")
	} else {

	### CALCULATE EX SITU COVERAGE

			## Geographic Coverage

		# calculate area based on large buffers
		geo_coverage_lg <- compare.buff.area(insitu,exsitu,large_buff,pt.proj,calc.proj)
		# calculate area based on medium buffers
		geo_coverage_md <- compare.buff.area(insitu,exsitu,med_buff,pt.proj,calc.proj)
		# calculate area based on small buffers
		geo_coverage_sm <- compare.buff.area(insitu,exsitu,small_buff,pt.proj,calc.proj)

			## Ecological Coverage

		## Global ecoregions
		# count ecoregions under large buffers
		eco_coverage_lg <- compare.eco.count(insitu,exsitu,large_buff,pt.proj,calc.proj,ecoregions)
		# count ecoregions under medium buffers
		eco_coverage_md <- compare.eco.count(insitu,exsitu,med_buff,pt.proj,calc.proj,ecoregions)
		# count ecoregions under small buffers
		eco_coverage_sm <- compare.eco.count(insitu,exsitu,small_buff,pt.proj,calc.proj,ecoregions)

		## U.S. Level 4 (most specific) ecoregions
		# get just points that are in the U.S.
		us_insitu <- clip.by.boundary(insitu,pt.proj,boundary.pt)
		us_exsitu <- clip.by.boundary(exsitu,pt.proj,boundary.pt)
		# if there are in situ and ex situ points in the U.S., then calculate coverage
		if(nrow(us_exsitu) > 0 & nrow(us_insitu) > 0){
			# count ecoregions under large buffers
			ecol4_coverage_lg <- compare.ecol4.count(insitu,exsitu,large_buff,pt.proj,calc.proj,ecol4)
			# count ecoregions under medium buffers
			ecol4_coverage_md <- compare.ecol4.count(insitu,exsitu,med_buff,pt.proj,calc.proj,ecol4)
			# count ecoregions under small buffers
			ecol4_coverage_sm <- compare.ecol4.count(insitu,exsitu,small_buff,pt.proj,calc.proj,ecol4)
		# if there's distribution in the U.S. but no ex situ points, assign 0%
		} else if(nrow(us_exsitu) == 0 & nrow(us_insitu) > 0){
			ecol4_coverage_lg <- "0%"
			ecol4_coverage_md <- "0%"
			ecol4_coverage_sm <- "0%"
		# if not in U.S. then NA
		} else {
			ecol4_coverage_lg <- NA
			ecol4_coverage_md <- NA
			ecol4_coverage_sm <- NA
		}

		  ## Summary Table

	  ## Add text results to summary table
	  summary_add <- data.frame(
			species = gsub("_"," ",target_sp[sp]),
			geo_sm = geo_coverage_sm,
			geo_md = geo_coverage_md,
			geo_lg = geo_coverage_lg,
			eco_sm = eco_coverage_sm,
			eco_md = eco_coverage_md,
			eco_lg = eco_coverage_lg,
			eco_usl4_sm = ecol4_coverage_sm,
			eco_usl4_md = ecol4_coverage_md,
			eco_usl4_lg = ecol4_coverage_lg,
			EOO = round(hull_area,2),
      dist_filter = dist_filter_val,
			stringsAsFactors=F)
	}
	summary_tbl[sp,] <- summary_add
}

## write summary table
summary_tbl
write.csv(summary_tbl, file.path(main_dir,"outputs","exsitu_coverage",
  paste0("ExSitu_Coverage_Table_", Sys.Date(), ".csv")),row.names = F)

	### for gap analysis of US oaks 2020
#write.csv(summary_tbl, file.path(main_dir,
#	"Oaks2020_BufferTable.csv"), row.names = F)
