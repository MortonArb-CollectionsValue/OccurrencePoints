################################################################################

## 1-1_prepare_gis_data.R
### Authors: Shannon M. Still & Emily Beckman ### Date: 08/04/2020

### DESCRIPTION:
  # Adds GlobalTreeSearch (GTS) and IUCN Red List (RL) country-level
  #   distribution data to target taxa list and preps country (adm0),
  #   state/province (adm1), and county (adm2) polygons for later use.

### DATA IN:
  # target_taxa_with_syn.csv
  # CSV of country-level distribution for each target genus, downloaded
  #   from GlobalTreeSearch (https://tools.bgci.org/global_tree_search.php)
  #   and placed in "inputs/known_distribution" folder

### DATA OUT:
  # List of target taxa with native country distribution from GTS and IUCN RL
  #   added (target_taxa_with_native_dist.csv); RL also has some introduced
  #   country distribution data that is added
  # RData file with country and state polygon data from 'rnaturalearthhires'
  #   package and U.S. county polygon data from census.gov
  #   (admin_shapefiles.RData)
  # Files for looking/working over the data
    # geo_work0.xlsx
    # geo_work1.xlsx
    # geo_work2.xlsx

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("raster", "sp", "tools", "spatialEco", "rgdal", "geosphere",
  "readxl", "writexl", "dplyr", "tidyr", "tidyverse", "housingData",
  "data.table", "textclean", "CoordinateCleaner", "countrycode", "usmap",
  "rnaturalearth", "rnaturalearthdata", "maps","rredlist")#, "sf"
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#main_dir <- "./Desktop"
#script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
# source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
source("scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))


################################################################################
################################################################################
# 1. Create native country list for each taxon using GlobalTreeSearch and
#   IUCN Red List
################################################################################

# read in taxa list
taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
  "target_taxa_with_syn.csv"), header = T, na.strings=c("","NA"),
  colClasses="character")
# keep only taxa with accepted species name
taxon_list <- taxon_list %>% filter(!is.na(species_name_acc))
  nrow(taxon_list) #636

# create new folder if not already present
if(!dir.exists(file.path(main_dir,"inputs","known_distribution")))
  dir.create(file.path(main_dir,"inputs","known_distribution"),
  recursive=T)

### GlobalTreeSearch (GTS)

# First, download raw data
  # Go to https://tools.bgci.org/global_tree_search.php
  # Type your target genus name into the "Genus" box
  # Click "Search Plants" then scroll to the bottom and click "download as CSV
  #   file"
  # If you have more than one target genus, repeat the above steps for the
  #   other genera
  # Move all downloads to "occurrence_points/inputs/known_distribution" folder
# read in and compile GlobalTreeSearch data
file_list <- list.files(path = file.path(main_dir,"inputs","known_distribution"),
  pattern = "globaltreesearch_results", full.names = T)
file_dfs <- lapply(file_list, read.csv, colClasses = "character",
  na.strings=c("","NA"),strip.white=T)
gts_list <- data.frame()
  for(file in seq_along(file_dfs)){
    gts_list <- rbind(gts_list, file_dfs[[file]])
  }
head(gts_list)
  # split countries by delimiter
gts_all <- gts_list %>%
  rename(taxon_name = taxon) %>%
  mutate(native_distribution =
    strsplit(as.character(native_distribution), "; ")) %>%
  unnest(native_distribution) %>% mutate(native_distribution =
    str_trim(native_distribution, side="both"))
# write out all GTS countries to check
#spp_countries <- as.data.frame(sort(unique(str_trim(
#  gts_all$native_distribution, side = c("both")))))
#write_xlsx(spp_countries, path=file.path(main_dir,"inputs",
#  "known_distribution","globaltreesearch_countries.xlsx"))

# use countrycode package to translate country codes from the country names
country_set <- as.data.frame(sort(unique(gts_all$native_distribution))) %>%
  add_column(iso3c = countrycode(sort(unique(gts_all$native_distribution)),
      origin="country.name", destination="iso3c")) %>%
  add_column(iso2c = countrycode(sort(unique(gts_all$native_distribution)),
      origin="country.name", destination="iso2c")) %>%
  add_column(iso3n = countrycode(sort(unique(gts_all$native_distribution)),
      origin="country.name", destination="iso3n")) %>%
  add_column(fips = countrycode(sort(unique(gts_all$native_distribution)),
      origin="country.name", destination="fips"))
names(country_set)[1] <- "country_name"
# add country codes to GTS native distribution data
names(gts_list)[5] <- "gts_native_dist"
gts_list$gts_native_dist_iso2c <- gts_list$gts_native_dist
gts_list$gts_native_dist_iso2c <- mgsub(gts_list$gts_native_dist_iso2c,
  array(as.character(country_set$country_name)),
  array(as.character(country_set$iso2c)))
gts_list <- gts_list %>% dplyr::select(-comment)
head(gts_list)
# save the country codes for ISO2, ISO3, and numeric and character
#   codes, FIPS code
#  write_xlsx(country_set, path=file.path(main_dir,"inputs","gis_data",
#      "imls_global_admin_areas.xlsx"))
#gadm <- country_set; rm(country_set)

# add country codes to the taxon list by matching to GTS
taxon_list <- left_join(taxon_list, gts_list[,c(2,4,5)],
  by=c("species_name_acc" = "taxon"))

### IUCN Red List (RL)

# use rredlist package to get taxon names and country-level spp. dist.
# can take a little while if lots of species
countries <- data.frame()
target_taxa <- taxon_list[,1]
for(i in 1:length(target_taxa)){
	dist <- rl_occ_country(target_taxa[[i]])
	name <- dist$name
	dist <- as.data.frame(dist$result)
	if(nrow(dist>0)){
    print(target_taxa[[i]])
		dist$genus_species <- rep(name)
		countries <- rbind(countries,dist)
	} else {
		print(paste(target_taxa[[i]],"not found"))
	}
}
# condense output so its one entry per species
countries_c <- countries %>%
  filter(presence != "Extinct Post-1500" &
         distribution_code != "Regionally Extinct") %>%
  group_by(genus_species,origin) %>%
  mutate(
    rl_native_dist_iso2c = paste(code, collapse = '; '),
    rl_native_dist = paste(country, collapse = '; ')) %>%
  ungroup() %>%
  dplyr::select(genus_species,origin,rl_native_dist_iso2c,rl_native_dist) %>%
  distinct(genus_species,origin,.keep_all=T)
# separate native dist countries from introduced dist countries
rl_native <- countries_c %>% filter(origin == "Native")
rl_introduced <- countries_c %>% filter(origin != "Native")
names(rl_introduced)[3] <- "rl_introduced_dist_iso2c"
names(rl_introduced)[4] <- "rl_introduced_dist"

# add country codes to the taxon list by matching to RL data
taxon_list <- left_join(taxon_list, rl_native[,c(1,4,3)],
  by=c("species_name_acc" = "genus_species"))
taxon_list <- left_join(taxon_list, rl_introduced[,c(1,4,3)],
  by=c("species_name_acc" = "genus_species"))

native_dist <- taxon_list %>%
  dplyr::select(species_name_acc,gts_native_dist,
  gts_native_dist_iso2c,rl_native_dist,rl_native_dist_iso2c,
  rl_introduced_dist,rl_introduced_dist_iso2c) %>%
  distinct(species_name_acc,gts_native_dist,
  gts_native_dist_iso2c,rl_native_dist,rl_native_dist_iso2c,
  rl_introduced_dist,rl_introduced_dist_iso2c)

# see which target species are missing GTS or RL data
native_dist[is.na(native_dist$gts_native_dist),]$species_name_acc
native_dist[is.na(native_dist$rl_native_dist),]$species_name_acc

# write taxon list with GTS and RL distribution information
write.csv(native_dist, file.path(main_dir,"inputs","known_distribution",
    "target_taxa_with_native_dist.csv"),row.names=F)

################################################################################
# 2. Download polygon data for countries, states, counties
################################################################################

# create folder for gis data
if(!dir.exists(file.path(main_dir,"inputs","gis_data")))
  dir.create(file.path(main_dir,"inputs","gis_data"), recursive=T)

## bring in polygons for world regions and US (down to county level);
##  could use maps::county() function instead
##  but will use the rnaturalearthhires package for polygons at adm0/adm1 levels
##  will use a provided shapefile for the adm2 level (county)
# may need to run this line first, to get rnaturalearthhires package working:
#devtools::install_github("ropensci/rnaturalearthhires")

# COUNTRIES
  # download
adm0.poly <- ne_countries(type = "countries", scale = "large")
  # ne_countries had some iso2 and iso3 values missing; filling them
#adm0.poly@data[is.na(adm0.poly@data$iso_a3),]
#adm0.poly@data[is.na(adm0.poly@data$iso_a2),]
add_codes <- data.frame(
  admin=c("Dhekelia Sovereign Base Area","Somaliland","France",
  "Norway","Kosovo","US Naval Base Guantanamo Bay","Northern Cyprus",
  "Cyprus No Mans Area","Siachen Glacier","Baykonur Cosmodrome",
  "Akrotiri Sovereign Base Area","Indian Ocean Territories",
  "Coral Sea Islands","Clipperton Island",
  "Ashmore and Cartier Islands","Bajo Nuevo Bank (Petrel Is.)",
  "Serranilla Bank"),
  iso_a3=c("GBR","SOM","FRA","NOR","SRB","USA","CYP","CYP","IND","KAZ",
  "GBR","AUS","AUS","FRA","AUS","AUS","COL"),
  iso_a2=c("GB","SO","FR","NO","RS","US","CY","CY","IN","KZ","GB","AU",
  "AU","FR","AU","AU","CO"),stringsAsFactors=F)
for(i in 1:nrow(add_codes)){
  adm0.poly@data[adm0.poly@data$admin==add_codes$admin[i],]$iso_a3 <- add_codes$iso_a3[[i]]
  adm0.poly@data[adm0.poly@data$admin==add_codes$admin[i],]$iso_a2 <- add_codes$iso_a2[[i]]
}
  # select only necessary columns and rename with qualifier at beginning
adm0.poly <- adm0.poly[,c(18,46,47,60)] #8,25,31,62,77
names(adm0.poly@data) <- c("country.name","country.iso_a2","country.iso_a3",
  "country.continent")
  # write file
write_xlsx(adm0.poly@data, path=file.path(main_dir,"inputs","gis_data",
  "geo_work0.xlsx"))
#plot(adm0.poly)

# STATES
  # download
adm1.poly <- ne_states(country=NULL)
  # select only necessary columns and rename with qualifier at beginning
adm1.poly <- adm1.poly[,c(7,9,10,12,25,46,56,65)]
names(adm1.poly@data) <- c("state.iso_a2","state.name","state.name_alt",
  "state.type","state.postal","state.gn_name","state.gns_adm1","state.name_en")
  # write file
write_xlsx(adm1.poly@data, path=file.path(main_dir,"inputs","gis_data",
  "geo_work1.xlsx"))
#plot(adm1.poly)

# U.S. COUNTIES
  # download
download.file("https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_county_5m.zip",
  destfile=file.path(main_dir,"inputs","gis_data","USA_adm2.zip"))
unzip(zipfile=file.path(main_dir,"inputs","gis_data","USA_adm2.zip"),
  exdir=file.path(main_dir,"inputs","gis_data","USA_adm2"))
  # delete unzipped folder
unlink(paste0(file.path(main_dir,"inputs","gis_data","USA_adm2.zip")))
  # read in file
adm2.poly <- readOGR(dsn=file.path(main_dir,"inputs","gis_data","USA_adm2"),
  "cb_2018_us_county_5m")
  # select only necessary columns
adm2.poly <- adm2.poly[,c(1:3,6)]
  # write file
write_xlsx(adm2.poly@data, path=file.path(main_dir,"inputs","gis_data",
  "geo_work2.xlsx"))
#plot(adm2.poly)

################################################################################
# 3. Download polygon data for urban areas
################################################################################

# URBAN AREAS
  # download; "large" takes longer to process data later but is more exact?
  #   choose "medium" if you want processing to go faster
urban.poly <- rnaturalearth::ne_download(scale = "large", type = "urban_areas")
#urban.poly <- rnaturalearth::ne_download(scale = "medium", type = "urban_areas")

################################################################################
# 4. Add centroids (skipping for now)
################################################################################

## calculate centroid of each polygon
#  ##first countries (adm0)
#  adm0.poly@data <- adm0.poly@data %>% mutate(long_centroid =
#    centroid(adm0.poly)[,1],
#      lat_centroid=centroid(adm0.poly)[,2], UID=as.character(paste0("adm0_",
#        sprintf("%04d", 1:nrow(adm0.poly@data))))) %>% rename(iso3c=adm0_a3_us,
#          adm0_type=type, iso2c=iso_a2, iso3n=iso_n3, NAME_0=admin)
#    adm0 <- adm0.poly@data
#  ##second states (adm1)
#  adm1.poly@data <- adm1.poly@data %>% mutate(long_centroid =
#    centroid(adm1.poly)[,1],
#      lat_centroid = centroid(adm1.poly)[,2], UID = as.character(paste0("adm0_",
#        sprintf("%04d", 1:nrow(adm1.poly@data))))) %>% rename(iso3c=adm0_a3,
#          iso2c=iso_a2, NAME_0=admin, NAME_1=name, adm1_type=type_en)
#    adm1 <- adm1.poly@data
#  ##third counties/municipalities/parishes (adm2)
#  adm2.poly@data <- adm2.poly@data %>% mutate(long_centroid =
#    centroid(adm2.poly)[,1],
#      lat_centroid=centroid(adm2.poly)[,2], UID = as.character(paste0("adm0_",
#        sprintf("%04d", 1:nrow(adm2.poly@data))))) %>% rename(iso3c=ISO,
#          adm2_type=TYPE_2) %>% select(-Shape_Leng, -Shape_Area)
#    adm2 <- adm2.poly@data
#  ## Antarctica has NaN for centroid...remove it
#      adm0 <- adm0[!is.na(adm0$long_centroid),]
#      adm1 <- adm1[!is.na(adm1$long_centroid),]
#      adm2 <- adm2[!is.na(adm2$long_centroid),]

## create SpatialPointsDataFrames
#  ## create proj4string to set coords
#      proj4string4poly <-
#        "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#  ## create spatialPointsDataframes for adm0, adm1, adm2 levels
#      adm0.spdf <- SpatialPointsDataFrame(adm0[,
#        c("long_centroid","lat_centroid")],
#        adm0,proj4string = CRS(proj4string4poly))
#      adm1.spdf <- SpatialPointsDataFrame(adm1[,
#        c("long_centroid", "lat_centroid")],
#        adm1,proj4string = CRS(proj4string4poly))
#      adm2.spdf <- SpatialPointsDataFrame(adm2[,
#        c("long_centroid", "lat_centroid")],
#        adm2,proj4string = CRS(proj4string4poly))

################################################################################
# 5. Save for later use
################################################################################

## save these objects for later use
save(adm0.poly, adm1.poly, adm2.poly, urban.poly, #native_dist,
     #taxon_list, gts_list, rl_native, rl_introduced,
     #adm0, adm1, adm2, adm0.spdf, adm1.spdf, adm2.spdf,
     file=file.path(main_dir, "inputs", "gis_data", "admin_shapefiles.RData"))
