################################################################################

## 1-1_prepare_gis_data.R
### Authors: Shannon M. Still & Emily Beckman ### Date: 08/04/2020

### DESCRIPTION:
  # This script is meant for massaging geographical data in both spatial
  #   polygon and tabular formats.
  # Prepares polygon data
  # Prepares tables of species distributions based upon taxa lists

### DATA IN:
  # output from 3_compile_raw_occurrence_points.R
  # tabular data:
  # - target_taxa_with_syn.csv
  # - globaltreesearch_country_distribution.csv
  # - spatialpolygon data ...
  #

### DATA OUT:
  # globaltreesearch_countries.xlsx
  # IMLS_GIS_data.RData
      # adm0.poly, adm1.poly, adm2.poly, taxon_list, gts_list, gts_all, 
      # adm0, adm1, adm2, adm0.spdf, adm1.spdf, adm2.spdf
  # These 5 files are for looking/working over the data
    # global_admin_areas.xlsx
    # taxa_work.xlsx
    # geo_work0.xlsx
    # geo_work1.xlsx
    # geo_work2.xlsx


################################################################################
# Load libraries
################################################################################

#rm(list=ls())
my.packages <- c("raster", "sp", "tools", "spatialEco", "rgdal", "geosphere",
  "readxl", "writexl", "dplyr", "tidyr", "tidyverse", "housingData",
  "data.table", "textclean", "CoordinateCleaner", "countrycode", "usmap",
  "rnaturalearth", "rnaturalearthdata", "maps")#, "sf"
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

source(file.path(script_dir,"0-2_load_IMLS_functions.R"))

################################################################################
################################ LET'S GO ######################################
################################################################################


################################################################################
# 1. Create native country list for each taxon using GlobalTreeSearch data
################################################################################
# create folder for final files
if(!dir.exists(file.path(main_dir,"outputs","final")))
  dir.create(file.path(main_dir,"outputs","final"), recursive=T)

# read in taxa list
taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
  "target_taxa_with_syn.csv"), header = T, na.strings=c("","NA"),
  colClasses="character")
# read in GlobalTreeSearch data
gts_list <- read.csv(file.path(main_dir,"inputs","known_distribution",
  "globaltreesearch_country_distribution.csv"), header = T,
  na.strings=c("","NA"), colClasses="character")

# split countries by delimiter
gts_all <- gts_list %>%
  mutate(native_distribution =
    strsplit(as.character(native_distribution), "; ")) %>%
  unnest(native_distribution) %>% mutate(native_distribution = str_trim(native_distribution, side="both"))
# write out all GTS countries to check
spp_countries <- as.data.frame(sort(unique(str_trim(gts_all$native_distribution, side = c("both")))))
write_xlsx(spp_countries, path=file.path(main_dir,"inputs","known_distribution",
  "globaltreesearch_countries.xlsx"))

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
# save the country codes for species, ISO2, ISO3, and numeric and character codes, FIPS code
  write_xlsx(country_set, path=file.path(main_dir,"inputs","gis_data",
      "global_admin_areas.xlsx"))

## add the country code to the GTS list by matching taxon names
    ## this part is incomplete, so skip next couple of lines
          # names(gts_list)
            ## gts_list <- gts_list %>%
# add the country code to the taxon list by matching taxon names
  taxon_co <- left_join(taxon_list, gts_list[,c(2,4)],
      by=c("species_name_acc" = "taxon"))
write_xlsx(taxon_co, path=file.path(main_dir,"inputs","known_distribution",
    "taxa_work.xlsx"))

################################################################################
## This next little section ~20 lines is not implemented but is an idea about 
    ## how to use rredlist package to get taxon names and spp. dist.
# WORKING: can also get RL country-level species distribution data like this:
#library(rredlist)
  #countries <- data.frame()
  #target_taxa <- taxon_list[,1]
  #for(i in 1:length(target_taxa)){
  	#print(target_sp[[i]])
  #	dist <- rl_occ_country(target_taxa[[i]])
  #	name <- dist$name
  #	dist <- as.data.frame(dist$result)
  #	if(nrow(dist>0)){
  #		dist$genus_species <- rep(name)
  #		countries <- rbind(countries,dist)
  #	} else {
  #		print(paste(target_taxa[[i]],"not found"))
  #	}
  #}
################################################################################

################################################################################
# 2. Create native country list for each taxon using GlobalTreeSearch data
################################################################################

## bring in polygon for world regions and US (down to county level);
##   could use maps::county() function instead
##    but will use the rnaturalearthhires package for polygons at adm0/adm1 levels
##        will use a provided shapefile for the adm2 level (county)
  # may need to run this line first, to get rnaturalearthhires package working:
  #devtools::install_github("ropensci/rnaturalearthhires")
adm0.poly <- ne_countries(type = "countries", scale = "large")
adm1.poly <- ne_states(country=NULL)
adm2.poly <- readOGR(dsn=file.path(main_dir,"inputs","gis_data","usa",
  "USA_adm"), "USA_adm2")
#plot(adm1.poly)
#names(adm0.poly); names(adm1.poly); names(adm2.poly); head(adm2.poly)
write_xlsx(adm0.poly@data, path=file.path(main_dir,"inputs","gis_data",
  "geo_work0.xlsx"))
write_xlsx(adm1.poly@data, path=file.path(main_dir,"inputs","gis_data",
  "geo_work1.xlsx"))
write_xlsx(adm2.poly@data, path=file.path(main_dir,"inputs","gis_data",
  "geo_work2.xlsx"))
save(adm0.poly, adm1.poly, adm2.poly, gts_list, file=file.path(main_dir,
 "inputs","gis_data","IMLS_GIS_data.RData"))

################################################################################
## calculate centroid of polygons
  ##first countries (adm0)
  adm0.poly@data <- adm0.poly@data %>% mutate(long_centroid = centroid(adm0.poly)[,1], 
        lat_centroid = centroid(adm0.poly)[,2])
    adm0 <- adm0.poly@data
  ##second states (adm1)
  adm1.poly@data <- adm1.poly@data %>% mutate(long_centroid = centroid(adm1.poly)[,1], 
        lat_centroid = centroid(adm1.poly)[,2])
    adm1 <- adm1.poly@data
  ##third counties/municipalities/parishes (adm2)
  adm2.poly@data <- adm2.poly@data %>% mutate(long_centroid = centroid(adm2.poly)[,1], 
        lat_centroid = centroid(adm2.poly)[,2])
    adm2 <- adm2.poly@data
## save these objects for later use
save(adm0.poly, adm1.poly, adm2.poly, taxon_list, gts_list, gts_all, 
      adm0, adm1, adm2, file=file.path(main_dir, "inputs", "gis_data", 'IMLS_GIS_data.RData'))
  rm(adm0, adm1, adm2)
################################################################################

  load(file.path(main_dir, "inputs", "gis_data", "IMLS_GIS_data.RData"))

  ## give a unique identifier to adm0 (country), adm1 (state) and adm2 (county level)
      adm0 <- adm0 %>% mutate(UID = as.character(paste0("adm0_", iso_a2))) %>% select(UID, colnames(adm0))
      adm1 <- adm1 %>% mutate(UID = as.character(paste0("adm1_", iso_a2))) %>% select(UID, colnames(adm1))
      adm2 <- adm2 %>% mutate(UID = as.character(paste0("adm2_", ID_2))) %>% select(UID, colnames(adm2))

  ## create proj4string to set coords
      proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  ## Antarctica has NaN for centroid...remove it
      adm0 <- adm0[!is.na(adm0$long_centroid),]
      adm1 <- adm1[!is.na(adm1$long_centroid),]
      adm2 <- adm2[!is.na(adm2$long_centroid),]

  ## create spatialPolygonDataframes for adm0, adm1, adm2 levels
      adm0.spdf <- SpatialPointsDataFrame(adm0[,c("long_centroid", "lat_centroid")], adm0,
                                          proj4string = CRS(proj4string4poly))
      adm1.spdf <- SpatialPointsDataFrame(adm1[,c("long_centroid", "lat_centroid")], adm1,
                                          proj4string = CRS(proj4string4poly))
      adm2.spdf <- SpatialPointsDataFrame(adm2[,c("long_centroid", "lat_centroid")], adm2,
                                          proj4string = CRS(proj4string4poly))

## save these objects for later use
save(adm0.poly, adm1.poly, adm2.poly, taxon_list, gts_list, gts_all, 
      adm0, adm1, adm2, adm0.spdf, adm1.spdf, adm2.spdf,
      file=file.path(main_dir, "inputs", "gis_data", 'IMLS_GIS_data.RData'))
  rm(adm0.poly, adm1.poly, adm2.poly, taxon_list, gts_list, gts_all, 
      adm0, adm1, adm2, adm0.spdf, adm1.spdf, adm2.spdf)
