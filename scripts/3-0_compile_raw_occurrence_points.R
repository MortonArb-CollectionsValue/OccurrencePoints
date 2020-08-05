################################################################################

### Authors: Emily Beckman & Shannon Still ### Date: 05/05/2020

### DESCRIPTION:
  # This script compiles data downloaded in 2-0_get_raw_occurrence_points.R,
  #   removes any rows for species not in target list, standardizes some
  #   key columns, and writes a CSV of lat-long points for each species

### DATA IN:
  # raw data downloaded in 2-0_get_raw_occurrence_points.R
  # target_taxa_with_syn.csv (list of target taxa)
    # columns:
      # 1. "taxon_name" (genus, species, infra rank, and infra name, all
      #    separated by one space each; hybrid symbol should be " x ", rather
      #    than "_" or "✕", and go between genus and species)
      # 2. "species_name_acc" (accepted species name you have chosen); this
      #    will be used to split the data
      # 3+ (optional) other data you want to keep with taxa info

### DATA OUT:
  # folder (raw_split_by_sp) with CSV of raw occurrence data for each target
  #   species (e.g., Malus_angustifolia.csv)

################################################################################
# Load libraries
################################################################################

# rm(list=ls())
  my.packages <- c('plyr','tidyverse','housingData','data.table','textclean',
  'CoordinateCleaner','maps','rnaturalearth','rnaturalearthdata','sf','sp',
  'raster')
# install.packages (my.packages) #Turn on to install current versions
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
source('scripts/0-1_set_workingdirectory.R')

################################################################################
# Load functions
################################################################################

source(file.path(script_dir,"0-2_load_IMLS_functions.R"))


################################################################################
################################ LET'S GO ######################################
################################################################################


################################################################################
# 1. Read in raw occurrence point data and stack
################################################################################

# create folder for output data
if(!dir.exists(file.path(main_dir,"outputs")))
  dir.create(file.path(main_dir,"outputs"), recursive=T)

# read in raw datasets
file_list <- list.files(file.path(main_dir,"inputs","compiled_occurrence"),
  pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = T, na.strings = c("","NA"),
  colClasses = "character")
length(file_dfs) #7

# stack all datasets using rbind.fill, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous.
# this may take a few minutes if you have lots of data
all_data_raw <- Reduce(rbind.fill, file_dfs)
  nrow(all_data_raw) #6437517
  ncol(all_data_raw) #27

################################################################################
# 2. Filter by target taxa
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir,"inputs","taxa_list",
  "target_taxa_with_syn.csv"), header = T, na.strings = c("","NA"),
  colClasses = "character")

# full join to taxon list
all_data_raw <- left_join(all_data_raw,taxon_list)
# join again just by species name if no taxon match
need_match <- all_data_raw[which(is.na(all_data_raw$list)),]
  nrow(need_match) #223762
  # remove columns from first taxon name match
need_match <- need_match[,1:(ncol(all_data_raw)-ncol(taxon_list)+1)]
  # rename column for matching
need_match <- need_match %>% rename(taxon_name_full = taxon_name)
need_match$taxon_name <- need_match$species_name
  # new join
need_match <- left_join(need_match,taxon_list)
  # bind together new matches and previously matched
matched <- all_data_raw[which(!is.na(all_data_raw$list)),]
matched$taxon_name_full <- matched$taxon_name
all_data <- rbind(matched,need_match)
  table(all_data$list) # desiderata: 4349588 | synonym: 1932170

# check names that got excluded.....
still_no_match <- all_data[which(is.na(all_data$list)),]
  nrow(still_no_match) #155759
table(still_no_match$database)
#sort(table(still_no_match$taxon_name))

# keep only rows for target taxa
all_data <- all_data[which(!is.na(all_data$list)),]
  nrow(all_data) #6281758

################################################################################
# 3. Standardize some key columns
################################################################################

## this section could potentially be moved to script 2-0

# create localityDescription column
all_data <- all_data %>% unite("localityDescription",
  c(locality,municipality,higherGeography,county,stateProvince,country,
    countryCode,locationNotes,verbatimLocality), remove = F, sep = " | ")
  # get rid of NAs but keep pipes, so you can split back into parts if desired
all_data$localityDescription <- mgsub(all_data$localityDescription,
  c("NA "," NA"), "")
  # if no locality info at all, make it NA
all_data$localityDescription <- gsub("| | | | | | | |", NA,
  all_data$localityDescription, fixed = T)
  # check it
head(unique(all_data$localityDescription))

# check year column
unique(all_data$year)

# check basis of record column
unique(all_data$basisOfRecord)
all_data$basisOfRecord[which(is.na(all_data$basisOfRecord))] <- "UNKNOWN"

# check establishment means
unique(all_data$establishmentMeans)
all_data$establishmentMeans[which(is.na(all_data$establishmentMeans))] <-
  "UNKNOWN"

# check validity of lat and long
  # convert to numeric
all_data$decimalLatitude <- as.numeric(all_data$decimalLatitude)
all_data$decimalLongitude <- as.numeric(all_data$decimalLongitude)
  # if coords are both 0, set to NA
zero <- which(all_data$decimalLatitude == 0 & all_data$decimalLongitude == 0)
all_data$decimalLatitude[zero] <- NA; all_data$decimalLongitude[zero] <- NA
  # flag non-numeric and not available coordinates and lat > 90, lat < -90,
  # lon > 180, and lon < -180
coord_test <- cc_val(all_data, lon = "decimalLongitude",lat = "decimalLatitude",
  value = "flagged", verbose = TRUE) #Flagged 467682 records.
  # try switching lat and long for invalid points and check validity again
all_data[!coord_test,c("decimalLatitude","decimalLongitude")] <-
  all_data[!coord_test,c("decimalLongitude","decimalLatitude")]
coord_test <- cc_val(all_data,lon = "decimalLongitude",lat = "decimalLatitude",
  value = "flagged",verbose = TRUE) #Flagged 467682 records.
  # make coords NA if they are still flagged
all_data[!coord_test,c("decimalLatitude","decimalLongitude")] <- c(NA,NA)

# set column order and remove a few unnecessary columns
all_data <- all_data %>% dplyr::select(species_name_acc,taxon_name,
  scientificName,taxonIdentificationNotes,database,year,basisOfRecord,
  establishmentMeans,decimalLatitude,decimalLongitude,
  coordinateUncertaintyInMeters,geolocationNotes,localityDescription,county,
  stateProvince,country,countryCode,locationNotes,datasetName,publisher,
  nativeDatabaseID,references,informationWithheld,issue,taxon_name_full,list)

# add unique ID column
all_data$unique_id <- seq.int(nrow(all_data))

# create folder for working files
if(!dir.exists(file.path(main_dir,"outputs","working")))
  dir.create(file.path(main_dir,"outputs","working"), recursive=T)

# separate out points with locality description only (no lat-long)
locality_pts <- all_data %>% filter(!is.na(localityDescription) &
  (is.na(decimalLatitude) | is.na(decimalLongitude))) %>%
  arrange(desc(year)) %>%
  distinct(species_name_acc,localityDescription,.keep_all=T)
nrow(locality_pts) #241011
table(locality_pts$database)
write.csv(locality_pts, file.path(main_dir,"outputs","working",
  "need_geolocation.csv"),row.names = F)

# move forward with subset of points that do have lat and long
geo_pts <- all_data %>%
  filter(!is.na(decimalLatitude) &
  !is.na(decimalLongitude)) %>%
  dplyr::select(-localityDescription)
    nrow(geo_pts) #5814076

# check if points are in water, mark, and separate out as other file
  # read in world country shapefile
world_polygons <- ne_countries(type = 'countries', scale = 'medium')
  # add buffer; 0.01 dd = ~ 0.4 to 1 km depending on location
world_buff <- buffer(world_polygons, width=0.01, dissolve=F)
  # check if in water and mark, then separate out
geo_pts$in_water <- is.na(map.where(world_buff, geo_pts$decimalLongitude,
  geo_pts$decimalLatitude))
water_pts <- geo_pts %>%
  filter(in_water) %>%
  dplyr::select(-in_water)
nrow(water_pts) #48947
table(water_pts$database)
write.csv(water_pts, file.path(main_dir,"outputs","working",
  "not_on_land.csv"),row.names = F)

# create final subset of geolocated points which are on land
geo_pts <- geo_pts %>%
  filter(!in_water) %>%
  dplyr::select(-in_water)
nrow(geo_pts) #5765129
table(geo_pts$database)
# can write a file just to look it over
#write.csv(geo_pts, file.path(main_dir,"outputs","are_geolocated.csv"),
#  row.names = F)

################################################################################
# 4. Remove duplicates
################################################################################

## this section could potentially be moved to script 04
## OTHER WAYS OF REMOVING DUPLICATES ARE ALSO POSSIBLE AND COULD MAKE MORE
##    SENSE FOR A SPECIFIC WAY OF USING THE POINTS, including
##    by grid cell, distance between points, etc...
## The segement below removes spatial duplicates based on rounded lattitude
##    and longitude. This is a simple fix that doesn't involved spatial data
##    or complex spatial calculations.

# create subset of all ex situ points, to add back in at end, if desired
ex_situ <- geo_pts[which(geo_pts$database=="Ex_situ"),]

# sort before removing duplicates; can turn any of these on/off, or add others
  # sort by basis of record
geo_pts$basisOfRecord <- factor(geo_pts$basisOfRecord,
  levels = c("PRESERVED_SPECIMEN","MATERIAL_SAMPLE","OBSERVATION",
  "HUMAN_OBSERVATION","MACHINE_OBSERVATION","LITERATURE","FOSSIL_SPECIMEN",
  "LIVING_SPECIMEN","UNKNOWN"))
geo_pts <- geo_pts %>% arrange(basisOfRecord)
  # sort by establishment means
geo_pts$establishmentMeans <- factor(geo_pts$establishmentMeans,
  levels = c("NATIVE","UNKNOWN","INTRODUCED","MANAGED","CUT","INVASIVE","DEAD"))
geo_pts <- geo_pts %>% arrange(establishmentMeans)
  # sort by coordiante uncertainty
geo_pts$coordinateUncertaintyInMeters <-
  as.numeric(geo_pts$coordinateUncertaintyInMeters)
geo_pts <- geo_pts %>% arrange(geo_pts$coordinateUncertaintyInMeters)
  # sort by year
geo_pts <- geo_pts %>% arrange(desc(year))
  # sort by dataset
geo_pts$database <- factor(geo_pts$database,
  levels = c("FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN","Ex_situ"))
geo_pts <- geo_pts %>% arrange(database)

# create rounded latitude and longitude columns for removing duplicates
#   number of digits can be changed based on how dense you want data
geo_pts$lat_round <- round(geo_pts$decimalLatitude,digits=3)
geo_pts$long_round <- round(geo_pts$decimalLongitude,digits=3)

# remove duplicates
# can create "all_source_databases" column, to capture
#    databases from which duplicates were removed
# can take a while to remove duplicates if there are lots a rows
geo_pts2 <- geo_pts %>%
  group_by(species_name_acc,lat_round,long_round) %>%
  mutate(all_source_databases = paste(database,collapse = ',')) %>%
  distinct(species_name_acc,lat_round,long_round,.keep_all=T) %>%
  ungroup()
  # remove duplicates in all_source_databases column
parts <- lapply(geo_pts2$all_source_databases, function(x)
  unlist(strsplit(x,split=",")))
source_standard <- as.data.frame(unlist(lapply(parts,function(x)
  paste(sort(unique(x)),collapse=','))))
names(source_standard)[1] <- "all_source_databases"
geo_pts2 <- geo_pts2 %>%
  dplyr::select(-all_source_databases) %>%
  cbind(source_standard)
  # take a look
head(geo_pts2)
nrow(geo_pts2) #1457614
table(geo_pts2$all_source_databases)
table(geo_pts2$database)

################################################################################
# 5. Look at results
################################################################################

# take a look at results
  # lat-long records
count_geo <- geo_pts2 %>% count(species_name_acc)
count_geo <- setorder(count_geo,n)
names(count_geo)[2] <- "num_latlong_records"
  # water records
count_water <- water_pts %>% count(species_name_acc)
count_water <- setorder(count_water,n)
names(count_water)[2] <- "num_water_records"
  # locality-only records
count_locality <- locality_pts %>% count(species_name_acc)
count_locality <- setorder(count_locality,n)
names(count_locality)[2] <- "num_locality_records"
  # make table of all categories
files <- list(count_geo,count_water,count_locality)
summary <- Reduce(full_join, files)
head(summary)
  # write file
write.csv(summary, file.path(main_dir,"outputs","working",
  "occurrence_point_count_per_sp.csv"),row.names = F)

## can save data out to a file so don't have to rerun
#save(all_data, taxon_list, s, geo_pts2, need_match,
#  file=file.path(main_dir,"outputs","EO_data.RData"))
#  rm(all_data_raw, file_dfs, geo_pts, locality_pts, matched, need_match,
#  source_standard); head(all_data)

################################################################################
# 6. Split by species
################################################################################

#load(file.path(main_dir,"outputs","EO_data.RData"))

# split lat-long points to create one CSV for each target species
sp_split <- split(geo_pts2, as.factor(geo_pts2$species_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))

# write files
if(!dir.exists(file.path(main_dir,"outputs","working","split_by_sp")))
  dir.create(file.path(main_dir,"outputs","working","split_by_sp"), recursive=T)
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
  file.path(main_dir,"outputs","working","split_by_sp",
  paste0(names(sp_split)[[i]], ".csv")),row.names = F))
