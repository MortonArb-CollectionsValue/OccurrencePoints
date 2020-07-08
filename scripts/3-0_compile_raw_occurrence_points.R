################################################################################

### Authors: Emily Beckman & Shannon Still ### Date: 05/05/2020

### DESCRIPTION:
# This script compiles data downloaded in 2_get_raw_occurrence_points.R,
#   removes any rows for species not in target list, standardizes some
#   key columns, and writes a CSV of lat-long points for each species

### INPUT:
# target_taxa_with_syn.csv (list of target taxa)
# columns:
# 1. "taxon_name" (genus, species, infra rank, and infra name, all
#    separated by one space each; hybrid symbol should be " x ", rather
#    than "_" or "✕", and go between genus and species)
# 2. "species_name_acc" (accepted species name you have chosen); this
#    will be used to split the data
# 3+ (optional) other data you want to keep with taxa info

### OUTPUTS:
# folder (raw_split_by_sp) with CSV of raw occurrence data for each target
#   species (e.g., Malus_angustifolia.csv)


################################################################################
# Load packages
################################################################################

rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'housingData', 'data.table', 'textclean',
                 'CoordinateCleaner', 'maps', 'rnaturalearth', 'rnaturalearthdata')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)


### could remove once we get "00" script up and running
#source("./Documents/GitHub/IMLS_CollectionsValue/scripts/set_workingdirectory.R")
source('scripts/0-1_set_workingdirectory.R')

################################################################################
# A) Read in raw data and stack
################################################################################

# read in raw datasets
file_list <- list.files(file.path(imls.raw, "datasets_edited"), pattern = ".csv",
                        full.names = T)
file_dfs <- lapply(file_list, read.csv, header = T, na.strings = c("","NA"),
                   colClasses = "character")
length(file_dfs) #6

# stack all datasets using rbind.fill, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous.
# this may take a few minutes if you have lots of data
all_data_raw <- Reduce(rbind.fill, file_dfs)
nrow(all_data_raw) #8085017
ncol(all_data_raw) #28
table(all_data_raw$database)
##add unique identifier
nms <- names(all_data_raw)

all_data_raw <- all_data_raw %>% mutate(UID=paste0('imls', sprintf("%07d", 1:nrow(all_data_raw)))) %>% select(c('UID', nms))
rm(nms, file_dfs)

write.csv(all_data_raw, file.path(imls.raw, "all_data_cleaning.csv"), row.names=FALSE)

################################################################################
# B) Filter by target taxa
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(imls.meta, "target_taxa_with_syn.csv"),
                       header = T, na.strings = c("","NA"), colClasses = "character")

# full join to taxon list
all_data <- left_join(all_data_raw, taxon_list); rm(all_data_raw)
# join again just by species name if no taxon match
need_match <- all_data[which(is.na(all_data$list)),]
nrow(need_match) #124547
# remove columns from first taxon name match
need_match <- need_match[,1:(ncol(all_data)-ncol(taxon_list)+1)]
# rename column for matching
need_match <- need_match %>% rename(taxon_name_full = taxon_name)
need_match$taxon_name <- need_match$species_name
# new join
need_match <- left_join(need_match,taxon_list)
# bind together new matches and previously matched
matched <- all_data[which(!is.na(all_data$list)),]
matched$taxon_name_full <- matched$taxon_name
all_data <- rbind(matched, need_match)
table(all_data$list) # desiderata: 5928806 | synonym: 2102904

# check names that got excluded.....
still_no_match <- all_data[which(is.na(all_data$list)),]
nrow(still_no_match) #58560
table(still_no_match$database)
#sort(table(still_no_match$taxon_name))

write.csv(still_no_match, file.path(imls.raw, 'records_to_examine', "no_taxon_match.csv"), row.names=FALSE)


# keep only rows for target taxa
all_data <- all_data[which(!is.na(all_data$list)),]
nrow(all_data) #8031710

################################################################################
# C) Standardize some key columns
################################################################################

## this section could potentially be moved to script 02

# create localityDescription column
all_data <- all_data %>% unite("localityDescription",
                               c(locality, verbatimLocality, municipality, higherGeography, county, stateProvince,
                                 country, countryCode), remove = F, sep = " | ")
# get rid of NAs but keep pipes, so you can split back into parts if desired
all_data$localityDescription <- mgsub(all_data$localityDescription,
                                      c("NA "," NA"), "")
# if no locality info at all, make it NA
all_data$localityDescription <- gsub("| | | | | | |", NA,
                                     all_data$localityDescription, fixed = T)
# check it
head(unique(all_data$localityDescription))

# check year column
unique(all_data$year)

# check basis of record column
unique(all_data$basisOfRecord)

# check establishment means
unique(all_data$establishmentMeans)
all_data$establishmentMeans[which(is.na(all_data$establishmentMeans))] <-
  "UNKNOWN"

# check validity of lat and long
# convert to numeric
all_data$decimalLatitude  <- as.numeric(all_data$decimalLatitude)
all_data$decimalLongitude <- as.numeric(all_data$decimalLongitude)
# if coords are both 0, set to NA
zero <- which(all_data$decimalLatitude == 0 & all_data$decimalLongitude == 0)
all_data$decimalLatitude[zero] <- NA; all_data$decimalLongitude[zero] <- NA
# flag non-numeric and not available coordinates and lat > 90, lat < -90,
# lon > 180, and lon < -180
coord_test <- cc_val(all_data, lon = "decimalLongitude", lat = "decimalLatitude",
                     value = "flagged", verbose = TRUE)

# out.pts <- all_data[coord_test,c("decimalLatitude", "decimalLongitude")]
# write.csv(out.pts, file.path(imls.local, "data_gis_check.csv"), row.names=FALSE)

# try switching lat and long for invalid points and check validity again
all_data[!coord_test,c("decimalLatitude", "decimalLongitude")] <-
  all_data[!coord_test,c("decimalLongitude", "decimalLatitude")]
coord_test <- cc_val(all_data, lon = "decimalLongitude", lat = "decimalLatitude",
                     value = "flagged", verbose = TRUE)
# make coords NA if they are still flagged
all_data[!coord_test, c("decimalLatitude", "decimalLongitude")] <- c(NA, NA)



# set column order and remove a few unnecessary columns
all_data <- all_data %>% dplyr::select(UID, species_name_acc, taxon_name, scientificName,
                                       taxonIdentificationNotes, database, year, basisOfRecord, establishmentMeans,
                                       decimalLatitude, decimalLongitude, coordinateUncertaintyInMeters,
                                       geolocationNotes, localityDescription, county, stateProvince, country, countryCode,
                                       locationNotes, datasetName, publisher, nativeDatabaseID, references,
                                       informationWithheld, issue, taxon_name_full, list)

# add unique ID column
# all_data$unique_id <- seq.int(nrow(all_data))

# separate out points with locality description only (no lat-long)
locality_pts <- all_data %>% filter(!is.na(localityDescription) &
                                      (is.na(decimalLatitude) | is.na(decimalLongitude))) %>%
  arrange(desc(year)) %>%
  distinct(species_name_acc,localityDescription,.keep_all=T)
nrow(locality_pts) #212901
table(locality_pts$database)
write.csv(locality_pts, file.path(imls.raw, 'records_to_examine', "need_geolocation.csv"),
          row.names = F)

# move forward with subset of points that do have lat and long
geo_pts <- all_data %>%
  filter(!is.na(decimalLatitude) &
           !is.na(decimalLongitude)) %>%
  dplyr::select(-localityDescription)
nrow(geo_pts) #7571820

# # check if points are in water, mark, and separate out as other file
#       ## we can also flag as how close to the polygon
# world.poly <- ne_countries(type = 'countries', scale = 'medium')
# geo_pts$in_water <- is.na(map.where(world.poly, geo_pts$decimalLongitude,
#   geo_pts$decimalLatitude))
# water_pts <- geo_pts %>%
#   filter(in_water) %>%
#   dplyr::select(-in_water)
#       nrow(water_pts) #59030
#       table(water_pts$database)
# write.csv(water_pts, file.path(imls.output, "not_on_land.csv"), row.names = F)
# 
# # create final subset of geolocated points which are on land
# geo_pts <- geo_pts %>%
#   filter(!in_water) %>%
#   dplyr::select(-in_water)
#       nrow(geo_pts) #7512790
#       table(geo_pts$database)
# can write a file just to look it over
# write.csv(geo_pts, file.path(imls.local, "are_geolocated.csv"), row.names = F)

################################################################################
# D) Remove duplicates
################################################################################

## this section could potentially be moved to script 04
## OTHER WAYS OF REMOVING DUPLICATES ARE ALSO POSSIBLE AND COULD MAKE MORE
##    SENSE FOR A SPECIFIC WAY OF USIGN THE POINTS, including
##    by grid cell, distance between points, etc...
## The segement below removes spatial duplicates based on rounded lattitude
##    and longitude. This is a simple fix that doesn't involved spatial data
##    or complex spatial calculations.

# sort before removing duplicates; can turn any of these on/off, or add others
# sort by basis of record
geo_pts$basisOfRecord <- factor(geo_pts$basisOfRecord,
                                levels = c("PRESERVED_SPECIMEN", "MATERIAL_SAMPLE", "OBSERVATION",
                                           "HUMAN_OBSERVATION", "MACHINE_OBSERVATION", "LITERATURE", "FOSSIL_SPECIMEN",
                                           "LIVING_SPECIMEN", "UNKNOWN"))
geo_pts <- geo_pts %>% arrange(basisOfRecord)
# sort by establishment means
geo_pts$establishmentMeans <- factor(geo_pts$establishmentMeans,
                                     levels = c("NATIVE", "UNKNOWN", "INTRODUCED", "MANAGED", "CUT", "INVASIVE", "DEAD"))
geo_pts <- geo_pts %>% arrange(establishmentMeans)
# sort by coordiante uncertainty
geo_pts$coordinateUncertaintyInMeters <-
  as.numeric(geo_pts$coordinateUncertaintyInMeters)
geo_pts <- geo_pts %>% arrange(geo_pts$coordinateUncertaintyInMeters)
# sort by year
geo_pts <- geo_pts %>% arrange(desc(year))
# sort by dataset
geo_pts$database <- factor(geo_pts$database,
                           levels = c("FIA", "GBIF", "US_Herbaria", "iDigBio", "BISON", "BIEN", "Ex_situ"))
geo_pts <- geo_pts %>% arrange(database)

# create rounded latitude and longitude columns for removing duplicates
#   number of digits can be changed based on how dense you want data
geo_pts$lat_round <- round(geo_pts$decimalLatitude,digits=4)
geo_pts$long_round <- round(geo_pts$decimalLongitude,digits=4)

# remove duplicates and create "all_source_databases" column, to capture
#    databases from which duplicates were removed
# can take a while to remove duplicates if there are lots a rows
geo_pts2 <- geo_pts %>%
  group_by(species_name_acc, lat_round,long_round) %>%
  #mutate(all_source_databases = paste(database,collapse = ',')) %>%
  distinct(species_name_acc, lat_round, long_round, .keep_all=T) %>%
  ungroup()
# remove duplicates in all_source_databases column
#parts <- lapply(geo_pts2$source_databases, function(x)
#  unlist(strsplit(x,split=",")))
#source_standard <- as.data.frame(unlist(lapply(parts,function(x)
#  paste(sort(unique(x)),collapse=','))))
#names(source_standard)[1] <- "all_source_databases"
#geo_pts2 <- geo_pts2 %>%
#  select(-all_source_databases) %>%
#  cbind(source_standard)
head(geo_pts2)
nrow(geo_pts2) #1367167
#table(geo_pts2$all_source_databases)
table(geo_pts2$database)

# take a look at results
# lat-long records
count_geo <- geo_pts2 %>% count(species_name_acc)
count_geo <- setorder(count_geo,n)
names(count_geo)[2] <- "num_latlong_records"
# water records
# count_water <- water_pts %>% count(species_name_acc)
# count_water <- setorder(count_water,n)
# names(count_water)[2] <- "num_water_records"
#   # locality-only records
count_locality <- locality_pts %>% count(species_name_acc)
count_locality <- setorder(count_locality,n)
names(count_locality)[2] <- "num_locality_records"
# make table of all categories
files <- list(count_geo, count_locality) #count_water, 
summary <- Reduce(full_join, files)
head(summary)
# write file
write.csv(summary, file.path(imls.output, "occurrence_point_count_per_sp.csv"),
          row.names = F)
rm(files, count_geo, count_locality, matched, need_match)#, count_water

##save data out to a file so don't have to rerun
#save(all_data, taxon_list, s, geo_pts2, need_match, file=file.path(data_in, "EO_data.RData"))
#  rm(all_data, file_dfs, geo_pts, locality_pts, matched, need_match, source_standard)
#  head(all_data)

################################################################################
# E) Split by species
################################################################################

#load(file.path(data_in, "EO_data.RData"))

# split lat-long points to create one CSV for each target species
sp_split <- split(geo_pts2, as.factor(geo_pts2$species_name_acc))
names(sp_split) <- gsub(" ","_", names(sp_split))

# write files
if(dir.exists(file.path(imls.output, "split_by_sp")))
  print('directory already created') else
    dir.create(file.path(imls.output, "split_by_sp"), recursive = TRUE)
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
                                                  file.path(imls.output, "split_by_sp", paste0(names(sp_split)[[i]], ".csv")),
                                                  row.names = F))

rm(sp_split)
