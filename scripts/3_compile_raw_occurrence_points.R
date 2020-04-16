### Author: Emily Beckman  ###  Date: 04/15/2020                                |

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

#################
### LIBRARIES ###
#################

library(plyr)
library(tidyverse) #ggplot2,dplyr,tidyr,readr,purrr,tibble,stringr,forcats
library(housingData)
library(data.table)
library(textclean)
library(CoordinateCleaner)


################################################################################
# A) Read in data and stack
################################################################################

setwd("./../..")
setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")

# read in raw datasets
file_list <- list.files(path = "raw_occurrence_point_data",
  pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = T, na.strings=c("","NA"),
  colClasses="character")
length(file_dfs) #5

# stack all datasets using rbind.fill, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous
# this may take a few minutes if you have lots of data
all_data_raw <- Reduce(rbind.fill, file_dfs)
  nrow(all_data_raw) #8389845
  ncol(all_data_raw) #30

################################################################################
# B) Filter by target taxa
################################################################################

# read in target taxa list
taxon_list <- read.csv("target_taxa_with_syn.csv", header = T,
  na.strings=c("","NA"), colClasses="character")

# full join to taxon list
all_data_raw <- left_join(all_data_raw,taxon_list)
# join again just by species name if no taxon match
need_match <- all_data_raw[which(is.na(all_data_raw$list)),]
  nrow(need_match) #690031
need_match <- need_match[,1:(ncol(all_data_raw)-ncol(taxon_list)+1)]
need_match <- need_match %>% rename(taxon_name_full = taxon_name)
need_match$taxon_name <- need_match$species_name
need_match <- left_join(need_match,taxon_list)
matched <- all_data_raw[which(!is.na(all_data_raw$list)),]
matched$taxon_name_full <- matched$taxon_name
all_data <- rbind(matched,need_match)
  table(all_data$list)

# check names that got excluded.....
#still_no_match <- all_data[which(is.na(all_data$list)),]
#  nrow(still_no_match) #624119
#table(still_no_match$database)
#sort(table(still_no_match$taxon_name))

# keep only rows for target taxa
all_data <- all_data[which(!is.na(all_data$list)),]
  nrow(all_data) #8022416

################################################################################
# C) Standardize some key columns
################################################################################

# create localityDescription column
all_data <- all_data %>% unite("localityDescription",
  c(locality,verbatimLocality,municipality,higherGeography,county,stateProvince,
    country,countryCode),remove=T,sep=" | ") #na.rm=T,
all_data$localityDescription <-
  mgsub(all_data$localityDescription,c("NA "," NA"),"")
all_data$localityDescription <-
  gsub("| | | | | | |",NA,all_data$localityDescription,fixed=T)

# check year column
unique(all_data$year)

# check basis of record column
unique(all_data$basisOfRecord)

# check establishment means
unique(all_data$establishmentMeans)
all_data$establishmentMeans[which(is.na(all_data$establishmentMeans))] <-
  "UNKNOWN"

# check validity of lat and long
  # flag non-numeric and not available coordinates and lat > 90, lat < -90,
  # lon > 180, and lon < -180
coord_test <- cc_val(all_data,lon = "decimalLongitude",lat = "decimalLatitude",
  value = "flagged",verbose = TRUE)
  # try switching lat and long and check validity again
all_data[!coord_test,c("decimalLatitude","decimalLongitude")] <-
  all_data[!coord_test,c("decimalLongitude","decimalLatitude")]
coord_test <- cc_val(all_data,lon = "decimalLongitude",lat = "decimalLatitude",
  value = "flagged",verbose = TRUE)
  # make coords NA if they are still flagged
all_data[!coord_test,c("decimalLatitude","decimalLongitude")] <- c(NA,NA)

# set column order and remove a few unnecessary columns
all_data <- all_data %>% select(species_name_acc,taxon_name,scientificName,
  taxonIdentificationNotes,database,year,basisOfRecord,establishmentMeans,
  decimalLatitude,decimalLongitude,coordinateUncertaintyInMeters,
  geolocationNotes,localityDescription,locationNotes,datasetName,publisher,
  nativeDatabaseID,references,source_databases,informationWithheld,issue,
  taxon_name_full,list)

# create subsets for locality-only points and lat-long points, then
#   continue forward with just lat-long points
locality_pts <- all_data %>% filter(!is.na(localityDescription) &
  (is.na(decimalLatitude) | is.na(decimalLongitude))) %>%
  arrange(desc(year)) %>%
  distinct(species_name_acc,localityDescription,.keep_all=T)
  nrow(locality_pts) #212168
  write.csv(locality_pts,"need_geolocation.csv")
geo_pts <- all_data %>% filter(!is.na(decimalLatitude) &
  !is.na(decimalLongitude))
  nrow(geo_pts) #7564899

################################################################################
# D) Remove duplicates
################################################################################

# sort before removing duplicates
geo_pts <- geo_pts %>% arrange(desc(year))

# create rounded latitude and longitude columns for removing dups
geo_pts$lat_round <- round(as.numeric(geo_pts$decimalLatitude),digits=3)
geo_pts$long_round <- round(as.numeric(geo_pts$decimalLongitude),digits=3)

# remove duplicates
geo_pts2 <- geo_pts %>%
  group_by(species_name_acc,lat_round,long_round,establishmentMeans) %>%
  mutate(source_databases = paste(database,collapse = ',')) %>%
  distinct(species_name_acc,lat_round,long_round,establishmentMeans,
    .keep_all=T) %>%
  ungroup()
  # remove duplicates in source_databases column
  s <- lapply(geo_pts2$source_databases, function(x)
    unlist(strsplit(x,split=",")))
  source_standard <- as.data.frame(unlist(lapply(s,function(x)
    paste(sort(unique(x)),collapse=','))))
  names(source_standard)[1] <- "source_databases"
  geo_pts2 <- geo_pts2 %>% select(-source_databases) %>% cbind(source_standard)
head(geo_pts2)
table(geo_pts2$source_databases)

# mark rows that may be geolocated to U.S. county centroids
geoCounty$lat_round <- round(geoCounty$cty_centroid_lat,digits=2)
geoCounty$long_round <- round(geoCounty$cty_centroid_lon,digits=2)
geoCounty <- geoCounty[,c(1,8:9)]
geo_pts2 <- left_join(geo_pts2,geoCounty)
nrow(geo_pts2)
nrow(geo_pts2[which(!is.na(geo_pts2$FIPS)),])

# take a look at results
count_geo <- geo_pts2 %>% count(species_name_acc)
count_geo <- setorder(count_geo,n)
names(count_geo)[2] <- "num_latlong_records"
count_locality <- locality_pts %>% count(species_name_acc)
count_locality <- setorder(count_locality,n)
names(count_locality)[2] <- "num_locality_records"
summary <- full_join(count_geo,count_locality)
write.csv(summary,"occurrence_point_count_per_species.csv")

################################################################################
# E) Split by species
################################################################################

# split
sp_split <- split(geo_pts2, as.factor(geo_pts2$species_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))
# write files
dir.create(file.path(getwd(),"raw_split_by_sp"))
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
  paste("raw_split_by_sp/",names(sp_split)[[i]],".csv",sep="")))
