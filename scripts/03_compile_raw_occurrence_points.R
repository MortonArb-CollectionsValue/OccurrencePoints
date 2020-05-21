### Author: Emily Beckman & Shannon Still ###  Date: 05/05/2020                                |

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

rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'housingData', 'data.table', 'textclean', 'CoordinateCleaner')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

#######################################
# run code to set your working directory and project folders based upon computer using
#      skip this if preferred, but then need to set your working directory and input/output folders manually
      # If not using working directory script, be sure to create data_in path
####################################################################################
source('scripts/set_workingdirectory.R')

# setwd("./../..")
# setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")
# data_in <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points"

################################################################################
# A) Read in data and stack
################################################################################

# read in raw datasets
  ## using file.path, I have listed the files in the directory "raw_occurrence_point_data"
  ## data_in is an object defined in the script 'set_workingdirectory.R'
file_list <- list.files(file.path(data_in, 'insitu_occurrence_points', "raw_occurrence_point_data"),
                        pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, header = T, na.strings=c("","NA"),
                   colClasses="character")
length(file_dfs) #6

# stack all datasets using rbind.fill, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous
# this may take a few minutes if you have lots of data
all_data_raw <- Reduce(rbind.fill, file_dfs)
  nrow(all_data_raw) #8085017
  ncol(all_data_raw) #28
  table(all_data_raw$database)

################################################################################
# B) Filter by target taxa
################################################################################

# read in target taxa list
taxon_list <- read.csv(file.path(data_in, 'insitu_occurrence_points', "target_taxa_with_syn.csv"), 
                       header=TRUE, na.strings=c("","NA"), colClasses="character")
  # taxon_list <- read.csv("target_taxa_with_syn.csv", header = T,
  #       na.strings=c("","NA"), colClasses="character")

# full join to taxon list
all_data_raw <- left_join(all_data_raw,taxon_list)
# join again just by species name if no taxon match
need_match <- all_data_raw[which(is.na(all_data_raw$list)),]
  nrow(need_match) #124399
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
  table(all_data$list) # desiderata: 5924118 | synonym: 2102339

# check names that got excluded.....
still_no_match <- all_data[which(is.na(all_data$list)),]
  nrow(still_no_match) #624119
table(still_no_match$database)
sort(table(still_no_match$taxon_name))

# keep only rows for target taxa
all_data <- all_data[which(!is.na(all_data$list)),]
  nrow(all_data) #8026457

################################################################################
# C) Standardize some key columns
  ## this step could be donw in script 2, if we want to take the time to go through it and add column header files
################################################################################

# create localityDescription column
all_data <- all_data %>% unite("localityDescription",
  c(locality,verbatimLocality,municipality,higherGeography,county,stateProvince,
    country,countryCode),remove=F,sep=" | ") #na.rm=T,
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
  # convert to numeric
all_data$decimalLatitude <- as.numeric(all_data$decimalLatitude)
all_data$decimalLongitude <- as.numeric(all_data$decimalLongitude)
  # if coord is 0, set to NA
all_data$decimalLatitude[which(all_data$decimalLatitude==0)] <- NA
all_data$decimalLongitude[which(all_data$decimalLongitude==0)] <- NA
  nrow(all_data[which(is.na(all_data$decimalLatitude) |
    is.na(all_data$decimalLongitude)),]) #459896
  # flag non-numeric and not available coordinates and lat > 90, lat < -90,
  # lon > 180, and lon < -180
coord_test <- cc_val(all_data,lon = "decimalLongitude",lat = "decimalLatitude",
  value = "flagged", verbose = TRUE)
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
  geolocationNotes,localityDescription,county,stateProvince,country,countryCode,
  locationNotes,datasetName,publisher,nativeDatabaseID,references,
  informationWithheld,issue,taxon_name_full,list)

# create subsets for locality-only points and lat-long points, then
#   continue forward with just lat-long points
locality_pts <- all_data %>% filter(!is.na(localityDescription) &
  (is.na(decimalLatitude) | is.na(decimalLongitude))) %>%
  arrange(desc(year)) %>%
  distinct(species_name_acc,localityDescription,.keep_all=T)
  nrow(locality_pts) #212931
  write.csv(locality_pts,"need_geolocation.csv")
geo_pts <- all_data %>% filter(!is.na(decimalLatitude) &
  !is.na(decimalLongitude)) %>% select(-localityDescription)
  nrow(geo_pts) #7566561
  
##write out data to examine
    ##can write to a trial data folder (set in set_workngdirectory.R script if you prefer)
  write.csv(locality_pts, file.path(data_in, "need_geolocation.csv"), row.names=FALSE)
  # write.csv(locality_pts,"need_geolocation.csv")
  geo_pts <- all_data %>% filter(!is.na(decimalLatitude) &
                                   !is.na(decimalLongitude))
  nrow(geo_pts) #7562242
  
# ## writing a file out just to look it over
# write.csv(geo_pts[1:1000,], file.path(trial_data, "are_geolocated.csv"), col.names=TRUE)
  write.csv(all_data, file.path(trial_data, "all_data.csv"), col.names=TRUE, row.names=FALSE)
  write.csv(geo_pts, file.path(trial_data, "are_geolocated.csv"), col.names=TRUE, row.names=FALSE)
  
  

################################################################################
# D) Remove duplicates
################################################################################

# sort before removing duplicates
  # by year
geo_pts <- geo_pts %>% arrange(desc(year))
  # by dataset
geo_pts$database <- factor(geo_pts$database,
  levels = c("FIA","GBIF","US_Herbaria","iDigBio","BISON","BIEN"))
geo_pts <- geo_pts %>% arrange(database)
geo_pts$database <- as.character(geo_pts$database)

# create rounded latitude and longitude columns for removing dups
    ## this step may not be necessary due to other comparisons we are attempting
geo_pts$lat_round <- round(geo_pts$decimalLatitude,digits=3)
geo_pts$long_round <- round(geo_pts$decimalLongitude,digits=3)

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
nrow(geo_pts2) #1501081
table(geo_pts2$source_databases)
table(geo_pts2$database)

# take a look at results
count_geo <- geo_pts2 %>% count(species_name_acc)
count_geo <- setorder(count_geo,n)
names(count_geo)[2] <- "num_latlong_records"
count_locality <- locality_pts %>% count(species_name_acc)
count_locality <- setorder(count_locality,n)
names(count_locality)[2] <- "num_locality_records"
summary <- full_join(count_geo,count_locality)
write.csv(summary,"occurrence_point_count_per_species_new.csv")

##save data out to a file so don't have to rerun
save(all_data, taxon_list, s, geo_pts2, need_match, file=file.path(data_in, "EO_data.RData"))
  rm(all_data_raw, file_dfs, geo_pts, locality_pts, matched, need_match, source_standard)
# 
#   
#   head(all_data)

################################################################################
# E) Split by species
################################################################################
load(file.path(data_in, "EO_data.RData"))

# split
sp_split <- split(geo_pts2, as.factor(geo_pts2$species_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))

# write files
if(dir.exists(file.path(data_in, "raw_split_by_sp"))) print('directory already created') else
  dir.create(file.path(data_in, "raw_split_by_sp"), recursive=TRUE)
# dir.create(file.path(getwd(),"raw_split_by_sp"))
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
  paste0(file.path(data_in, "raw_split_by_sp", names(sp_split)[[i]]), ".csv")))
