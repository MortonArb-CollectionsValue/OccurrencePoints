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
taxon_list <- read.csv("target_taxa_with_syn.csv", header = T,
  na.strings=c("","NA"), colClasses="character")

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








library(sp); library(ggplot2); library(maps)

path.imls <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"
path.pts <- file.path(path.imls, "insitu_occurrence_points/raw_split_by_sp")
#path.figs <- file.path(path.imls, "Environmental Niche Value", "figures")
#path.data <- file.path(path.imls, "Environmental Niche Value", "data")

#if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)
#if(!dir.exists(path.figs)) dir.create(path.data, recursive=T)
#if(!dir.exists(file.path(path.figs, "maps"))) dir.create(file.path(path.figs, "maps"), recursive=T)

#arb.lat=41.812739
#arb.lon=-88.072749

map.world <- map_data("world")
map.world2 <- map_data("world2")
map.us <- map_data("state")

spp.test <- c("Quercus georgiana", "Quercus imbricaria", "Quercus arkansana", "Quercus falcata", "Quercus stellata", "Quercus acutissima")

spp.all <- dir(path.pts, ".csv")

for(i in 1:length(spp.test)){
  fnow <- grep(sub(" ", "_", spp.test[i]), spp.all)

  dat.now <- read.csv(file.path(path.pts, spp.all[fnow]))
  summary(dat.now)

  # we have some VERY odd locations... FIA and BIEN seem to be the biggest offenders
  #  FIA should only be in the northern hemisphere (US), so assume everything negative is backwards
  #  BIEN is a bit different... BIEN looks like it has a LOT of redundancy, so lets just get rid of it
  #  Not feeling like "preserved specimens" are actually located to places
  #  Also highly questioning the reliability of iNaturalist observations -- I think they're REALLY skewing Q. acutissima
  dat.now <- dat.now[!is.na(dat.now$decimalLatitude),]
  if(nrow(dat.now)==0) next

  dat.now <- dat.now[dat.now$decimalLatitude!=0 & dat.now$decimalLongitude!=0,] # get rid of things with 0,0 location
  dat.now <- dat.now[!dat.now$database %in% c("BIEN"),] # Lets just get rid of BIEN -- it looks like lots of overlap
  dat.now <- dat.now[!dat.now$basisOfRecord %in% c("PRESERVED_SPECIMEN"),] # This seems odd too and probably not an actual occurrence point
  dat.now <- dat.now[!dat.now$publisher %in% ("iNaturalist.org"),]
  rows.bad <- which((dat.now$database=="FIA" & dat.now$decimalLatitude < 0) ) #
  dat.now[rows.bad, c("decimalLongitude", "decimalLatitude")] <- dat.now[rows.bad, c("decimalLatitude", "decimalLongitude")]
  dat.now[dat.now$decimalLatitude<0,c("decimalLongitude", "decimalLatitude", "database")]

  png(file.path(path.figs, "maps", paste0(sub(" ", "_", spp.test[i]), "_adj.png")), height=6, width=10, units="in", res=180)
  print(


  dat.now <- read.csv(file.path(path.pts, spp.all[1]))

  ggmap() +
    coord_equal() +
    ggtitle(spp.test[1]) +
    geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
    geom_point(data=dat.now, aes(x=decimalLongitude, y=decimalLatitude, color=database), size=2) +
    #scale_x_continuous(expand=c(0,0)) +
    #scale_y_continuous(expand=c(0,0)) +
    theme_minimal()


  )
  dev.off()




}
