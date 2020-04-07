### Author: Emily Beckman  ###  Date: 02/05/2020                                |

### DESCRIPTION:
  # This script provides instructions and code chunks for downloading wild
  #   occurrence points from:
    # GLOBAL DATABASES (though all likely have U.S. bias?)
      # Global Biodiversity Information Facility (GBIF)
      # Integrated Digitized Biocollections (iDigBio)
      # U.S. Herbarium Consortia (SERNEC, SEINet, etc.)
      # Botanical Information and Ecology Network (BIEN)
    # NATIONAL DATABASES
      # Forest Inventory and Analysis (FIA) Program of the USDA Forest Service

### INPUT:
  # target_taxa_with_syn.csv (list of target taxa)
    # columns:
      # 1. "taxon_name" (genus, species, infra rank, and infra name, all
      #    separated by one space each; hybrid symbol should be " x ", rather
      #    than "_" or "✕", and go between genus and species)
      # 2. (optional) other data you want to keep with taxa info

### OUTPUTS:
    # gbif_raw.csv
    # idigbio_raw.csv
    # herbaria_raw.csv
    # bien_raw.csv
    # fia_raw.csv

#################
### LIBRARIES ###
#################

library(plyr)
library(tidyverse) #ggplot2,dplyr,tidyr,readr,purrr,tibble,stringr,forcats
library(spocc)
library(rgbif)
library(data.table)
library(BIEN)
library(ridigbio)
library(batchtools)
library(googledrive)

################################################################################
# A) Read in target taxa list
################################################################################

setwd("./../..")
setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")

# read in taxa list
taxon_list <- read.csv("target_taxa_with_syn.csv", header = T,
  na.strings=c("","NA"), colClasses="character")

# filter out cultivars
taxon_list <- taxon_list %>% filter(is.na(taxon_type) |
  taxon_type != "cultivar") %>% filter(!is.na(taxon_name))
nrow(taxon_list) #784

# list of target taxon names
taxon_names <- taxon_list[,1]
# list of target species names
species_names <- unique(taxon_list[,2])

# create vector of target genera
target_genera <- c("Malus","Quercus","Tilia","Ulmus")

# set working directory to folder where you want the raw data
setwd("./raw_occurrence_point_data")

################################################################################
# B) Global Biodiversity Information Facility (GBIF) download
################################################################################

# GBIF account user information
  # if you don't have account yet, go to https://www.gbif.org then click
  #   "Login" in top right corner, then click "Register"
  # !!! FILL THIS IN WITH YOUR INFO:
user <- "user"
pwd <- "password"
email <- "email"

# get GBIF taxon keys for all taxa in target list
keys <- sapply(taxon_names,function(x) name_backbone(name=x)$speciesKey,
  simplify = "array")
# remove duplicate and NULL keys
keys_nodup <- keys[!duplicated(keys) & keys != "NULL"]
# create data frame of keys and matching taxon_name
gbif_codes <- map_df(keys_nodup,~as.data.frame(.x),.id="taxon_name")
names(gbif_codes)[2] <- "speciesKey"
# create vector of keys as input into gbif download
gbif_taxon_keys <- vector(mode="numeric")
for(i in 2:length(keys_nodup)){
  gbif_taxon_keys <- c(gbif_taxon_keys,keys_nodup[[i]][1])
}; sort(gbif_taxon_keys)

# download GBIF data (Darwin Core Archive format)
gbif_download <- occ_download(
                     pred_in("taxonKey", gbif_taxon_keys),
                     pred_in("basisOfRecord", c("PRESERVED_SPECIMEN",
                        "HUMAN_OBSERVATION","FOSSIL_SPECIMEN","OBSERVATION",
                        "UNKNOWN","MACHINE_OBSERVATION","MATERIAL_SAMPLE",
                        "LITERATURE")),
                     #pred("hasCoordinate", TRUE),
                     #pred("hasGeospatialIssue", FALSE),
                     format = "DWCA", #"SIMPLE_CSV"
                     user=user,pwd=pwd,
                     email=email)
# load gbif data just downloaded
  # create new folder for data and set as working directory
dir.create(file.path(getwd(),"gbif_read_in"))
setwd(file.path(getwd(),"gbif_read_in"))

# must wait for download to complete before continuing;
# it may take a while (up to 3 hours) if you have a large taxa list;
# you can check download status here: https://www.gbif.org/user/download

  # download and unzip before reading in
gbif_download # !!! PASTE "Download key" as first argument in next two lines !!!
occ_download_get(key="0031135-200221144449610", overwrite=TRUE)
unzip("0031135-200221144449610.zip")
  # read in data
gbif_raw <- fread("occurrence.txt",quote="")
nrow(gbif_raw) #2399719
# write file
setwd("./..")
write.csv(gbif_raw, "gbif_raw.csv")

################################################################################
# C) Integrated Digitized Biocollections (iDigBio) download
################################################################################

# I'm not sure the R interface actually gets all fields available; use manual
#    way down below (starts line 144) if you want to be sure

# download iDigBio data for target taxa
  # we have to go taxon by taxon; function can only return 100,000
  #   records at once and Quercus has more than that so can't download by genera
idigbio_raw <- data.frame()
for(i in 1:length(taxon_names)){
  output_new <- idig_search_records(rq=list(scientificname=taxon_names[[i]]),
    fields="all")
  idigbio_raw <- rbind(idigbio_raw,output_new)
  print(paste(round(i/length(taxon_names)*100,digits=1),"% complete",sep=""))
}
nrow(idigbio_raw) #153467
# remove rows that are lists
idigbio_raw <- idigbio_raw %>% select(everything(),-commonnames,-flags,
  -mediarecords,-recordids)
# write file
write.csv(idigbio_raw, "idigbio_raw.csv")

### OLD MANUAL WAY:
# First, download raw data
  # Go to https://www.idigbio.org/portal/search
  # Type your target genus name into the "Genus" box on the left side
  # Click the "Download" tab, type in your email, and click the download button
  #   (down arrow within circle)
# If you have more than one target genus, repeat the above steps for the
#   other genera
# Your downloads will pop up in the "Downloads" section;
# click "Click To Download" for each
# Move all the zipped files you downloaded into a "idigbio_read_in" folder
#   within your working directory
# Unzip each file and pull the "occurrence.csv" file out into the
#   "idigbio_read_in" folder -- obviously "keep both" when prompted
# read in raw occurrence points
#file_list <- list.files(path = "idigbio_read_in", pattern = ".csv",
#  full.names = T)
#file_dfs <- lapply(file_list, read.csv, colClasses = "character",
#  na.strings=c("","NA"),strip.white=T,fileEncoding="UTF-8")
#length(file_dfs) #4
# stack datasets to create one dataframe
#idigbio_raw <- data.frame()
#for(file in seq_along(file_dfs)){
#  idigbio_raw <- rbind(idigbio_raw, file_dfs[[file]])
#}; nrow(idigbio_raw) #316612
# write file
#write.csv(idigbio_raw, "idigbio_raw.csv")

################################################################################
# D) U.S. Herbaria Consortia (SERNEC, SEINet, etc.) download
################################################################################

# First, download raw data
  # Go to http://sernecportal.org/portal/collections/harvestparams.php
  # Type your target genus name into the "scientific name" box and click
  #   "List Display"
  # Click the Download Specimen Data button (arrow pointing down into a box),
  #   in the top right corner
  # In the pop-up window, select the "Darwin Core" radio button,
  #   uncheck everything in the "Data Extensions:" section, and
  #   select the "UTF-8 (unicode)" radio button
  #   leave other fields as-is
  # Click "Download Data"

# If you have more than one target genus, repeat the above steps for the
#   other genera

# Move all the zipped files you downloaded into a "sernec_read_in" folder
#   within your working directory
# Unzip each file and pull the "occurrences.csv" file out into the
#   "sernec_read_in" folder -- obviously "keep both" if prompted

# read in raw occurrence points
file_list <- list.files(path = "sernec_read_in", pattern = ".csv",
  full.names = T)
file_dfs <- lapply(file_list, read.csv, colClasses = "character",
  na.strings=c("","NA"),strip.white=T,fileEncoding="latin1")
length(file_dfs) #4
# stack datasets to create one dataframe
sernec_raw <- data.frame()
for(file in seq_along(file_dfs)){
  sernec_raw <- rbind(sernec_raw, file_dfs[[file]])
}; nrow(sernec_raw) #211506
# write file
write.csv(sernec_raw, "sernec_raw.csv")

################################################################################
# E) Botanical Information and Ecology Network (BIEN) download
################################################################################

# information about functions in package
#vignette("BIEN")

# download BIEN occurrence data for target genera
bien_raw <- BIEN_occurrence_genus(target_genera,all.taxonomy=T,native.status=T,
  natives.only=F,observation.type=T,collection.info=T,political.boundaries=T,
  cultivated=T)
nrow(bien_raw) #2508808
# write file
write.csv(bien_raw, "bien_raw.csv")

################################################################################
# F) USDA Forest Service, Forest Inventory and Analysis (FIA) download
################################################################################

# First, download raw data
  # Go to https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html
  # Either download the "TREE" file (e.g., "AL_TREE.csv") for each state
  #   (works well if you only need a few) or scroll to the bottom of the
  #   page and download "TREE.csv", which gives data for all states combined
  #   (9.73 GB); you need lots of memory to do it with just the one "TREE" file
  # Place all the tree files in an "fia_read_in" folder in your working
  #   directory
  # While you're on the FIA data download webpage, scroll to the bottom of
  #   the page and download the "PLOT.csv" file (data for all states combined)
  #   and place in your working directory

# read in FIA species codes
setwd("./..")
fia_codes <- read.csv("FIA_AppendixF_TreeSpeciesCodes_2016.csv",
  colClasses="character")
# join taxa list to FIA species codes
names(fia_codes) <- c("fia_code","fia_common_name","taxon_name","species_name")
  glimpse(fia_codes)
taxon_list <- left_join(taxon_list,fia_codes,by="species_name")
# make a list of unique FIA species codes to select from the data
species_codes <- sort(unique(taxon_list$fia_code))
sort(unique(taxon_list$taxon_name_acc[which(!is.na(taxon_list$fia_code))]))
length(species_codes) #56

# I have to read in each state file separately and pull info for our target
#   taxa then remove the file before loading the next state because memory
#   gets exhaused otherwise

# function to extract target species data from each state CSV
extract_tree_data <- function(file_name){
  # read in tree data, which lists all species and the plots in which they were
  #   found; larger ones will take time to read in
  state_df <- read.csv(file_name)
  # cycle through vector of target species codes and extract those rows from
  #   the state CSV
  for (sp in 1:length(species_codes)){
    target_sp <- state_df[which(state_df$SPCD==species_codes[[sp]]),]
    fia_raw <- rbind(fia_raw,target_sp)
    }
  # remove state file to make space for reading in next one
  rm(state_df)
  # take a look at how much data were pulled
  print(dim(fia_raw))
  return(fia_raw)
}

# make a new data frame to gather data for target taxa
fia_raw <- data.frame()
# create list of state files
file_list <- list.files(path = "raw_occurrence_point_data/fia_read_in",
  pattern = ".csv",full.names = T)
# loop through states and pull data using the function defined above
fia_outputs <- lapply(file_list, extract_tree_data)
  length(fia_outputs) #50
# stack state-by-state data extracted to create one dataframe
fia_raw <- data.frame()
for(file in seq_along(fia_outputs)){
  fia_raw  <- rbind(fia_raw , fia_outputs[[file]])
}; nrow(fia_raw) #3086137
# write file
write.csv(fia_raw,"raw_occurrence_point_data/fia_raw.csv")
