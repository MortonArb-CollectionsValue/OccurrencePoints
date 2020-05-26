### Author: Emily Beckman & Shannon Still ###  Date: 02/05/2020

### DESCRIPTION:
  # This script provides instructions and code chunks for downloading and
  #   standardizing wild occurrence points from:
    # GLOBAL DATABASES (though all likely have U.S. bias?)
      # Global Biodiversity Information Facility (GBIF)
      # Integrated Digitized Biocollections (iDigBio)
      # U.S. Herbarium Consortia (SERNEC, SEINet, etc.)
      # Botanical Information and Ecology Network (BIEN)
    # NATIONAL DATABASES
      # Forest Inventory and Analysis (FIA) Program of the USDA Forest Service
  ## NOTES: Not all data from these sources are reliable. The aim of this
  #         script is to get all easily-downloadable occurrence data, which
  #         can then be sorted and vetted for the user's specific purposes.

### INPUTS:
  # (optional) target_taxa_with_syn.csv
    # columns:
      # 1. "taxon_name" (genus, species, infra rank, and infra name, all
      #    separated by one space each; hybrid symbol should be " x ", rather
      #    than "_" or "✕", and go between genus and species)
      # 2. (optional) other data you want to keep with taxa info
  # extra files needed for FIA data download/standardization:
  #   FIA_AppendixF_TreeSpeciesCodes_2016.csv
  #   US_state_county_FIPS_codes.csv

### OUTPUTS:
    # gbif_raw.csv
    # idigbio_raw.csv
    # herbaria_raw.csv
    # bien_raw.csv
    # fia_raw.csv

#################
### LIBRARIES ###
#################

# rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'spocc', 'rgbif', 'data.table', 'BIEN',
                 'ridigbio', 'batchtools', 'googledrive', 'textclean','rbison')
# install.packages(my.packages) #Turn on to install current versions
          #ggplot2,dplyr,tidyr,readr,purrr,tibble,stringr,forcats
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

####################################################################################
####################################################################################
## set working directory
####################################################################################
# run code to set your working directory and project folders based upon computer using
      # skip this if preferred, but then need to set your working directory and input/output folders manually
# source('scripts/set_workingdirectory.R')

# If setting manually, set your working directory and data_in file paths in the next few lines
setwd("./../..")
setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")
data_in <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points"

## location for your login information for GBIF (or maybe other stuff) if using Otherwise comment out.
log_loc <- file.path("/Users/aesculus/Desktop/gbif.txt")

## create folders if they are npt already there
if(dir.exists(file.path(data_in, "raw_occurrence_point_data/gbif_read_in"))) print('directory already created') else
  dir.create(file.path(data_in, "raw_occurrence_point_data/gbif_read_in"), recursive=TRUE)

####################################################################################
####################################################################################
## load functions
####################################################################################
source('scripts/load_IMLS_functions.R')
####################################################################################


################################################################################
# A) Create list of target taxa
################################################################################

## IF YOU HAVE CSV OF TARGET TAXA AND SYNONYMS:
# read in taxa list
taxon_list <- read.csv(file.path(data_in, "target_taxa_with_syn.csv"), header = T,
                       na.strings=c("","NA"), colClasses="character")

# taxon_list <- read.csv("target_taxa_with_syn.csv", header = T,
#   na.strings=c("","NA"), colClasses="character")

head(taxon_list)

# filter out cultivars and blank rows
taxon_list <- taxon_list %>%
              filter(taxon_type != "cultivar" & !is.na(taxon_name))
nrow(taxon_list) #805

# list of target taxon names
taxon_names <- taxon_list$taxon_name

## IF JUST ONE OR A FEW TAXA, CREATE A LIST BY HAND:
#taxon_names <- "Quercus havardii"

################################################################################
# B) Download/compile data from each target database
################################################################################

# you can pick and choose any/all sections below, depending on which databases
# you'd like to use

###############
# 1) Global Biodiversity Information Facility (GBIF)
###############

# GBIF account user information
  # if you don't have account yet, go to https://www.gbif.org then click
  #   "Login" in top right corner, then click "Register"
  # !!! FILL THIS IN WITH YOUR INFO:

# user <- "ebeckman"
# pwd <- "Quercus51"
# email <- "ebeckman@mortonarb.org"

## I created a text file that contains 3 things on 3 successive lines: my username, password, and email address.
    ## By putting this in a specific location on my own computer, and not part of our gut, I can keep my login info separate.
    ## My login info is sourced in the set_workingdirectory.R script
login <- read_lines(log_loc)

user  <- login[1] #"user"
pwd   <- login[2] #"password"
email <- login[3] #"email"

# user <- "user"
# pwd <- "password"
# email <- "email"


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
for(i in 1:length(keys_nodup)){
  gbif_taxon_keys <- c(gbif_taxon_keys,keys_nodup[[i]][1])
}; sort(gbif_taxon_keys)

# download GBIF data (Darwin Core Archive format)
# download GBIF data (Darwin Core Archive format)
gbif_download <- occ_download(
                pred_in("taxonKey", gbif_taxon_keys),
                #pred_in("basisOfRecord", c("PRESERVED_SPECIMEN",
                #    "HUMAN_OBSERVATION","FOSSIL_SPECIMEN","OBSERVATION",
                #    "UNKNOWN","MACHINE_OBSERVATION","MATERIAL_SAMPLE",
                #    "LITERATURE")),
                #pred("hasCoordinate", TRUE),
                #pred("hasGeospatialIssue", FALSE),
                format = "DWCA", #"SIMPLE_CSV"
                user=user,pwd=pwd,
                email=email)
rm(user, pwd, email, login)

# load gbif data just downloaded
  # create new folder for data and set as working directory
## first, save regular wd
oldwd <- getwd()

setwd(file.path(data_in, "raw_occurrence_point_data/gbif_read_in"))
    # dir.create(file.path(getwd(),"raw_occurrence_point_data/gbif_read_in"))
    # setwd(file.path(getwd(),"raw_occurrence_point_data/gbif_read_in"))

# download and unzip before reading in
gbd <- gbif_download # !!! "Download key" can be acquired through this line and then pasted in the next two lines

# must wait for download to complete before continuing;
    # it may take a while (up to 3 hours) if you have a large taxa list;
    # you can check download status here: https://www.gbif.org/user/download
      ## -or-
  ## Since you sometimes need to wait to download
      ## There is a funtion that helps you to wait until the download is ready.
          ##this fxn occ_download_wait()

occ_download_wait(gbd, status_ping=10, quiet=TRUE)
occ_download_get(key=gbd[1], overwrite=TRUE)

unzip(paste0(gbd[1], '.zip'))

# read in data

gbif_raw <- fread("occurrence.txt", quote="", na.strings="")
# setwd("./../..")
setwd(oldwd); rm(oldwd)
nrow(gbif_raw) #2201869

### standardize column names

# create taxon_name column
subsp <- gbif_raw %>% filter(taxonRank == "SUBSPECIES")
  subsp$taxon_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",
    subsp$infraspecificEpithet)
var <- gbif_raw %>% filter(taxonRank == "VARIETY")
  var$taxon_name <- paste(var$genus,var$specificEpithet,"var.",
    var$infraspecificEpithet)
form <- gbif_raw %>% filter(taxonRank == "FORM")
  form$taxon_name <- paste(form$genus,form$specificEpithet,"f.",
    form$infraspecificEpithet)
spp <- gbif_raw %>% filter(taxonRank == "SPECIES")
  spp$taxon_name <- paste(spp$genus,spp$specificEpithet)
gbif_raw <- Reduce(rbind.fill,list(subsp,var,form,spp))

# keep only necessary columns
gbif_raw <- gbif_raw %>% select(
    # taxon name
  "taxon_name","scientificName",
  #"family","genus","specificEpithet","taxonRank",
  #  "infraspecificEpithet",
    # taxon IDs
  #"taxonID","speciesKey","taxonKey",
    # taxon identification notes (GROUP)
  "identificationRemarks","identificationVerificationStatus","identifiedBy",
    "taxonRemarks",
    # lat-long
  "decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters",
    # record details
  "basisOfRecord","year","gbifID","references",
    #"identifier","occurrenceID","recordNumber",
    # locality description (GROUP)
  "locality","verbatimLocality","county","municipality","stateProvince",
    "higherGeography","countryCode",
    # location notes (GROUP)
  "associatedTaxa","eventRemarks","fieldNotes","habitat","locationRemarks",
    "occurrenceRemarks","occurrenceStatus",
    # geolocation details (GROUP)
  "georeferencedBy","georeferencedDate",
    "georeferenceProtocol","georeferenceRemarks","georeferenceSources",
    "georeferenceVerificationStatus",
    # data source details
  "datasetName","publisher",#"collectionCode","institutionCode",
    # other caveats
  "establishmentMeans","informationWithheld","issue"
  #"dataGeneralizations","hasGeospatialIssues"
)
gbif_raw$database <- "GBIF"

# rename columns
gbif_raw <- gbif_raw %>% rename(nativeDatabaseID = gbifID)

# combine a few similar columns
gbif_raw <- gbif_raw %>% unite("taxonIdentificationNotes",
  identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
  gbif_raw$taxonIdentificationNotes <-
    gsub("^$",NA,gbif_raw$taxonIdentificationNotes)
gbif_raw <- gbif_raw %>% unite("locationNotes",
  associatedTaxa:occurrenceStatus,na.rm=T,remove=T,sep=" | ")
  gbif_raw$locationNotes <- gsub("^$",NA,gbif_raw$locationNotes)
gbif_raw <- gbif_raw %>% unite("geolocationNotes",
  georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
  gbif_raw$geolocationNotes <- gsub("^$",NA,gbif_raw$geolocationNotes)

# fix taxa names
gbif_raw$taxon_name <- mgsub(gbif_raw$taxon_name,
  c("Tilia xeuropaea","Tilia xvulgaris"),
  c("Tilia x europaea","Tilia x vulgaris"))

# create species_name column
gbif_raw$species_name <- NA
gbif_raw$species_name <- sapply(gbif_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(gbif_raw$species_name))

# check data
percent.filled(gbif_raw)
head(gbif_raw)

# write file
# write.csv(gbif_raw, "raw_occurrence_point_data/gbif_raw.csv")
write.csv(gbif_raw, file.path(data_in, "raw_occurrence_point_data/gbif_raw.csv"), row.names=FALSE)

################################################################################################################
##Shannon working here 4/21/2020
################################################################################################################
## We would like to do this if time permitting ##
  ## this section is to get the header column names from a file, then subset the table by the column names wanted, then set the colnames appropriate for that data.frame
  ## this can be repeated for each new data source
  ## we may have to move some of these steps around

# b.h <- read_excel('/Users/sstill/Box/Seed Menus Project/HerbariumRecords/HerbariumRecords_wHeaders/Burke_AllSpp_wHeaders.xlsx', col_types = 'text')
##this line sets the correct data.frame
  # n.h <- 'burke'

##this line selects the correct columns
  # nms  <- nm.set %>% filter(!is.na(!!as.name(n.h))) %>% select(!!as.name(n.h)) %>% as.character()
  # # nms1 <- nm.set %>% filter(!is.na(!!as.name(n.h))) %>% select(final_nm, !!as.name(n.h))

# b.h <- b.h %>% select_(nms)

##this line sets the correct column names
  # names(b.h) <- nms1$final_nm

##these lines add some data to fill in where missing
  # b.h <- b.h %>% mutate(dataSource=paste0('burke', sprintf("%05d", 1:nrow(b.h))), id=as.character(id), coordinateSystem=NA_character_, zone = NA_character_, x_coord = as.numeric(x_coord), y_coord = as.numeric(y_coord), coordinateUncertaintyMeters = as.numeric(coordinateUncertaintyMeters)); rm(n.h, nms, nms1)
  # b.h <- b.h %>% mutate(EPSG='undefined', data_type='herbarium voucher')
################################################################################################################

###############
# 2) Integrated Digitized Biocollections (iDigBio)
###############

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
nrow(idigbio_raw) #155359
# remove rows that are lists
idigbio_raw <- idigbio_raw %>% select(everything(),-commonnames,-flags,
  -mediarecords,-recordids)

# OLD MANUAL WAY:
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
#file_list <- list.files(path = "raw_occurrence_point_data/idigbio_read_in",
#  pattern = ".csv",full.names = T)
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

### standardize column names

# split date collected column to just get year
idigbio_raw <- idigbio_raw %>% separate("eventdate","year",sep="-",remove=T)
idigbio_raw$year <- gsub("[[:digit:]]+/[[:digit:]]+/","",idigbio_raw$year)
  sort(unique(idigbio_raw$year))

# keep only necessary columns
idigbio_raw$taxon_name <- idigbio_raw$scientificname
idigbio_raw <- idigbio_raw %>% select(
  "taxon_name","scientificname",
  #"family","genus","specificepithet","taxonrank","infraspecificepithet",
  #"taxonid",
  "geopoint.lon","geopoint.lat","coordinateuncertainty",
  "basisofrecord","year","uuid","occurrenceid",#"recordnumber",
  "locality","verbatimlocality","county","municipality","stateprovince",
    "country","countrycode",
  "institutioncode"#"collectioncode"
)

# rename columns
setnames(idigbio_raw,
  old = c("scientificname",
          #"specificepithet","taxonrank","infraspecificepithet",
          #"taxonid",
          "geopoint.lon","geopoint.lat",
          "coordinateuncertainty",
          "basisofrecord","uuid","occurrenceid",#"recordnumber",
          "verbatimlocality","stateprovince","countrycode",
          "institutioncode"),
  new = c("scientificName",
          #"specificEpithet","taxonRank","infraspecificEpithet",
          #"taxonID",
          "decimalLongitude","decimalLatitude",
          "coordinateUncertaintyInMeters",
          "basisOfRecord","nativeDatabaseID","references",#"recordNumber",
          "verbatimLocality","stateProvince","countryCode",
          "datasetName"),
  skip_absent=T)
idigbio_raw$database <- "iDigBio"

# fix taxa names
idigbio_raw$taxon_name <- str_to_sentence(idigbio_raw$taxon_name)
#idigbio_raw$family <- str_to_title(idigbio_raw$family)
#idigbio_raw$genus <- str_to_title(idigbio_raw$genus)

# create species_name column
idigbio_raw$species_name <- NA
idigbio_raw$species_name <- sapply(idigbio_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(idigbio_raw$species_name))

# recode standard columns
  # basis of record
idigbio_raw <- idigbio_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
    "preservedspecimen" = "PRESERVED_SPECIMEN",
    "machineobservation" = "MACHINE_OBSERVATION",
    "humanobservation" = "HUMAN_OBSERVATION",
    "fossilspecimen" = "FOSSIL_SPECIMEN",
    .missing = "UNKNOWN"))
  # year
idigbio_raw$year <- as.integer(idigbio_raw$year)
idigbio_raw$year[which(idigbio_raw$year < 1500)] <- NA

# check data
percent.filled(idigbio_raw)
head(idigbio_raw)

# write file
write.csv(idigbio_raw, file.path(data_in, "raw_occurrence_point_data/idigbio_raw.csv"), row.names=FALSE)
# write.csv(idigbio_raw, "raw_occurrence_point_data/idigbio_raw.csv")

###############
# 3) U.S. Herbaria Consortia (SERNEC, SEINet, etc.)
###############

# First, download raw data
  # Go to http://sernecportal.org/portal/collections/harvestparams.php
  # Type your target genus name into the "scientific name" box and click
  #   "List Display"; or, alternatively, if you are just looking for a few
  #   taxa you can search for and download them individually
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
#   "sernec_read_in" folder and rename with appropriate genus name

# read in raw occurrence points
file_list <- list.files(path = file.path(data_in, "raw_data/sernec_read_in"),
                        pattern = ".csv", full.names = T)
# file_list <- list.files(path = "raw_occurrence_point_data/sernec_read_in",
# pattern = ".csv", full.names = T)
file_dfs <- lapply(file_list, read.csv, colClasses = "character",
                   na.strings=c("", "NA"), strip.white=T, fileEncoding="latin1")
length(file_dfs) #4
# stack datasets to create one dataframe
sernec_raw <- data.frame()
for(file in seq_along(file_dfs)){
  sernec_raw <- rbind(sernec_raw, file_dfs[[file]])
}
nrow(sernec_raw) #211506

### standardize column names

# create taxon_name column
subsp <- sernec_raw %>% filter(taxonRank == "subsp.")
  subsp$taxon_name <- paste(subsp$genus,subsp$specificEpithet,"subsp.",
    subsp$infraspecificEpithet)
var <- sernec_raw %>% filter(taxonRank == "var.")
  var$taxon_name <- paste(var$genus,var$specificEpithet,"var.",
    var$infraspecificEpithet)
form <- sernec_raw %>% filter(taxonRank == "f.")
  form$taxon_name <- paste(form$genus,form$specificEpithet,"f.",
    form$infraspecificEpithet)
hybrid <- sernec_raw %>% filter(taxonRank == "x" | taxonRank == "X")
  hybrid$taxon_name <- paste(hybrid$genus,hybrid$specificEpithet,"x",
    hybrid$infraspecificEpithet)
spp <- sernec_raw %>% filter(is.na(taxonRank))
  spp$taxon_name <- paste(spp$genus,spp$specificEpithet)
sernec_raw <- Reduce(rbind.fill,list(subsp,var,form,hybrid,spp))
sernec_raw$taxon_name[which(is.na(sernec_raw$taxon_name))] <-
  sernec_raw$scientificName[which(is.na(sernec_raw$taxon_name))]
sort(unique(sernec_raw$taxon_name))
sernec_raw$taxon_name <- gsub("Ã\u0097","",sernec_raw$taxon_name)
sernec_raw$taxon_name <- gsub("Ã«","e",sernec_raw$taxon_name)
sernec_raw$taxon_name <- str_squish(sernec_raw$taxon_name)

# keep only necessary columns
sernec_raw <- sernec_raw %>% select(
  "taxon_name",
  #"family","genus","specificEpithet","taxonRank","infraspecificEpithet",
    "scientificName",
  #"taxonID",
  "identificationRemarks","identifiedBy","taxonRemarks",
  "decimalLatitude","decimalLongitude",
  "coordinateUncertaintyInMeters",
  "basisOfRecord","year","id","references",#"occurrenceID","recordNumber",
  "locality","county","municipality","stateProvince","country",
  "associatedTaxa","habitat","locationRemarks","occurrenceRemarks",
  "georeferencedBy","georeferenceProtocol","georeferenceRemarks",
    "georeferenceSources","georeferenceVerificationStatus",
  "institutionCode",
  #"collectionCode",
  "establishmentMeans","informationWithheld")
sernec_raw$database <- "US_Herbaria"

# rename columns
sernec_raw <- sernec_raw %>% rename(nativeDatabaseID = id)
sernec_raw <- sernec_raw %>% rename(datasetName = institutionCode)

# combine a few similar columns
sernec_raw <- sernec_raw %>% unite("taxonIdentificationNotes",
  identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
  sernec_raw$taxonIdentificationNotes <-
    gsub("^$",NA,sernec_raw$taxonIdentificationNotes)
sernec_raw <- sernec_raw %>% unite("locationNotes",
  associatedTaxa:occurrenceRemarks,na.rm=T,remove=T,sep=" | ")
  sernec_raw$locationNotes <- gsub("^$",NA,sernec_raw$locationNotes)
sernec_raw <- sernec_raw %>% unite("geolocationNotes",
  georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
  sernec_raw$geolocationNotes <- gsub("^$",NA,sernec_raw$geolocationNotes)

# create species_name column
sernec_raw$species_name <- NA
sernec_raw$species_name <- sapply(sernec_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(sernec_raw$species_name))

# recode standard columns
  # basis of record
sernec_raw <- sernec_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
    "PreservedSpecimen" = "PRESERVED_SPECIMEN",
    "Preserved specimen" = "PRESERVED_SPECIMEN",
    "Preserved Specimen" = "PRESERVED_SPECIMEN",
    "Physicalspecimen" = "PRESERVED_SPECIMEN",
    "preservedspecimen" = "PRESERVED_SPECIMEN",
    "preservedSpecimen" = "PRESERVED_SPECIMEN",
    "Observation" = "OBSERVATION",
    "LivingSpecimen" = "LIVING_SPECIMEN",
    "Physical specimen" = "PRESERVED_SPECIMEN",
    "Ejemplar herborizado" = "PRESERVED_SPECIMEN",
    "Physical Specimen" = "PRESERVED_SPECIMEN",
    "HumanObservation" = "HUMAN_OBSERVATION",
    .default = "UNKNOWN"))
  # year
sort(unique(sernec_raw$year))
sernec_raw <- sernec_raw %>%
  mutate(year = recode(year,
    "9999" = "0",
    "18914" = "1891",
    "19418" = "1941"))
sernec_raw$year <- as.integer(sernec_raw$year)
sernec_raw$year[which(sernec_raw$year < 1500)] <- NA
sort(unique(sernec_raw$year))
  # establishment means
sort(unique(sernec_raw$establishmentMeans))
sernec_raw <- sernec_raw %>%
  mutate(establishmentMeans = recode(establishmentMeans,
    "Alien" = "INTRODUCED",
    "clonal" = "UNKNOWN",
    "Native" = "NATIVE",
    "Native." = "NATIVE",
    "native" = "NATIVE",
    "Wild." = "NATIVE",
    "Naturalized." = "INTRODUCED",
    "Uncertain" = "UNKNOWN",
    "wild caught" = "UNKNOWN",
    .default = "MANAGED"))

# check data
percent.filled(sernec_raw)
head(sernec_raw)

# write file
write.csv(sernec_raw, file.path(data_in, "raw_occurrence_point_data/sernec_raw.csv"), row.names=FALSE)
# write.csv(sernec_raw, "raw_occurrence_point_data/sernec_raw.csv")

###############
# 4) Botanical Information and Ecology Network (BIEN)
###############

# information about functions in package
#vignette("BIEN")

# download BIEN occurrence data for target taxa
bien_raw <- BIEN_occurrence_species(taxon_names,all.taxonomy=T,native.status=T,
  natives.only=F,observation.type=T,collection.info=T,political.boundaries=T,
  cultivated=T)
nrow(bien_raw) #2199972

### standardize column names

# split date collected column to just get year
bien_raw <- bien_raw[,-24]
bien_raw <- bien_raw %>% separate("date_collected","year",sep="-",remove=T)
  sort(unique(bien_raw$year))

# keep only necessary columns
bien_raw <- bien_raw %>% select(
  "name_matched","verbatim_scientific_name",
  #"scrubbed_family","scrubbed_genus",
  "identified_by","identification_remarks","date_identified",
  "latitude","longitude",
  "observation_type","year","record_number",
  "locality","county","state_province","country",
  #"collection_code",
  "dataset","datasource",
  "is_cultivated_observation")

# rename columns
setnames(bien_raw,
  old = c("name_matched","verbatim_scientific_name",
          #"scrubbed_family","scrubbed_genus",
          "latitude","longitude",
          "observation_type","record_number",
          "state_province",
          #"collection_code",
          "dataset","datasource"),
  new = c("taxon_name","scientificName",
          #"family","genus",
          "decimalLatitude","decimalLongitude",
          "basisOfRecord","nativeDatabaseID",
          "stateProvince",
          #"collectionCode",
          "datasetName","publisher"),
  skip_absent=T)
bien_raw$database <- "BIEN"

# combine a few similar columns
bien_raw <- bien_raw %>% unite("taxonIdentificationNotes",
  identified_by:date_identified,na.rm=T, remove=T, sep=" | ")
  bien_raw$taxonIdentificationNotes <-
    gsub("^$", NA, bien_raw$taxonIdentificationNotes)

# fix taxa names
bien_raw$taxon_name <- gsub(" fo. "," f. ",bien_raw$taxon_name)
bien_raw$taxon_name <- str_squish(bien_raw$taxon_name)

# create species_name column
bien_raw$species_name <- NA
bien_raw$species_name <- sapply(bien_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(bien_raw$species_name))

# recode standard columns
  # basis of record
sort(unique(bien_raw$basisOfRecord))
bien_raw <- bien_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
    "literature" = "LITERATURE",
    "plot" = "OBSERVATION",
    "specimen" = "PRESERVED_SPECIMEN"))
  # establishment means
bien_raw$is_cultivated_observation <- as.character(
  bien_raw$is_cultivated_observation)
bien_raw <- bien_raw %>%
  mutate(establishmentMeans = recode(is_cultivated_observation,
    "1" = "MANAGED",
    "0" = "UNKNOWN",
    .missing = "UNKNOWN")) %>%
  select(-is_cultivated_observation)

# check data
percent.filled(bien_raw)
head(bien_raw)

# write file
write.csv(bien_raw, file.path(data_in, "raw_occurrence_point_data/bien_raw.csv"), row.names=FALSE)
# write.csv(bien_raw, "raw_occurrence_point_data/bien_raw.csv")

###############
# 5) USDA Forest Service, Forest Inventory and Analysis (FIA)
###############

# First, download raw data
  # Go to https://apps.fs.usda.gov/fia/datamart/CSV/datamart_csv.html
  # Either download the "TREE" CSV file (e.g., "AL_TREE.csv") for each state
  #   or scroll to the bottom of the page and download "TREE.csv", which gives
  #   data for all states combined (9.73 GB);
  #   you need lots of memory to do it with just the one "TREE" file,
  #   so the script below uses the state files and cycles through each
  # Place all the tree files in an "fia_read_in" folder in your working
  #   directory
  # While you're on the FIA data download webpage, scroll to the bottom of
  #   the page and download the "PLOT.csv" file (data for all states combined)
  #   and place in your working directory

# read in FIA species codes
fia_codes <- read.csv(file.path(data_in, "FIA_tables/FIA_AppendixF_TreeSpeciesCodes_2016.csv"),
  colClasses="character")
# join taxa list to FIA species codes
fia_codes <- fia_codes[,c(1,3)]
names(fia_codes) <- c("SPCD","taxon_name")
  glimpse(fia_codes)
taxon_fia <- fia_codes[which(fia_codes$taxon_name %in% taxon_names),]
# make a list of unique FIA species codes to select from the data
species_codes <- sort(unique(taxon_fia$SPCD))
# check results
sort(unique(taxon_fia$taxon_name[which(
  !is.na(taxon_fia$SPCD))]))
length(species_codes) #56

# I have to read in each state file separately and pull info for our target
#   taxa then remove the file before loading the next state because memory
#   gets exhaused otherwise

# function to extract target species data from each state CSV
extract_tree_data <- function(file_name){
  data <- data.frame()
  # read in tree data, which lists all species and the plots in which they were
  #   found; larger ones will take time to read in
  state_df <- read.csv(file_name)
  # cycle through vector of target species codes and extract those rows from
  #   the state CSV
  for (sp in 1:length(species_codes)){
    target_sp <- state_df[which(state_df$SPCD==species_codes[[sp]]),]
    data <- rbind(data,target_sp)
    }
  # remove state file to make space for reading in next one
  rm(state_df)
  # take a look at how much data were pulled
  print(paste(nrow(data),file_name))
  return(data)
}

# create list of state files
file_list <- list.files(path = "raw_occurrence_point_data/fia_read_in",
  pattern = ".csv",full.names = T)
# loop through states and pull data using the function defined above
fia_outputs <- lapply(file_list, extract_tree_data)
  length(fia_outputs) #50
# stack state-by-state data extracted to create one dataframe
fia_raw <- data.frame()
for(file in seq_along(fia_outputs)){
  fia_raw  <- rbind(fia_raw, fia_outputs[[file]])
}
nrow(fia_raw) #3312303

### standardize column names

# read in supplemental FIA tables:
#  - list of species tracked and their codes (read in above)
#  - state and county codes and names
#  - plot level data (has lat-long)
county_codes <- read.csv("FIA_tables/US_state_county_FIPS_codes.csv",header = T,
  na.strings=c("","NA"), colClasses="character")
fia_plots <- read.csv("FIA_tables/PLOT.csv")
  # remove unnecessary columns from plot data
  fia_plots <- fia_plots[,c("INVYR","STATECD","UNITCD","COUNTYCD","PLOT",
                            "LAT","LON")]
# join FIA data to supplemental tables
fia_raw <- join(fia_raw,fia_codes)
fia_raw <- join(fia_raw,county_codes)
fia_raw <- join(fia_raw,fia_plots)
# create ID column
fia_raw <- fia_raw %>% unite("fiaPlotID",
  c("INVYR","UNITCD","COUNTYCD","PLOT","STATECD"),remove=F,sep="-")

# keep only necessary columns
fia_raw <- fia_raw %>% select(
  "taxon_name",
  "LAT","LON",
  "INVYR","fiaPlotID",
  "STATUSCD")

# rename columns
setnames(fia_raw,
  old = c("LAT","LON",
          "INVYR",
          "STATUSCD","fiaPlotID"),
  new = c("decimalLatitude","decimalLongitude",
          "year",
          "isAlive","nativeDatabaseID"),
  skip_absent=T)
fia_raw$database <- "FIA"

# create species_name column
fia_raw$species_name <- NA
fia_raw$species_name <- sapply(fia_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(fia_raw$species_name))

# recode standard columns
  # establishment means
fia_raw$isAlive <- as.character(fia_raw$isAlive)
fia_raw <- fia_raw %>%
  mutate(establishmentMeans = recode(isAlive,
    "1" = "UNKNOWN",
    "2" = "CUT",
    "3" = "DEAD",
    "0" = "UNKNOWN"))
fia_raw <- fia_raw %>% select(-isAlive)
  # basis of record
fia_raw$basisOfRecord <- "OBSERVATION"
  # year
fia_raw$year[which(fia_raw$year == 9999)] <- NA

# check data
percent.filled(fia_raw)
head(fia_raw)

# write file
write.csv(fia_raw, file.path(data_in, "raw_occurrence_point_data/fia_raw.csv"), row.names=FALSE)
# write.csv(fia_raw,"raw_occurrence_point_data/fia_raw.csv")

###############
# 6) Biodiversity Information Serving Our Nation (BISON), USGS
###############

# download BISON occurrence data for target taxa
#   there is also county distribution data
# if loop gets hung up, generally best to just try again instead of waiting
bison_raw <- data.frame()
us_cty_dist <- data.frame()
for(i in 1:length(taxon_names)){
  occ <- bison(species = taxon_names[i])
  bison_raw <- rbind.fill(bison_raw,occ$points)
  if(length(occ$counties>0)){
    occ$counties$taxon_name <- taxon_names[i]
    us_cty_dist <- rbind.fill(us_cty_dist,occ$counties)
  }
  print(taxon_names[i])
}
nrow(bison_raw) #4008

### standardize column names

# keep only necessary columns
bison_raw <- bison_raw %>% select(
  "name",
  "decimalLatitude","decimalLongitude",
  "basis","occurrenceID",
  "provider")

# rename columns
setnames(bison_raw,
  old = c("name",
          "basis","occurrenceID",
          "provider"),
  new = c("taxon_name",
          "basisOfRecord","nativeDatabaseID",
          "datasetName"),
  skip_absent=T)
bison_raw$database <- "BISON"

# create species_name column
bison_raw$species_name <- NA
bison_raw$species_name <- sapply(bison_raw$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(bison_raw$species_name))

# recode standard columns
  # basis of record
sort(unique(bison_raw$basisOfRecord))
bison_raw <- bison_raw %>%
  mutate(basisOfRecord = recode(basisOfRecord,
    "Living" = "LIVING_SPECIMEN",
    "Observation" = "HUMAN_OBSERVATION",
    "Specimen" = "PRESERVED_SPECIMEN",
    "Fossil" = "FOSSIL_SPECIMEN",
    "Unknown" = "UNKNOWN"))

# check data
percent.filled(bison_raw)
head(bison_raw)

# write file
write.csv(bison_raw, file.path(data_in, "raw_occurrence_point_data/bison_raw.csv"), row.names=FALSE)
# write.csv(bison_raw, "raw_occurrence_point_data/bison_raw.csv")

# write file of county distribution
write.csv(us_cty_dist, file.path(data_in, "BISON_US_county_distribution.csv"), row.names=FALSE)
# write.csv(us_cty_dist, "BISON_US_county_distribution.csv")
