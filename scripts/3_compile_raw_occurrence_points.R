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
      # 2. (optional) "taxon_name_acc" (accepted taxon name you have chosen)
      # 3+ (optional) other data you want to keep with taxa info

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
library(data.table)
library(batchtools)
library(textclean)


#################
### FUNCTIONS ###
#################

# searches for data frame columns with only NAs and removes them
remove.empty.col <- function(df){
  remove <- vector(mode = "character")
  for(i in 1:ncol(df)){
    if(sum(is.na(df[,i])) == nrow(df)){
      remove <- c(remove,names(df)[i])
      print(names(df)[i])
    }
  }
  if(length(remove)>0){
    df <-  df[,-which(names(df) %in% remove)]
  }
  return(df)
}

# calculates percent of each data frame column that is not NA
percent.filled <- function(df){
  for(i in 1:ncol(df)){
    print(paste(names(df)[i],": ",
      round((nrow(df)-sum(is.na(df[,i])))/nrow(df),3)*100,"%",sep=""))
  }
}

################################################################################
# 1. Read in target taxa list and raw occurrence data
################################################################################

setwd("./../..")
setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")

taxon_list <- read.csv("target_taxa_with_syn.csv", header = T,
  na.strings=c("","NA"), colClasses="character")
gbif_raw <- read.csv("raw_occurrence_point_data/gbif_raw.csv",header=T,
  na.strings=c("","NA"),stringsAsFactors=F)
  gbif_raw <- remove.empty.col(gbif_raw) #; percent.filled(gbif_raw)
idigbio_raw <- read.csv("raw_occurrence_point_data/idigbio_raw.csv",header=T,
  na.strings=c("","NA"),stringsAsFactors=F)
  idigbio_raw <- remove.empty.col(idigbio_raw) #; percent.filled(idigbio_raw)
sernec_raw <- read.csv("raw_occurrence_point_data/sernec_raw.csv",header=T,
  na.strings=c("","NA"),stringsAsFactors=F)
  sernec_raw <- remove.empty.col(sernec_raw) #; percent.filled(sernec_raw)
bien_raw <- read.csv("raw_occurrence_point_data/bien_raw.csv",header=T,
  na.strings=c("","NA"),stringsAsFactors=F)
  bien_raw <- remove.empty.col(bien_raw) #; percent.filled(bien_raw)
fia_raw <- read.csv("raw_occurrence_point_data/fia_raw.csv",header=T,
  na.strings=c("","NA"),stringsAsFactors=F)
  fia_raw <- remove.empty.col(fia_raw) #; percent.filled(fia_raw)

################################################################################
# 2. Subset and standardize column names
################################################################################

# GBIF

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
gbif_raw2 <- gbif_raw %>% select(
    #1 taxon name
  "taxon_name",
  "family","genus","specificEpithet","taxonRank",
    "infraspecificEpithet","scientificName",
    #2 taxon IDs
  "taxonID","speciesKey","taxonKey",
    #3 taxon identification notes (GROUP)
  "identificationRemarks","identificationVerificationStatus","identifiedBy",
    "taxonRemarks",
    #4 lat-long
  "decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters",
    #5 record details
  "basisOfRecord","year","gbifID","identifier","occurrenceID","recordNumber",
    #6 locality description (GROUP)
  "locality","county","municipality","stateProvince","higherGeography",
    "countryCode",
    #6 location notes (GROUP)
  "associatedTaxa","eventRemarks","fieldNotes","habitat","locationRemarks",
    "occurrenceRemarks","occurrenceStatus",
    #7 geolocation details (GROUP)
  "georeferencedBy","georeferencedDate",
    "georeferenceProtocol","georeferenceRemarks","georeferenceSources",
    "georeferenceVerificationStatus",
    #8 data source details
  "collectionCode","datasetName","institutionCode","publisher",
    #9 other caveats
  "dataGeneralizations","establishmentMeans","hasGeospatialIssues",
    "informationWithheld","issue")
gbif_raw2$dataset <- "GBIF"
head(gbif_raw2)
# combine a few similar columns
gbif_raw2 <- gbif_raw2 %>% unite("taxonIdentificationNotes",
  identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
  gbif_raw2$taxonIdentificationNotes <-
    gsub("^$",NA,gbif_raw2$taxonIdentificationNotes)
gbif_raw2 <- gbif_raw2 %>% unite("localityDescription",
  locality:countryCode,na.rm=T,remove=T,sep=" | ")
  gbif_raw2$localityDescription <- gsub("^$",NA,gbif_raw2$localityDescription)
gbif_raw2 <- gbif_raw2 %>% unite("locationNotes",
  associatedTaxa:occurrenceStatus,na.rm=T,remove=T,sep=" | ")
  gbif_raw2$locationNotes <- gsub("^$",NA,gbif_raw2$locationNotes)
gbif_raw2 <- gbif_raw2 %>% unite("geolocationNotes",
  georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
  gbif_raw2$geolocationNotes <- gsub("^$",NA,gbif_raw2$geolocationNotes)
# create species_name column
gbif_raw2$species_name <- NA
gbif_raw2$species_name <- sapply(gbif_raw2$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(gbif_raw2$species_name))
# check data
percent.filled(gbif_raw2)
head(gbif_raw2)

# iDigBio

# split eventdate column to just get year
idigbio_raw2 <- idigbio_raw %>% separate("eventdate","year",sep="-",remove=T)
idigbio_raw2$year <- gsub("[[:digit:]]+/[[:digit:]]+/","",idigbio_raw2$year)
  sort(unique(idigbio_raw2$year))
idigbio_raw2$year <- as.numeric(idigbio_raw2$year)
idigbio_raw2$year[which(idigbio_raw2$year < 1000)] <- NA
# keep only necessary columns
idigbio_raw2$taxon_name <- idigbio_raw2$scientificname
idigbio_raw2 <- idigbio_raw2 %>% select(
  "taxon_name",
  "family","genus","specificepithet","taxonrank","infraspecificepithet",
    "scientificname",
  "taxonid",
  "geopoint.lon","geopoint.lat","coordinateuncertainty",
  "basisofrecord","year","occurrenceid","recordnumber","uuid",
  "locality","county","municipality","stateprovince","country","countrycode",
  "collectioncode","institutioncode")
# rename columns
setnames(idigbio_raw2,
  old = c("scientificname",
          "specificepithet","taxonrank","infraspecificepithet",
          "taxonid",
          "geopoint.lon","geopoint.lat",
          "basisofrecord","occurrenceid","recordnumber","uuid",
          "coordinateuncertainty",
          "collectioncode","institutioncode"),
  new = c("scientificName",
          "specificEpithet","taxonRank","infraspecificEpithet",
          "taxonID",
          "decimalLongitude","decimalLatitude",
          "basisOfRecord","occurrenceID","recordNumber","idigbioID",
          "coordinateUncertaintyInMeters",
          "collectionCode","institutionCode"),
  skip_absent=T)
idigbio_raw2$dataset <- "iDigBio"
# combine a few similar columns
idigbio_raw2 <- idigbio_raw2 %>% unite("localityDescription",
  locality:countrycode,na.rm=T,remove=T,sep=" | ")
  idigbio_raw2$localityDescription <-
    gsub("^$",NA,idigbio_raw2$localityDescription)
# fix capitalization issues
idigbio_raw2$taxon_name <- str_to_sentence(idigbio_raw2$taxon_name)
idigbio_raw2$family <- str_to_title(idigbio_raw2$family)
idigbio_raw2$genus <- str_to_title(idigbio_raw2$genus)
# create species_name column
idigbio_raw2$species_name <- NA
idigbio_raw2$species_name <- sapply(idigbio_raw2$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(idigbio_raw2$species_name))
# check data
percent.filled(idigbio_raw2)
head(idigbio_raw2)

# US_Herbaria

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
sernec_raw2 <- sernec_raw %>% select(
  "taxon_name",
  "family","genus","specificEpithet","taxonRank","infraspecificEpithet",
    "scientificName",
  "taxonID",
  "identificationRemarks","identifiedBy","taxonRemarks",
  "coordinateUncertaintyInMeters",
  "decimalLatitude","decimalLongitude",
  "basisOfRecord","year","id","occurrenceID","recordNumber",
  "locality","county","municipality","stateProvince","country",
  "associatedTaxa","habitat","locationRemarks","occurrenceRemarks",
  "georeferencedBy","georeferenceProtocol","georeferenceRemarks",
    "georeferenceSources","georeferenceVerificationStatus",
  "collectionCode","institutionCode",
  "establishmentMeans","informationWithheld")
sernec_raw2$dataset <- "US_Herbaria"
# rename column
sernec_raw2 <- sernec_raw2 %>% rename(sernecID = id)
# combine a few similar columns
sernec_raw2 <- sernec_raw2 %>% unite("taxonIdentificationNotes",
  identificationRemarks:taxonRemarks,na.rm=T,remove=T,sep=" | ")
  sernec_raw2$taxonIdentificationNotes <-
    gsub("^$",NA,sernec_raw2$taxonIdentificationNotes)
sernec_raw2 <- sernec_raw2 %>% unite("localityDescription",
  locality:country,na.rm=T,remove=T,sep=" | ")
  sernec_raw2$localityDescription <-
    gsub("^$",NA,sernec_raw2$localityDescription)
sernec_raw2 <- sernec_raw2 %>% unite("locationNotes",
  associatedTaxa:occurrenceRemarks,na.rm=T,remove=T,sep=" | ")
  sernec_raw2$locationNotes <- gsub("^$",NA,sernec_raw2$locationNotes)
sernec_raw2 <- sernec_raw2 %>% unite("geolocationNotes",
  georeferencedBy:georeferenceVerificationStatus,na.rm=T,remove=T,sep=" | ")
  sernec_raw2$geolocationNotes <- gsub("^$",NA,sernec_raw2$geolocationNotes)
# create species_name column
sernec_raw2$species_name <- NA
sernec_raw2$species_name <- sapply(sernec_raw2$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(sernec_raw2$species_name))
# check data
percent.filled(sernec_raw2)
head(sernec_raw2)

# BIEN

# split date_collected column to just get year
bien_raw2 <- bien_raw %>% separate("date_collected","year",sep="-",remove=T)
  sort(unique(bien_raw2$year))
# keep only necessary columns
bien_raw2 <- bien_raw2 %>% select(
  "name_matched",
  "scrubbed_family","scrubbed_genus","verbatim_scientific_name",
  "identified_by","identification_remarks","date_identified",
  "latitude","longitude",
  "observation_type","year","record_number",
  "locality","county","state_province","country",
  "collection_code","dataset","datasource",
  "is_cultivated_observation")
# rename columns
setnames(bien_raw2,
  old = c("name_matched",
          "scrubbed_family","scrubbed_genus","verbatim_scientific_name",
          "latitude","longitude",
          "observation_type","record_number",
          "collection_code","dataset","datasource"),
  new = c("taxon_name",
          "family","genus","scientificName",
          "decimalLongitude","decimalLatitude",
          "basisOfRecord","recordNumber",
          "collectionCode","institutionCode","publisher"),
  skip_absent=T)
bien_raw2$dataset <- "BIEN"
# fix taxon name column
bien_raw2$taxon_name <- gsub(" fo. "," f. ",bien_raw2$taxon_name)
bien_raw2$taxon_name <- str_squish(bien_raw2$taxon_name)
# combine a few similar columns
bien_raw2 <- bien_raw2 %>% unite("taxonIdentificationNotes",
  identified_by:date_identified,na.rm=T,remove=T,sep=" | ")
  bien_raw2$taxonIdentificationNotes <-
    gsub("^$",NA,bien_raw2$taxonIdentificationNotes)
bien_raw2 <- bien_raw2 %>% unite("localityDescription",
  locality:country,na.rm=T,remove=T,sep=" | ")
  bien_raw2$localityDescription <- gsub("^$",NA,bien_raw2$localityDescription)
# create species_name column
bien_raw2$species_name <- NA
bien_raw2$species_name <- sapply(bien_raw2$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(bien_raw2$species_name))
# check data
percent.filled(bien_raw2)
head(bien_raw2)

# FIA

# read in supplemental FIA tables:
#  - list of species tracked and their codes
#  - state and county codes and names
#  - plot level data (has lat-long)
fia_codes <- read.csv("FIA_AppendixF_TreeSpeciesCodes_2016.csv", header = T,
  na.strings=c("","NA"), colClasses="character")
#fia_codes <- read.csv("FIA_AppendixF_TreeSpeciesCodes_2016.csv")
  # remove unnecessary columns and rename species name column
  fia_codes <- fia_codes %>% select(SPCD,taxon_name,species_name)
county_codes <- read.csv("US_state_county_FIPS_codes.csv", header = T,
  na.strings=c("","NA"), colClasses="character")
#county_codes <- read.csv("US_state_county_FIPS_codes.csv")
fia_plots <- read.csv("PLOT.csv")
  # remove unnecessary columns from plot data
  fia_plots <- fia_plots[,c("INVYR","STATECD","UNITCD","COUNTYCD","PLOT",
                            "LAT","LON")]
# join FIA data to supplemental tables
fia_raw2 <- join(fia_raw,fia_codes)
fia_raw2 <- join(fia_raw2,county_codes)
fia_raw2 <- join(fia_raw2,fia_plots)
# create ID column
fia_raw2 <- fia_raw2 %>% unite("fiaPlotID",
  c("INVYR","UNITCD","COUNTYCD","PLOT","STATECD"),remove=F,sep="-")
# keep only necessary columns
fia_raw2 <- fia_raw2 %>% select(
  "taxon_name",
  "LAT","LON",
  "INVYR","fiaPlotID",
  "county","stateProvince",
  "STATUSCD")
# rename columns
setnames(fia_raw2,
  old = c("LAT","LON",
          "INVYR",
          "STATUSCD"),
  new = c("decimalLongitude","decimalLatitude",
          "year",
          "isAlive"),
  skip_absent=T)
fia_raw2$dataset <- "FIA"
# combine a few similar columns
fia_raw2 <- fia_raw2 %>% unite("localityDescription",
  county:stateProvince,na.rm=T,remove=T,sep=" | ")
  fia_raw2$localityDescription <- gsub("^$",NA,fia_raw2$localityDescription)
# create species_name column
fia_raw2$species_name <- NA
fia_raw2$species_name <- sapply(fia_raw2$taxon_name, function(x)
  unlist(strsplit(x," var. | subsp. | f. "))[1])
sort(unique(fia_raw2$species_name))
# check data
percent.filled(fia_raw2)
head(fia_raw2)

################################################################################
# 3. Join everything together
################################################################################

# create list of raw data frames and remove to make space
datasets <- list(fia_raw2,gbif_raw2,idigbio_raw2,bien_raw2,sernec_raw2)
  rm(gbif_raw);rm(idigbio_raw);rm(sernec_raw);rm(bien_raw);rm(fia_raw)
  rm(gbif_raw2);rm(idigbio_raw2);rm(sernec_raw2);rm(bien_raw2);rm(fia_raw2)
  str(datasets)
# bind everything together
all_data_raw <- Reduce(rbind.fill, datasets)
  nrow(all_data_raw) #8342476
  ncol(all_data_raw) #39

# check out the species names
sort(unique(all_data_raw$species_name))
# fix small, specific errors (e.g. hybrid symbol not separated)
all_data_raw$species_name <- mgsub(all_data_raw$species_name,
  c("Malus xdomestica","Quercus xalapensis","Quercus xanthoclada",
    "Quercus xylina","Tilia xeuropaea","Tilia xvulgaris"),
  c("Malus x domestica","Quercus x alapensis","Quercus x anthoclada",
    "Quercus x ylina","Tilia x europaea","Tilia x vulgaris"))
all_data_raw$taxon_name <- mgsub(all_data_raw$taxon_name,
  c("Malus xdomestica","Quercus xalapensis","Quercus xanthoclada",
    "Quercus xylina","Tilia xeuropaea","Tilia xvulgaris"),
  c("Malus x domestica","Quercus x alapensis","Quercus x anthoclada",
    "Quercus x ylina","Tilia x europaea","Tilia x vulgaris"))
sort(unique(all_data_raw$species_name))

# full join to taxon list
all_data_raw <- left_join(all_data_raw,taxon_list)
# join again just by species name if no taxon match
need_match <- all_data_raw[which(is.na(all_data_raw$list)),]
matched <- all_data_raw[which(!is.na(all_data_raw$list)),]
need_match <- need_match[,1:(ncol(all_data_raw)-ncol(taxon_list)+2)]
taxon_list_sp <- taxon_list[,-1]
need_match <- left_join(need_match,taxon_list_sp)
all_data_raw <- rbind(matched,need_match)

# check names that got excluded.....
still_no_match <- all_data_raw[which(is.na(all_data_raw$list)),]
table(still_no_match$dataset)
sort(table(still_no_match$taxon_name))

# keep only rows for target taxa
all_data_raw <- all_data_raw[which(!is.na(all_data_raw$list) &
  !is.na(all_data_raw$species_name_acc) &
  !(is.na(all_data_raw$decimalLatitude) & is.na(all_data_raw$decimalLongitude)
      & is.na(all_data_raw$localityDescription))),]
nrow(all_data_raw) #7635719


################################################################################
# 4. Remove duplicates and standardize some key columns
################################################################################

# fix year column
all_data_raw$year <- as.numeric(all_data_raw$year)
all_data_raw$year[which(all_data_raw$year < 1000)] <- NA
unique(all_data_raw$year) #check
all_data_raw$year[which(all_data_raw$year == 18914)] <- 1891
all_data_raw$year[which(all_data_raw$year == 19418)] <- 1941
all_data_raw$year[which(all_data_raw$year == 9999)] <- NA

# sort before removing dups
all_data_raw <- setorder(all_data_raw,-year,na.last=T)

# create rounded latitude and longitude columns for removing dups
all_data_raw$lat_round <- round(all_data_raw$decimalLatitude,digits=3)
all_data_raw$long_round <- round(all_data_raw$decimalLongitude,digits=3)

write.csv(all_data_raw,"all_data_raw.csv")

# remove duplicates; only keep points with coordinates
all_data <- all_data_raw %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  #group_by(species_name_acc,lat_round,long_round) %>%
  #mutate(source_databases = paste(dataset,collapse = ',')) %>%
  distinct(species_name_acc,lat_round,long_round,dataset,.keep_all=T) %>%
  ungroup()
# remove duplicates in source_databases column
#s <- lapply(all_data$source_databases, function(x) unlist(strsplit(x,split=",")))
#source_standard <- as.data.frame(unlist(lapply(s,function(x) paste(unique(x),collapse=','))))
#names(source_standard)[1] <- "source_databases_all"
#all_data <- cbind(all_data,source_standard)
#all_data <- all_data %>% select(-source_databases)
write.csv(all_data,"all_data_raw_coordinates_noDups.csv")

# see how many points there may be with locality descriptions
all_data_local <- all_data_raw %>%
  filter(is.na(decimalLatitude) & is.na(decimalLongitude)) %>%
  #group_by(species_name_acc,localityDescription) %>%
  #mutate(source_databases = paste(dataset,collapse = ',')) %>%
  distinct(species_name_acc,localityDescription,dataset,.keep_all=T) %>%
  ungroup()
#s <- lapply(all_data_local$source_databases, function(x) unlist(strsplit(x,split=",")))
#source_standard <- as.data.frame(unlist(lapply(s,function(x) paste(unique(x),collapse=','))))
#names(source_standard)[1] <- "source_databases_all"
#all_data_local <- cbind(all_data_local,source_standard)
#all_data_local <- all_data_local %>% select(-source_databases)

# plot number of points per species
all_data$has_coord <- "Coordinate"
all_data_local$has_coord <- "Locality description"
to_plot <- rbind(all_data,all_data_local)
to_plot <- to_plot %>% count(species_name_acc,has_coord,dataset,sort=T)
write.csv(to_plot,"counts_per_species.csv")

ggplot(to_plot, aes(x = species_name_acc,
                    y = n,
                    fill = has_coord)) +
                geom_col() +
                scale_y_continuous(limits = c(0,200))

ggplot(to_plot, aes(x = species_name_acc,
                y = n,
                fill = has_coord)) +
    geom_col() +
    facet_wrap(~dataset) +
    scale_y_continuous(limits = c(0,200))

ggplot(data=datos,aes(x = dia, y = PRECIP)) +
    geom_bar(colour = "blue",stat = "identity") +
    ylab("Precipitación (l)") +
    xlab("Hora solar") +
    opts(title = "Precipitacion acumulada horaria \n 2008-05-27 Burriana") +
    scale_y_continuous(limits = c(0,50))

all_data_noDup <- all_data_raw %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  #group_by(species_name_acc,lat_round,long_round) %>%
  #mutate(source_databases = paste(dataset,collapse = ',')) %>%
  distinct(species_name_acc,lat_round,long_round,.keep_all=T) %>%
  ungroup()
all_data_simple <- all_data_noDup %>% count(species_name_acc,sort=T)

write.csv(all_data_simple,"counts_per_species_simple.csv")
