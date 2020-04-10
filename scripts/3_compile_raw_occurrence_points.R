### Author: Emily Beckman  ###  Date: 02/05/2020                                |

### DESCRIPTION:
  # This script

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
#library(data.table)
#library(batchtools)
#library(textclean)


################################################################################
# A) Read in data and stack
################################################################################

setwd("./../..")
setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")

# read in target taxa list
taxon_list <- read.csv("target_taxa_with_syn.csv", header = T,
  na.strings=c("","NA"), colClasses="character")

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
  nrow(all_data_raw) #100089
  ncol(all_data_raw) #1007

################################################################################
# B) Filter by target taxa
################################################################################

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
# C) Standardize some key columns
################################################################################

# year recorded
all_data_raw$year <- as.numeric(all_data_raw$year)
all_data_raw$year[which(all_data_raw$year < 1000)] <- NA
unique(all_data_raw$year) #check
all_data_raw$year[which(all_data_raw$year == 18914)] <- 1891
all_data_raw$year[which(all_data_raw$year == 19418)] <- 1941
all_data_raw$year[which(all_data_raw$year == 9999)] <- NA


gbif_raw <- gbif_raw %>% unite("localityDescription",
  locality:countryCode,na.rm=T,remove=T,sep=" | ")
  gbif_raw$localityDescription <- gsub("^$",NA,gbif_raw$localityDescription)


# Precision type (GPS, county centroid, georeferenced…)

# Basis of record

# establishment means / is cultivated



################################################################################
# D) Separate by species
################################################################################








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
