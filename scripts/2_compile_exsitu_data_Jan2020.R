### Author: Emily Beckman  ###  Date: 12/13/2019                                |

### DESCRIPTION:
  # This script takes a folder of CSV files representing accessions data from
  #   different institutions, combines them into one dataset, and standardizes
  #   some important fields.

### INPUTS:
  # 1. Folder (standard_column_names) of CSV files whose column names have
  #     already be standardized by hand using the
  #     "standardizing_acessions_data_fields.xlsx" template
  # 2. Target taxa list (taxa_list.csv), created through
  #     1_compile_taxa_list.R script

### OUTPUTS:
  # 1. exsitu_compiled_Raw.csv
  # 2. exsitu_compiled_StandardNames.csv
  # 3. exsitu_compiled_TaxaMatched.csv
  # 4. exsitu_compiled_Standardized.csv
  # 5. exsitu_compiled_ReadyToGeolocate.csv


#################
### LIBRARIES ###
#################

library(tidyverse) #ggplot2,dplyr,tidyr,readr,purrr,tibble,stringr,forcats
library(data.table)
library(anchors)
library(measurements)
library(textclean)
library(plyr); library(dplyr)
library(naniar)

#################
### FUNCTIONS ###
#################

##############
### SCRIPT ###
##############

setwd("./Desktop")

###############################
# 1. Stack all accessions data
###############################

# create list of paths to ex situ accessions CSV files in folder
file_list <- list.files(path="./standard_column_names",pattern=".csv",
  full.names=TRUE); str(file_list)
# read in each csv in path list to create list of dataframes
file_dfs <- lapply(file_list,read.csv,header=TRUE,fileEncoding="LATIN1",
  strip.white=TRUE,colClasses="character")
length(file_dfs) #136
#sapply(file_dfs, nrow) # can look at number of rows in each csv

# add file name as column, to record which institution each record came from
for(file in seq_along(file_dfs)){
  file_dfs[[file]]$inst_short_added <- rep(file_list[file],
    nrow(file_dfs[[file]]))
}

# stack all datasets using rbind.fill, which keeps non-matching columns
#   and fills with NA; 'Reduce' iterates through list and merges with previous
# this may take a few minutes if you have lots of data
all_data_raw <- Reduce(rbind.fill, file_dfs)
  nrow(all_data_raw) #100089
  ncol(all_data_raw) #1007

# check out column names
sort(colnames(all_data_raw))
all_data <- all_data_raw
# IF NEEDED: remove extra columns (can be created through Excel to CSV issues)
  all_data <- all_data[, -grep("^X", names(all_data))]
  # check schema to see if problems still exist
  str(all_data); sort(colnames(all_data)); ncol(all_data) #38
# IF NEEDED: see which datasets have extraneous columns so you can fix manually
#  if desired; change line below as needed
  #unique(all_data$inst_short_added[all_data$taxon_full_name.1!=""])
# IF NEEDED: merge similar columns (you may not need to do this if no schema
#   mistakes were made)
  all_data <- tidyr::unite(all_data,"num_indiv", c("num_indiv","num_plants"),
    sep="",remove=T)
      all_data$num_indiv <- sub("NA","",all_data$num_indiv)
    sort(colnames(all_data)); unique(all_data$num_indiv); ncol(all_data) #37
  all_data <- tidyr::unite(all_data,"acq_year", c("acq_year","coll_year"),
    sep="",remove=T)
      all_data$acq_year <- sub("NA","",all_data$acq_year)
    sort(colnames(all_data)); ncol(all_data) #36
# IF NEEDED: remove unused columns or rename columns
  #all_data <- all_data[ , -which(names(all_data) %in% c("donor"))]
  #colnames(all_data)[colnames(all_data)=="elevation"] <- "altitude"

# update inst_short_added column to contain only institution name, not filepath
all_data$inst_short_added <- sub("./standard_column_names/","",
  all_data$inst_short_added)
all_data$inst_short_added <- sub(".csv","",all_data$inst_short_added)
  sort(unique(all_data$inst_short_added))

# if there is an inst_short provided, use it to replace inst_short_added
all_data$inst_short_added[!is.na(all_data$inst_short)] <-
  all_data$inst_short[!is.na(all_data$inst_short)]
all_data <- all_data[ , -which(names(all_data) %in% c("inst_short"))]
colnames(all_data)[colnames(all_data)=="inst_short_added"] <- "inst_short"
  sort(unique(all_data$inst_short))
# remove rows with no inst_short
all_data <- all_data[which(all_data$inst_short!=""),]

# remove leading, trailing, and middle (e.g., double space) whitespace,
#   to prevent future errors
all_data <- as.data.frame(lapply(all_data, function(x) str_squish(x)),
  stringsAsFactors=F)

# replace "" cells with NA in whole dataset
all_data[all_data == ""] <- NA

# write raw CSV file
write.csv(all_data,"exsitu_compiled_Raw.csv")

######################################
# 2. Standardize species name columns
######################################

# read in target taxa list
taxa_list <- read.csv("taxa_list.csv",fileEncoding="latin1",
  strip.white=T,colClasses="character",as.is=T,na.strings=c("","NA"))

### STANDARDIZE HYBRIDS ###

all_data2 <- all_data

unique(all_data2$hybrid)
# replace anything in hybrid column that does not notate it is a hybrid
all_data2 <- replace.value(all_data,"hybrid",c("species"),NA)
# now replace all strings in hybrid col with "x"
all_data2$hybrid[!is.na(all_data2$hybrid)] <- "x"
  sort(unique(all_data2$hybrid))
  nrow(all_data2[which(all_data2$hybrid=="x"),]) #3210

# look for hybrid symbols in taxon_full_name and species columns; when found,
#   mark in hybrid column
all_data2$hybrid[grep("_",paste(all_data2$taxon_full_name,
  all_data2$species))] <- "x"
    nrow(all_data2[which(all_data2$hybrid=="x"),]) #4675
all_data2$hybrid[grep(" hybrid ",paste(all_data2$taxon_full_name,
  all_data2$species))] <- "x"
    nrow(all_data2[which(all_data2$hybrid=="x"),]) #4768
all_data2$hybrid[grep("×",paste(all_data2$taxon_full_name,
  all_data2$species))] <- "x"
    nrow(all_data2[which(all_data2$hybrid=="x"),]) #4769
all_data2$hybrid[grep(" X",paste(all_data2$taxon_full_name,
  all_data2$species))] <- "x"
    nrow(all_data2[which(all_data2$hybrid=="x"),]) #4907

# replace hybrid symbols with "x" in taxon_full_name and species columns
all_data2$taxon_full_name <- mgsub(all_data2$taxon_full_name,
  c("_"," hybrid ","×"," X")," x ")
all_data2$species <- mgsub(all_data2$species,
  c("_"," hybrid ","×"," X")," x ")

### SEPARATE TAXON FULL NAME ###

all_data3 <- all_data2

## create contents of taxon_full_name column for when it hasn't been provided
  # preserve original taxon name
all_data3$taxon_full_name_orig <- all_data3$taxon_full_name
  # create concatenated taxon_full_name col
all_data3 <- unite(all_data3, "taxon_full_name_concat",
  c(genus,species,infra_rank,infra_name), sep = " ", remove = F)
  # get rid of NAs in concatenated taxon name
all_data3$taxon_full_name_concat <- mgsub(all_data3$taxon_full_name_concat,
  c("NA "," NA"," NA"," NA"," NA"), "")
  # trim whitespace
all_data3$taxon_full_name_concat <- str_squish(all_data3$taxon_full_name_concat)
  # when blank, fill taxon_full_name column with concatenated full name
all_data3$taxon_full_name[is.na(all_data3$taxon_full_name)] <-
  all_data3$taxon_full_name_concat[is.na(all_data3$taxon_full_name)]

# fix some string errors\inconsistencies
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("\'","(","\\","\"",")","."), "") # replace unwanted taxon name characters
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("var.","v."), " var. ") # put space between var. and species name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("subsp.","ssp."), " subsp. ") # put space between subsp. and species name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("f."), "f. ") # put space between f. and species name
all_data3 <- data.frame(lapply(all_data3, function(x)
  {mgsub(x,c("ê","Ê")," ")})) # replace unwanted characters in all columns
# trim whitespace
all_data3 <- as.data.frame(lapply(all_data3,str_squish),stringsAsFactors=F)

# separate out taxon full name and trim whitespace again
all_data3 <- all_data3 %>% separate("taxon_full_name",
  c("genus_new","species_new","extra1","extra2",
    "extra3","extra4","extra5","extra6"),sep=" ",extra="warn",
    remove=F,fill="right")
all_data3 <- as.data.frame(lapply(all_data3,str_squish),stringsAsFactors=F)

# fix genus capitalization issues
all_data3$genus_new <- str_to_title(all_data3$genus_new)

# check genus names
sort(unique(all_data3$genus_new))
# IF NEEDED: fix misspellings
  all_data3$genus_new <- mgsub(all_data3$genus_new,
    c("Tila","Ulmua","Magnoliax","^M$","Querucs"),
    c("Tilia","Ulmus","Magnolia","Magnolia","Quercus"),fixed=F)

# keep only rows from target genera
all_data3 <- all_data3 %>% filter(all_data3$genus_new %in%
  unique(taxa_list$genus))
nrow(all_data3) #90376

# check species names
all_data3$species_new <- mgsub(all_data3$species_new,
    c("[","]","/canbyi","Ô","Õ","\u008a/","\u0095","\u0097"),"")
sort(unique(all_data3$species_new))

### FIND INFRATAXA AND HYBRIDS ###

all_data4 <- all_data3

## look for infrataxa key words
# !!!!! check position of 'species_new' and 'extra' columns and change below
#   as needed
colnames(all_data4)
# make data in all "extra" columns lower case
### FIX SO NOT HARD CODED ###
all_data4[,24:30] <- as.data.frame(sapply(all_data4[,24:30], tolower),
  stringsAsFactors=F)
# create matrix of all "extra" species name columns, to search for
#   infraspecific key words
search.col <- matrix(cbind(all_data4$extra1,all_data4$extra2,all_data4$extra3,
  all_data4$extra4,all_data4$extra5,all_data4$extra6),nrow=nrow(all_data4))
  str(search.col)
# search the "extra" column matrix for matches to infraspecific key words
matches_i <- which(search.col=="variety"|search.col=="var"|search.col=="var."|
                  search.col=="v"|search.col=="v."|search.col=="va"|
                 search.col=="subspecies"|search.col=="subsp"|
                  search.col=="subsp."|search.col=="ssp"|search.col=="ssp."|
                  search.col=="subs."|search.col=="spp."|
                 search.col=="infra"|
                 search.col=="forma"|search.col=="form"|search.col=="fma"|
                  search.col=="fo"|search.col=="fo."|search.col=="f"|
                  search.col=="f.",arr.ind=T)
# !!! change the number added below based on where the extra columns are in your
  # dataset; e.g., add 19 if extra1 is in the 20th column
### FIX SO NOT HARD CODED ###
matches_i[,2] <- matches_i[,2]+24
# create new infra_rank column and fill with "extra" contents that matched
#   infraspecific key words
all_data4$infra_rank_new <- NA
all_data4$infra_rank_new[matches_i] <- all_data4[matches_i]
#all_data4$infra_rank_new[matches_h] <- "x"
  unique(all_data4$infra_rank_new) # check results

# create new infra_name column and fill with next column over from "extra"
#   contents that matched infraspecific key word
all_data4$infra_name_new <- NA
matches_i[,2] <- matches_i[,2]+1
all_data4$infra_name_new[matches_i] <- all_data4[matches_i]
  sort(unique(all_data4$infra_name_new))

# standardize infraspecific rank names
all_data4$infra_rank_new <- replace(all_data4$infra_rank_new,
  grep("^v$|^v.$|^var$|^variety$|^va$",all_data4$infra_rank_new), "var.")
all_data4$infra_rank_new <- replace(all_data4$infra_rank_new,
  grep("^subspecies$|^subsp$|^ssp$|^ssp.$|^subs.$|^spp.$",
  all_data4$infra_rank_new), "subsp.")
all_data4$infra_rank_new <- replace(all_data4$infra_rank_new,
 grep("^forma$|^form$|^fma$|^fo$|^fo.$|^f$",all_data4$infra_rank_new), "f.")
unique(all_data4$infra_rank_new)

# search species_new column for hybrid matches
matches_h <- which(all_data4$species_new=="x")
# where "x" was found in the species_new column, paste contents of the "extra1"
#   column after the "x"
all_data4$species_new[matches_h] <- paste("x", all_data4$extra1[matches_h])
# search "extra1" column for hybrid matches
matches_h2 <- which(all_data4$extra1=="x")
# where "x" was found in the extra1 column, paste contents of the "extra2"
#   in the infra_name column and add "x" to infra_rank
all_data4$infra_name_new[matches_h2] <- all_data4$extra2[matches_h2]
all_data4$infra_rank_new[matches_h2] <- "x"
  sort(unique(all_data4$species_new))

### REMOVE UNWANTED CULTIVAR SYMBOLS ###

all_data5 <- all_data4

# replace apostrophe
all_data5$cultivar_new <- all_data5$cultivar
all_data5$cultivar_new <- gsub("\\'s","s",all_data5$cultivar_new)
# remove characters after the second quote and before first quote
all_data5$cultivar_new <-
  gsub("([[:alpha:]])\\'.*","\\1",all_data5$cultivar_new)
all_data5$cultivar_new <-
  gsub("([[:alpha:]])\\\".*","\\1",all_data5$cultivar_new)
all_data5$cultivar_new <- gsub("[[:alpha:]]\\'","",all_data5$cultivar_new)
# replace quotes and parentheses
all_data5$cultivar_new <- mgsub(all_data5$cultivar_new,
  c("\'","\"","(",")","[","]"), "")
# replace "_" with "X"
all_data5$cultivar_new <- gsub("_","X",all_data5$cultivar_new)
# replace extra characters with ""
all_data5$cultivar_new <- mgsub(all_data5$cultivar_new, c("M. ","√î","√µ"), "")
# replace non-cultivar names with NA
all_data5$cultivar_new <- mgsub(all_data5$cultivar_new,
  c("M. "," cultivar",":","cv.","cv","cvs.","yunnanensis X insignis"),"")
all_data5$cultivar_new <- mgsub(all_data5$cultivar_new,
  c("^var*","^subsp*","^ssp*","^\\."),"",fixed=FALSE)
all_data5$cultivar_new <- gsub("^$",NA,all_data5$cultivar_new)
# capitalize
all_data5$cultivar_new <- str_to_title(all_data5$cultivar_new)
# remove leading/trailing whitespace
all_data5$cultivar_new <- str_squish(all_data5$cultivar_new)
  sort(unique(all_data5$cultivar_new))

### CREATE FINAL TAXON FULL NAME ###

# create new taxon full name column
all_data5$taxon_full_name <- NA
  # select rows with infraspecific name and concatenate
yes_infra <- which(!is.na(all_data5$infra_rank_new) &
  !is.na(all_data5$infra_name_new))
all_data5$taxon_full_name[yes_infra] <- paste(all_data5$genus_new[yes_infra],
  all_data5$species_new[yes_infra], all_data5$infra_rank_new[yes_infra],
  all_data5$infra_name_new[yes_infra],sep=" ")
  # select rows without infraspecific name and concatenate
all_data5$taxon_full_name[-yes_infra] <- paste(all_data5$genus_new[-yes_infra],
  all_data5$species_new[-yes_infra],sep=" ")
  # replace "NA" in new name, which comes from rows with no species name
all_data5$taxon_full_name <- gsub(" NA","",all_data5$taxon_full_name)
  # select rows with cultivar name and concatenate in new column
all_data5$taxon_full_name_cultivar <- NA
yes_cult <- which(!is.na(all_data5$cultivar_new)); nrow(yes_cult)
all_data5$taxon_full_name_cultivar[yes_cult] <-
  paste(all_data5$taxon_full_name[yes_cult],
  all_data5$cultivar_new[yes_cult],sep=" ")
  # add genus_species column
all_data5$genus_species <- paste(all_data5$genus_new,all_data5$species_new)

# check out results
#   most common issue is a hybrid name with no space between the "x" and the
#   rest of the taxon name (e.g., "Quercus xbebbiana")
sort(unique(all_data5$taxon_full_name))
sort(unique(all_data5$taxon_full_name_cultivar))

# fix some string errors
#all_data5$taxon_full_name <- mgsub(all_data5$taxon_full_name,
#  c("-gruppen"), "") # replace unwanted characters

# write file
write.csv(all_data5, "exsitu_compiled_StandardNames.csv")

#####################################
# 3. Filter by target species names
#####################################

all_data6 <- all_data5

# for comparison as we remove rows:
nrow(all_data6) #90376

# create table with institutions and genera, for analysis
genera <- all_data6 %>% filter(all_data6$genus_new %in%
  c("Acer","Magnolia","Malus","Quercus","Tilia","Ulmus"))
genera_g <- unique(genera %>% summarise(inst_short,genus_new))
  str(genera_g)
write.csv(genera_g, "genera_per_institution.csv")

# select rows that should be added back in, even if don't match genus_species
add_back <- all_data6[which(all_data6$genus_new == "Acer" |
                            all_data6$genus_new == "Magnolia"),]

# keep only rows with target species name
all_data6 <- all_data6 %>% filter(all_data6$species_new %in%
  unique(taxa_list$species))
nrow(all_data6) #38681

# keep only rows for target genus_species
taxa_list$genus_species <- paste(taxa_list$genus,taxa_list$species)
all_data6 <- all_data6 %>% filter(all_data6$genus_species %in%
  unique(taxa_list$genus_species))
nrow(all_data6) #35222

# add selected rows back in
all_data6 <- rbind(all_data6,add_back)
  nrow(all_data6) #81210

# !!! check to see if institutions got excluded, and manually check those files
#   as needed, to see if issues
setdiff(unique(all_data2$inst_short),unique(all_data6$inst_short))

##############################
# 4. Join to target taxa list
##############################

all_data7 <- all_data6

# join dataset to species list
  # rename some taxon name columns to preserve originals
setnames(all_data7,
  old = c("genus","species","infra_rank","infra_name","cultivar"),
  new = c("genus_orig","species_orig","infra_rank_orig","infra_name_orig",
    "cultivar_orig"))
setnames(all_data7,
  old = c("genus_new","species_new","infra_rank_new","infra_name_new",
    "taxon_full_name","cultivar_new"),
  new = c("genus","species","infra_rank","infra_name","taxon_full_name_created",
    "cultivar"))
  # remove unused columns
all_data7 <- subset(all_data7, select = -c(taxon_full_name_concat,extra1,extra2,
  extra3,extra4,extra5,extra6))
  # join
all_data7 <- plyr::join(all_data7, taxa_list, type = "left", match = "first")
  str(all_data7)
  # if no match when infra name is included, join just by genus and species
try_again <- all_data7[which(is.na(all_data7$taxon_full_name_acc)),]
try_again <- within(try_again,rm(taxon_full_name_acc,taxon_full_name,orig_list))
### FIX SO NOT HARD CODED ###
taxa_list <- taxa_list[-c(6)]
try_again <- plyr::join(try_again, taxa_list,type = "left", match = "first")
  str(try_again)
  # remove rows with no taxon name match
#all_data7 <- all_data7[-which(is.na(all_data7$taxon_full_name_acc)),]
#  nrow(all_data7)
  # join matches from first round and 'try_again' round
all_data8 <- plyr::join(all_data7, try_again, type = "full")
  nrow(all_data8)
  # see how many rows have taxon name match
nrow(all_data8[which(!is.na(all_data8$taxon_full_name_acc)),])

# write file
write.csv(all_data8, "exsitu_compiled_TaxaMatched.csv")

###################################
# 5. Standardize important columns
###################################

### PROVENANCE TYPE ###

# look at column contents
sort(unique(all_data8$prov_type))
## IF NEEDED: transfer contents of one column to another column, if data
  #needs to be preserved but is in wrong place
  #all_data8$notes[grep("Of known, direct wild origin - Florence County, SC.",
  #all_data8$prov_type)] <- "Florence County, SC"

# standardize column by searching for keywords and replacing with standard value
  # remove confusing words/phrases
all_data8$prov_type <- mgsub(all_data8$prov_type,
  c(". Accession not of wild source"), "")
  # wild (W)
all_data8$prov_type <- ifelse(grepl(paste(
  c("wild","wld","collect","^w$","(W)"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"W",all_data8$prov_type)
  # ex wild (Z)
all_data8$prov_type <- ifelse(grepl(paste(
  c("indirect","ex wild","^z$","Cultivated from wild"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"Z",all_data8$prov_type)
  # cultivated (H)
all_data8$prov_type <- ifelse(grepl(paste(
  c("cultiva","garden","nursery","^c$","^g$","horticult"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"H",all_data8$prov_type)
  # native to site (N)
all_data8$prov_type <- ifelse(grepl(
  "native",all_data8$prov_type, ignore.case=T),"N",all_data8$prov_type)
  # unknown (U) ; everything else is unknown
all_data8$prov_type <- ifelse(all_data8$prov_type!= "W" &
  all_data8$prov_type != "Z" & all_data8$prov_type != "H" &
  all_data8$prov_type != "N","U",all_data8$prov_type)
all_data8$prov_type[which(is.na(all_data8$prov_type))] <- "U"

# check results
unique(all_data8$prov_type)

### NUMBER OF INDIVIDUALS ###

# look at column contents
sort(unique(all_data8$num_indiv))
## IF NEEDED: replace unwanted characters
  all_data8$num_indiv <- mgsub(all_data8$num_indiv,
  c("+3","+","ca ","*"," in terra"," In terra","-3?","-Jan","?deck",
    " & 2","-Apr",":04","mass of ","?","deck","-10","inG7"), "")
  sort(unique(all_data8$num_indiv))

# change type to numeric and replace NA with 1
all_data8$num_indiv <- as.numeric(all_data8$num_indiv)
all_data8$num_indiv[which(is.na(all_data8$num_indiv))] <- 1

# check results
unique(all_data8$num_indiv)

### LATITUDE AND LONGITUDE ###

# preserve original lat and long columns
all_data8$lat_dd <- all_data8$orig_lat
all_data8$long_dd <- all_data8$orig_long

# replace comma with decimal (european notation)
all_data8$lat_dd <- mgsub(all_data8$lat_dd, c(",",";"," ."), ".")
all_data8$long_dd <- mgsub(all_data8$long_dd, c(",",";"), ".")

# replace non-ascii characters
all_data8$lat_dd <- replace_non_ascii(all_data8$lat_dd)
all_data8$long_dd <- replace_non_ascii(all_data8$long_dd)

# replace remaining unwanted characters and format for conversion
  #(e.g., ## ## ## (DMS) OR ## ##.### (DM));
  #(d, m, and s must be in the same cell, with 1 space between each value)
  ### the next section needs to be condensed ###

## latitude
  sort(unique(all_data8$lat_dd))
  all_data8$lat_dd <- gsub("^0$",NA,all_data8$lat_dd)
all_data8$lat_dd <- mgsub(all_data8$lat_dd,
  c("N","\\","\"","\'","/","M","A_",": ","-24032O","E","AZ","R","_","d")," ")
all_data8$lat_dd <- str_squish(all_data8$lat_dd)
  sort(unique(all_data8$lat_dd))
    # the next four lines search for latitudes that start with four digits in a
    # row and then adds a space after the first two
need_space <- grep("^[2-9][0-9][0-9][0-9]+",all_data8$lat_dd)
  unique(all_data8$lat_dd[need_space])
  all_data8$lat_dd[need_space] <- gsub("^(.{2})(.*)$", "\\1 \\2",
  all_data8$lat_dd[need_space])
  sort(unique(all_data8$lat_dd))
need_space <- grep("^[1][0-9][0-9][0-9]+",all_data8$lat_dd)
  unique(all_data8$lat_dd[need_space])
  all_data8$lat_dd[need_space] <- gsub("^(.{3})(.*)$", "\\1 \\2",
  all_data8$lat_dd[need_space])
  sort(unique(all_data8$lat_dd))
need_space <- grep("^-[2-9][0-9][0-9][0-9]+",all_data8$lat_dd)
  unique(all_data8$lat_dd[need_space])
  all_data8$lat_dd[need_space] <- gsub("^(.{3})(.*)$", "\\1 \\2",
  all_data8$lat_dd[need_space])
  sort(unique(all_data8$lat_dd))
need_space <- grep("^-[1][0-9][0-9][0-9]+",all_data8$lat_dd)
  unique(all_data8$lat_dd[need_space])
  all_data8$lat_dd[need_space] <- gsub("^(.{4})(.*)$", "\\1 \\2",
  all_data8$lat_dd[need_space])
  sort(unique(all_data8$lat_dd))
need_space <- grep("^[2-9][0-9][0-9]+",all_data8$lat_dd)
  unique(all_data8$lat_dd[need_space])
  all_data8$lat_dd[need_space] <- gsub("^(.{2})(.*)$", "\\1 \\2",
  all_data8$lat_dd[need_space])
  sort(unique(all_data8$lat_dd))
need_space <- grep("[[:blank:]][0-9][0-9][0-9][0-9]+",all_data8$lat_dd)
  unique(all_data8$lat_dd[need_space])
  all_data8$lat_dd[need_space] <- gsub("(.+)[[:blank:]](.{2})(.*)$", "\\1 \\2 \\3",
  all_data8$lat_dd[need_space])
  sort(unique(all_data8$lat_dd[need_space]))
## longitude
  sort(unique(all_data8$long_dd))
  all_data8$long_dd <- gsub("^0$",NA,all_data8$long_dd)
all_data8$long_dd <- mgsub(all_data8$long_dd,
  c("E","\\","\"","\'","/","_","NR","d")," ")
all_data8$long_dd <- str_squish(all_data8$long_dd)
  sort(unique(all_data8$long_dd))
    # move "W" to end if at beginning
all_data8$long_dd[grep("^W",all_data8$long_dd)] <-
  paste(all_data8$long_dd[grep("^W",all_data8$long_dd)],"W")
all_data8$long_dd[grep("^W",all_data8$long_dd)] <-
  gsub("W","",all_data8$long_dd[grep("^W",all_data8$long_dd)],"W")
    # the next four lines search for longitudes that start with five digits in a
    # row and then adds a space after the first three
need_space <- grep("^[0-9][0-9][0-9][0-9][0-9]+",all_data8$long_dd)
need_space <- c(need_space,grep("^1[0-9][0-9][0-9]",all_data8$long_dd))
  all_data8$long_dd[need_space] <- gsub("^(.{3})(.*)$", "\\1 \\2",
  all_data8$long_dd[need_space])
  sort(unique(all_data8$long_dd))
need_space <- grep("^[0-9][0-9][0-9][0-9]+",all_data8$long_dd)
  all_data8$long_dd[need_space] <- gsub("^(.{2})(.*)$", "\\1 \\2",
  all_data8$long_dd[need_space])
  sort(unique(all_data8$long_dd))
need_space <- grep("^[2-9][0-9][0-9]+",all_data8$long_dd)
  all_data8$long_dd[need_space] <- gsub("^(.{2})(.*)$", "\\1 \\2",
  all_data8$long_dd[need_space])
  sort(unique(all_data8$lat_dd))
    # if first three numbers are greater than 180, split string after second
    # number
#all_data8$long_dd <- ifelse(as.numeric(substr(all_data8$long_dd,start=1,
#  stop=3))>180,gsub("^(.{2})(.*)$", "\\1 \\2", all_data8$long_dd),
#  all_data8$long_dd)
    # add negative sign where needed and remove "W"
all_data8$long_dd[grep("W",all_data8$long_dd,ignore.case=T)] <-
  paste("-",all_data8$long_dd[grep("W",all_data8$long_dd,ignore.case=T)],sep="")
  all_data8$long_dd <- gsub("W","",all_data8$long_dd)
  all_data8$long_dd <- str_squish(all_data8$long_dd)
  sort(unique(all_data8$long_dd))

# convert decimal-minutes-seconds (dms) to decimal degrees (dd)
  # mark rows that need to be converted
convert <- all_data8[which(grepl(" ",all_data8$lat_dd) | grepl(" ",all_data8$long_dd)),]; nrow(convert) #125
  unique(convert$lat_dd)
good <- subset(all_data8, !(convert %in% all_data8))
  # separate by dec_min_sec and deg_dec_min then convert to decimal degrees
dms <- convert[which(str_count(convert$lat_dd," ") == 2),]; nrow(dms) #60
ddm <- convert[which(str_count(convert$lat_dd," ") == 1),]; nrow(ddm) #63
extra <- convert[which(str_count(convert$lat_dd," ") == 0),]; nrow(extra) #1
  dms$lat_dd = measurements::conv_unit(dms$lat_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$lat_dd = measurements::conv_unit(ddm$lat_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #124
dms <- convert[which(str_count(convert$long_dd," ") == 2),]; nrow(dms) #60
ddm <- convert[which(str_count(convert$long_dd," ") == 1),]; nrow(ddm) #64
extra <- convert[which(str_count(convert$long_dd," ") != 1 & str_count(convert$long_dd," ") != 2),]; nrow(extra) #0
  dms$long_dd = measurements::conv_unit(dms$long_dd, from = 'deg_min_sec', to = 'dec_deg')
  ddm$long_dd = measurements::conv_unit(ddm$long_dd, from = 'deg_dec_min', to = 'dec_deg')
  convert <- rbind(dms,ddm,extra); nrow(convert) #124
  # join everything back together
all_data9 <- rbind(good,convert); nrow(all_data9) #1498

# clean up lat and long coordinates
all_data9$lat_dd <- as.numeric(all_data9$lat_dd)
  sort(unique(all_data9$lat_dd))
all_data9$long_dd <- as.numeric(all_data9$long_dd)
  sort(unique(all_data9$long_dd))

# add gps_det (gps determination) column
all_data9$gps_det <- NA
all_data9$gps_det[which(all_data9$prov_type == "H")] <- "H"
all_data9$gps_det[which(!is.na(all_data9$lat_dd) &
  !is.na(all_data9$long_dd))] <- "G"
table(all_data9$gps_det)

# where prov_type is "H" but lat-long is given, change to "H?"
all_data9$prov_type[which(all_data9$gps_det == "G" &
  all_data9$prov_type == "H")] <- "H?"
table(all_data9$prov_type)

### LOCALITY AND COLLECTOR ###

# create all_locality and collector columns
  # replace non-ascii characters
all_data9$locality <- replace_non_ascii(all_data9$locality)
all_data9$municipality <- replace_non_ascii(all_data9$municipality)
all_data9$county <- replace_non_ascii(all_data9$county)
all_data9$state <- replace_non_ascii(all_data9$state)
all_data9$orig_source <- replace_non_ascii(all_data9$orig_source)
all_data9$notes <- replace_non_ascii(all_data9$notes)
  # create all_locality column
all_data9 <- unite(all_data9, "all_locality",
                    c(locality,municipality,county,state,orig_source,notes),
                    sep = " | ", remove = F)

# write file
write.csv(all_data9, "exsitu_compiled_Standardized.csv")

##############################
# 6. Remove duplicate records
##############################

# remove duplicates
all_data10 <- ddply(all_data9,
                  .(inst_short,taxon_full_name_acc,taxon_full_name_orig,
                    taxon_full_name,taxon_full_name_created,
                    taxon_full_name_cultivar,orig_list,
                    genus,species,infra_rank,infra_name,hybrid,cultivar,
                    prov_type,lat_dd,long_dd,all_locality,gps_det,
                    country,municipality,state,county,locality,assoc_sp,notes,
                    acc_num,lin_num,orig_source,rec_as,germ_type,garden_loc,
                    acq_year,coll_num,coll_name,
                    notes,condition,name_determ,habitat,trade_name),
                    summarise, sum_num_indiv = sum(num_indiv))
  str(all_data10); nrow(all_data10) #78802

# replace commas with semicolon, just to be sure CSV works properly
all_data10[] <- lapply(all_data10, function(x) gsub(",", ";", x))

# write file
write.csv(all_data10, "exsitu_compiled_ReadyToGeolocate.csv")

# write individual files for genera
magnolia <- all_data10[which(all_data10$genus=="Magnolia"),]; nrow(magnolia) #15401
  # remove unused columns
  magnolia <- magnolia[ , -which(names(magnolia) %in% c("taxon_full_name_acc",
    "taxon_full_name","orig_list"))]
  write.csv(magnolia, "exsitu_compiled_Magnolia.csv")
acer <- all_data10[which(all_data10$genus=="Acer"),]; nrow(acer) #29630
  acer <- acer[ , -which(names(acer) %in% c("taxon_full_name_acc",
    "taxon_full_name","orig_list"))]
  write.csv(acer, "exsitu_compiled_Acer.csv")
morton <- all_data10[which(all_data10$genus=="Malus" | all_data10$genus=="Quercus" |
  all_data10$genus=="Tilia" | all_data10$genus=="Ulmus"),]; nrow(morton) #33771
  write.csv(morton, "exsitu_compiled_MortonIMLS.csv")
