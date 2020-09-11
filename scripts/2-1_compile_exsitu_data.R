################################################################################

## !! NEEDS RUN-THROUGH AND SOME AREAS NEED WORK STILL !!

## 2-1_compile_exsitu_data.R
### Author: Emily Beckman ### Date: 12/13/2019

### DESCRIPTION:
  # This script takes a folder of CSV files representing accessions data from
  #   different institutions, combines them into one dataset, and standardizes
  #   some important fields.

### DATA IN:
  # 1. Folder ("standard_column_names") of CSV files whose column names have
  #     already be standardized by hand using the
  #     "standardizing_accessions_data_fields" template
  #     (https://docs.google.com/spreadsheets/d/1QLxxWu-bUIRcrjHiaWeSz9n1ms4EQB3yQ8P8PBBx3Zk/edit?usp=sharing)
  # 2. Target taxa list (target_taxa_with_syn.csv), created through
  #     1-0_get_taxonomic_info.R

### DATA OUT:

################################################################################
# Load libraries
################################################################################

#rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'data.table', 'anchors', 'textclean',
  'measurements', 'naniar')
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
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))

# function to read in ex situ files from different folders/years and stack
read.exsitu.csv <- function(path,submission_year){
  # create list of paths to ex situ accessions CSV files in folder
  file_list <- list.files(path=path,pattern=".csv",full.names=TRUE)
  # read in each csv in path list to create list of dataframes
  file_dfs <- lapply(file_list,read.csv,header=TRUE,fileEncoding="LATIN1",
    strip.white=TRUE,colClasses="character",na.strings=c("","NA"))
  print(length(file_dfs))
    #sapply(file_dfs, nrow) # can look at number of rows in each csv
  for(file in seq_along(file_dfs)){
    # add file name as column, to record home institution for each record
    file_dfs[[file]]$filename <- rep(file_list[file],
      nrow(file_dfs[[file]]))
    # remove file path portion
    file_dfs[[file]]$filename <- mgsub(
      file_dfs[[file]]$filename,c(paste0(path,"/"),".csv"),"")
    # add year of submission
    file_dfs[[file]]$submission_year <- submission_year
  }
  print(head(file_dfs[[1]]))
  # stack all datasets using rbind.fill, which keeps non-matching columns
  #   and fills with NA; 'Reduce' iterates through and merges with previous
  # this may take a few minutes if you have lots of data
  all_data6 <- Reduce(rbind.fill, file_dfs)
    print(nrow(all_data6))
    print(ncol(all_data6))
  return(all_data6)
}


################################################################################
################################################################################
# 1. Read in and stack all accessions data
################################################################################

# FIRST: After receiving accession data from institutions, you need to process
#   it manually. See here for instructions:
#   https://docs.google.com/spreadsheets/d/1QLxxWu-bUIRcrjHiaWeSz9n1ms4EQB3yQ8P8PBBx3Zk/edit?usp=sharing

# ## READ IN FROM MULTIPLE FOLDERS FOR DIFFERENT YEARS/SURVEYS:
#  # read in data from 2017 and 2019 and stack
  raw_2019 <- read.exsitu.csv(file.path(main_dir,"inputs",
    "raw_occurrence","exsitu_standard_column_names","data_2019"), "2019")
  raw_2017 <- read.exsitu.csv(file.path(main_dir,"inputs",
    "raw_occurrence","exsitu_standard_column_names","data_2017"), "2017")
  # if genus is blank in 2017 data, make it "Quercus"
  raw_2017[which(is.na(raw_2017$genus)),]$genus <- "Quercus"
  # stack all data
  all_data_raw <- rbind.fill(raw_2019,raw_2017)

# ## OR READ IN FROM ONE FOLDER ONLY:
#  # create list of paths to ex situ accessions CSV files in folder
#  file_list <- list.files(path=imls.exsitu,pattern=".csv",full.names=TRUE)
#  # read in each csv in path list to create list of dataframes
#  file_dfs <- lapply(file_list,read.csv,header=TRUE,fileEncoding="LATIN1",
#    strip.white=TRUE,colClasses="character",na.strings=c("","NA"))
#  print(length(file_dfs))
#    #sapply(file_dfs, nrow) # can look at number of rows in each csv
#  for(file in seq_along(file_dfs)){
#    # add file name as column, to record home institution for each record
#    file_dfs[[file]]$filename <- rep(file_list[file],
#      nrow(file_dfs[[file]]))
#    # remove file path portion
#    file_dfs[[file]]$filename <- mgsub(
#      file_dfs[[file]]$filename,c(paste0(imls.exsitu,"/"),".csv"),"")
#  }
#  print(head(file_dfs[[1]]))
#  # stack all datasets using rbind.fill, which keeps non-matching columns
#  #   and fills with NA; this may take a few minutes if you have lots of data
#  all_data_raw <- data.frame()
#  # first order dataframes by number of rows, to speed things up
#  file_dfs <- file_dfs[order(sapply(file_dfs,nrow))]
#  # now stack everything
#  for(i in 1:length(file_dfs)){
#    all_data_raw <- rbind.fill(file_dfs[[i]],all_data_raw)
#    print(paste(unique(file_dfs[[i]]$filename),i))
#  }
#  nrow(all_data_raw)
#  ncol(all_data_raw)

# new version before big changes, so can easily go back to original
all_data <- all_data_raw
# check out column names
sort(colnames(all_data))
# IF NEEDED: remove extra columns (can be created through Excel to CSV issues)
  all_data <- all_data[, -grep("^X", names(all_data))]
  # check schema to see if problems still exist (there should be 36 columns)
  str(all_data); sort(colnames(all_data)); ncol(all_data)
# IF NEEDED: separate column into multiple
all_data <- all_data %>% separate("specific",
  c("infra_rank_add","infra_name_add"),sep=" ",remove=T,fill="right")
# IF NEEDED: see which datasets have extraneous columns so you can fix manually
#  as desired; change line below as needed
  #unique(all_data$filename[all_data$ï..taxon_full_name!=""])
# IF NEEDED: merge similar columns (you may not need to do this if no schema
#   mistakes were made when manually editing column names).
#   Can do this step with lots of unites (see below) or from table ?????
# NOT WORKING:
#col_align <- read.csv(file.path(imls.meta,"GA2_exsitu_column_headers.csv"),
#  header=TRUE,strip.white=TRUE,colClasses="character",na.strings=c("","NA"))
#all_data2 <- all_data
#for(i in 1:ncol(col_align)){
#  col_group <- col_align[which(!is.na(col_align[,i])),][i]
#  for(j in 1:(nrow(col_group)-1)){
#    col_select <- all_data2[,which(names(all_data2)==col_group[j,1])]
#    not_na <- col_select[which(!is.na(col_select))]
#    all_data[,i] <- paste(part[j,1],part[j+1,1],sep=";")
#  }
#}

#### IMLS
  all_data <- tidyr::unite(all_data,"acc_num", c("acc_num","acc_no",
    "ï..acc_num"),sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"lin_num", c("lin_num","lin_no"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"inst_short", c("inst_short",
    "ï..inst_short"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"orig_source", c("orig_source","donor_name",
    "source2","source"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"garden_loc", c("garden_loc","loc"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"dataset_year", c("dataset_year",
    "updated_year"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"state", c("state","maj_region"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"county", c("county","min_region"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"municipality", c("municipality",
    "other_region"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"habitat", c("habitat","site_notes"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"notes", c("notes","orig_notes"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"taxon_full_name", c("taxon_full_name",
    "sp_full_name","ï..sp_full_name","ï..taxon_full_name"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"infra_name", c("infra_name",
    "infra_name_add"),sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"infra_rank", c("infra_rank",
    "infra_rank_add"),sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"name_determ", c("name_determ","id_by",
    "id_notes","uncert_id"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"num_indiv", c("num_indiv","num_plants",
    "no_alive","no_plants"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"coll_num", c("coll_num","coll_no"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"coll_year", c("coll_year","planted_year",
    "acq_year","aqu_year"),sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"condition", c("condition","plant_status"),
    sep=";",remove=T,na.rm=T)
    sort(colnames(all_data)); ncol(all_data)
# IF NEEDED: remove unused columns or rename columns
  all_data <- all_data[ , -which(names(all_data) %in% c("altitude",
    "altitude_units","author","common_name","coord_det","coord_precision",
    "garden_lat","garden_long","id_year","plant_age","plant_dbh",
    "plant_dbh_units","plant_ht","plant_ht_units","spatial_proj",
    "species_distrib","syn","voucher"))]
  #colnames(all_data)[colnames(all_data)=="elevation"] <- "altitude"
    sort(colnames(all_data)); ncol(all_data) # There should be max of 36...
  # acc_num,assoc_sp,coll_name,coll_num,coll_year,condition,country,county,
  # cultivar,dataset_year,filename,garden_loc,genus,germ_type,habitat,hybrid,
  # infra_name,infra_rank,inst_short,lin_num,locality,municipality,name_determ,
  # notes,num_indiv,orig_lat,orig_long,orig_source,private,prov_type,rec_as,
  # species,state,submission_year,taxon_full_name,trade_name

# fill in inst_short column with filename if none provided
all_data$inst_short[all_data$inst_short==""] <-
  all_data$filename[all_data$inst_short==""]
sort(unique(all_data$inst_short))
# remove rows with no inst_short
all_data <- all_data[which(all_data$inst_short!=""),]

# remove leading, trailing, and middle (e.g., double space) whitespace,
#   to prevent future errors
all_data <- as.data.frame(lapply(all_data, function(x) str_squish(x)),
  stringsAsFactors=F)
# replace "" cells with NA in whole dataset
all_data[all_data == ""] <- NA

# IF NEEDED:remove duplicates in PCN datasets if institution submitted their own
#     data separately
nrow(all_data) #111581
  # Magnolia
magnolia_remove <- c("GreenBayBG","JCRaulstonArb","QuarryhillBG",
  "UBritishColumbiaBG")
all_data <- all_data[!(all_data$filename=="PCNMagnolia" &
  all_data$inst_short %in% magnolia_remove),]
  # Acer
acer_remove <- c("QuarryhillBG","UBritishColumbiaBG","USNatlArb")
all_data <- all_data[!(all_data$filename=="PCNAcer" &
  all_data$inst_short %in% acer_remove),]
  # Quercus
quercus_remove <- c("MissouriBG","RanchoSantaAnaBG","StarhillForestArb")
all_data <- all_data[!(all_data$filename=="PCNQuercus" &
  all_data$inst_short %in% quercus_remove),]
nrow(all_data) #108722

# !! WORKING: create unique ID column
#all_data <- tidyr::unite(all_data,"unique_id",
#  c("inst_short","acc_num","taxon_full_name","genus","species","infra_rank",
#    "infra_name","cultivar","hybrid","prov_type","germ_type","coll_year",
#    "orig_lat","orig_long"),
#  sep=";",remove=F,na.rm=T)
#head(all_data[which(duplicated(all_data$unique_id)),])
#nrow(all_data[which(duplicated(all_data$unique_id)),])

# write raw CSV file
#write.csv(all_data,"exsitu_compiled_raw.csv",row.names=F)

################################################################################
# 2. Standardize species name columns
################################################################################

### Right now not looking at cultivars or hybrids - any work below involving
###   hybrids is to make sure records that are hybrids get recorded as such
###   (i.e., we don't want to count a hybrid as a pure specimen)

##
## A) Fill in taxon_full_name column for rows that only have parts
##       (e.g., genus, species, rank, etc.)

all_data2 <- all_data

# replace anything in hybrid column that does not notate it is a hybrid
unique(all_data2$hybrid)
all_data2 <- replace.value(all_data2, "hybrid", c("species"), NA)
# replace other hybrid signifiers with "x"
all_data2$hybrid <- mgsub(all_data2$hybrid, c("_","H","X","Hybrid","×","1",
  "XH","prob hybrid","unsusual hybrid","poss hybrid","unknown"), "x", fixed=T)

# preserve original taxon name
all_data2$taxon_full_name_orig <- all_data2$taxon_full_name
# create concatenated taxon_full_name column
all_data2 <- tidyr::unite(all_data2, "taxon_full_name_concat",
  c(genus,species,infra_rank,infra_name,hybrid,cultivar), sep=" ", remove=F,
  na.rm=T)
# trim whitespace
all_data2$taxon_full_name_concat <- str_squish(all_data2$taxon_full_name_concat)
# when blank, fill taxon_full_name column with concatenated full name
all_data2$taxon_full_name[is.na(all_data2$taxon_full_name)] <-
  all_data2$taxon_full_name_concat[is.na(all_data2$taxon_full_name)]

##
## B) Standardize taxon_full_name column then split into parts
##

all_data3 <- all_data2

# replace hybrid symbols with "x" in taxon_full_name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("_"," hybrid ","×"," X")," x ")
# make sure there is a space between var. and species name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("var.","v."), " var. ")
# make sure there is a space between subsp. and species name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("subsp.","ssp."), " subsp. ")
# make sure there is a space between f. and species name
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("f."), " f. ")

# IF YOU HAVE NO TARGET SPECIES BEGINNING WITH "x"...
# separate hybrid "x" from species name
all_data3$taxon_full_name <- gsub("(x)([[:alpha:]])", "\\1 \\2",
  all_data3$taxon_full_name)

# replace unwanted characters
all_data3$taxon_full_name <- mgsub(all_data3$taxon_full_name,
  c("ê","â","ô","õ","â","\'"), " ")
# trim whitespace
all_data3 <- as.data.frame(lapply(all_data3,str_squish),stringsAsFactors=F)

# separate out taxon full name and trim whitespace again
all_data3 <- all_data3 %>% separate("taxon_full_name",
  c("genus_new","species_new","extra1","extra2",
    "extra3","extra4","extra5","extra6"),sep=" ",extra="warn",
    remove=F,fill="right")
all_data3 <- as.data.frame(lapply(all_data3,str_squish),stringsAsFactors=F)

# fix genus and species capitalization issues
all_data3$genus_new <- str_to_title(all_data3$genus_new)
all_data3$species_new <- str_to_lower(all_data3$species_new)

# check genus names
sort(unique(all_data3$genus_new))
# IF NEEDED: fix misspellings or abbreviations
  all_data3$genus_new <- mgsub(all_data3$genus_new,
    c("Tila","Ulmua","Magnoliax","^M$","^M\\.","^Q.*"),
    c("Tilia","Ulmus","Magnolia","Magnolia","Magnolia","Quercus"),fixed=F)

# keep only rows from target genera
target_genera <- c("Quercus","Cyclobalanopsis","Malus","Tilia","Ulmus","Pyrus",
  "Crataegus","Docyniopsis","Eriolobus","Eulomalus","Euphorbia","Microptelea",
  "Planera","Sinomalus","Tithymalus","Acer","Magnolia")
all_data3 <- all_data3 %>% filter(all_data3$genus_new %in% target_genera)
#all_data3 <- all_data3 %>% filter(all_data3$genus_new %in%
#  unique(taxon_list$genus))
nrow(all_data3) #100224

# check species names
### you can go through here and find spelling errors you may want to add to
#     your synonym list
sort(unique(all_data3$species_new))

##
## C) Find infrataxa and hybrids
##

all_data4 <- all_data3

## look for infrataxa key words
# make data in all "extra" columns lower case
sp_col <- grep("^species_new$", colnames(all_data4))
all_data4[,sp_col:(sp_col+5)] <- as.data.frame(sapply(
  all_data4[,sp_col:(sp_col+5)], tolower), stringsAsFactors=F)
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
matches_i[,2] <- matches_i[,2]+sp_col
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

##
## D) Create final taxon full name
##

all_data5 <- all_data4

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
  # add genus_species column
all_data5$genus_species <- paste(all_data5$genus_new,all_data5$species_new)
  # if hybrid, mark genus_species column so it doesn't count as non-hybrid
  #   during matching to target taxa list
all_data5[which(grepl(" x ",all_data5$taxon_full_name)),]$genus_species <-
  paste(all_data5[which(grepl(" x ",
  all_data5$taxon_full_name)),]$genus_species,"x")

# check out results
sort(unique(all_data5$taxon_full_name))

# write file
#write.csv(all_data5, "exsitu_compiled_standardNames.csv",row.names=F)

################################################################################
# 3. Join to target taxa list
################################################################################

all_data6 <- all_data5

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list", "target_taxa_with_syn.csv"),
  header = T, na.strings = c("","NA"), colClasses = "character")
#taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list",
#  "global_quercus_and_IMLS_taxa_with_syn.csv"),
#  header = T, na.strings=c("","NA"), colClasses="character")

# rename some taxon name columns to preserve originals
setnames(all_data6,
  old = c("genus","species","infra_rank","infra_name"),
  new = c("genus_orig","species_orig","infra_rank_orig","infra_name_orig"))
setnames(all_data6,
  old = c("genus_new","species_new","infra_rank_new","infra_name_new",
    "taxon_full_name"),
  new = c("genus","species","infra_rank","infra_name",
    "taxon_full_name_created"))
# remove unused columns
all_data6 <- subset(all_data6, select = -c(taxon_full_name_concat,extra1,extra2,
  extra3,extra4,extra5,extra6))

# join dataset to species list
all_data6$taxon_name <- all_data6$taxon_full_name_created
  # full join to taxon list
all_data6 <- left_join(all_data6,taxon_list)
  # join again just by species name if no taxon match
need_match <- all_data6[which(is.na(all_data6$list)),]
  nrow(need_match) #61035
  # remove columns from first taxon name match
need_match <- need_match[,1:(ncol(all_data6)-ncol(taxon_list)+1)]
  # rename column for matching
need_match <- need_match %>% rename(taxon_name_full = taxon_name)
need_match$taxon_name <- need_match$genus_species
  # new join
need_match <- left_join(need_match,taxon_list)
  # bind together new matches and previously matched
matched <- all_data6[which(!is.na(all_data6$list)),]
matched$taxon_name_full <- matched$taxon_name
all_data7 <- rbind(matched,need_match)
  table(all_data7$list) # desiderata: 30989 | synonym: 8228
  # see how many rows have taxon name match
nrow(all_data7[which(!is.na(all_data7$list)),]) #39217 ; 45533 with gloabl oak

# write file
#write.csv(all_data7, file.path(imls.local, "exsitu_compiled_taxaMatched.csv"),
#  row.names = F)

################################################################################
# 5. Standardize important columns
################################################################################

all_data8 <- all_data7

##
## A) Provenance type
##

# look at column contents and change below phrases as needed
sort(unique(all_data8$prov_type))
## IF NEEDED: transfer contents of one column to another column, if data
#   needs to be preserved but is in wrong place
  #all_data8$notes[grep("Of known, direct wild origin - Florence County, SC.",
  #all_data8$prov_type)] <- "Florence County, SC"

# standardize column by searching for keywords and replacing with standard value
  # remove confusing words/phrases
all_data8$prov_type <- mgsub(all_data8$prov_type,
  c(". Accession not of wild source"), "")
  # ex wild (Z)
all_data8$prov_type <- ifelse(grepl(paste(
  c("indirect","ex wild","^z$","cultivated from wild","PropagatedFromWild",
    "ex W","G from W Plant"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"Z",all_data8$prov_type)
  # wild (W)
all_data8$prov_type <- ifelse(grepl(paste(
  c("wild","wld","collect","^w$","(W)"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"W",all_data8$prov_type)
  # native to site (N)
all_data8$prov_type <- ifelse(grepl(paste(
  c("native","existing"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"N",all_data8$prov_type)
  # cultivated (H)
all_data8$prov_type <- ifelse(grepl(paste(
  c("cultiva","garden","nursery","^c$","^g$","horticult","purchase"),
  collapse = "|"), all_data8$prov_type, ignore.case=T),"H",all_data8$prov_type)
  # unknown (U) ; everything else is unknown
all_data8$prov_type <- ifelse(all_data8$prov_type!= "W" &
  all_data8$prov_type != "Z" & all_data8$prov_type != "H" &
  all_data8$prov_type != "N","U",all_data8$prov_type)
all_data8$prov_type[which(is.na(all_data8$prov_type))] <- "U"

# check results
unique(all_data8$prov_type)

##
## B) Number of Individuals
##

# look at column contents
sort(unique(all_data8$num_indiv))
## IF NEEDED: replace unwanted characters
  all_data8$num_indiv <- mgsub(all_data8$num_indiv,
  c("+3","+","ca ","*"," in terra"," In terra","-3?","-Jan","?deck",
    " & 2","-Apr",":04","mass of ","?","deck","-10","inG7","Alive;",
    "Â","-Mar","/4"," "), "")
  sort(unique(all_data8$num_indiv))

# change type to numeric and replace NA with 1
all_data8$num_indiv <- as.numeric(all_data8$num_indiv)
all_data8$num_indiv[which(is.na(all_data8$num_indiv))] <- 1

# check results
sort(unique(all_data8$num_indiv))

##
## C) Latitude and Longitude
##

# preserve original lat and long columns
all_data8$lat_dd <- all_data8$orig_lat
all_data8$long_dd <- all_data8$orig_long

# replace comma with decimal (european notation)
#all_data8$lat_dd <- mgsub(all_data8$lat_dd, c(",",";"," ."), ".")
#all_data8$long_dd <- mgsub(all_data8$long_dd, c(",",";"), ".")

# replace unwanted characters
  ## latitude
    # non-ascii characters (characters with accents, degree symbol, etc.)
all_data8$lat_dd <- replace_non_ascii(all_data8$lat_dd,
  replacement=" ", remove.nonconverted=T)
  # random unnecessary characters
all_data8$lat_dd <- mgsub(all_data8$lat_dd,
  c("N","\\","\"","\'","/","M","A",": ","E","AZ","R","_","d","*",",","a","?"),
  " ")
    # remove leading zero
all_data8$lat_dd[which(grepl("^ *[0][1-9]+",all_data8$lat_dd))] <- gsub(
  "^ *[0]","",all_data8$lat_dd[which(grepl("^ *[0][1-9]+",all_data8$lat_dd))])
all_data8$lat_dd[which(grepl("^S *[0][1-9]+",all_data8$lat_dd))] <- gsub(
  "^S *[0]","-",all_data8$lat_dd[which(grepl("^S *[0][1-9]+",all_data8$lat_dd))])
    # add negative sign if south and remove "S"
all_data8$lat_dd[grep("S",all_data8$lat_dd,ignore.case=T)] <-
  paste("-",all_data8$lat_dd[grep("S",all_data8$lat_dd,ignore.case=T)],sep="")
all_data8$lat_dd <- gsub("S","",all_data8$lat_dd)
all_data8$lat_dd <- gsub("--","-",all_data8$lat_dd)
    # remove double spaces or leading/trailing whitespace
all_data8$lat_dd <- str_squish(all_data8$lat_dd)
sort(unique(all_data8$lat_dd))
  ## longitude
all_data8$long_dd <- replace_non_ascii(all_data8$long_dd,
  replacement=" ", remove.nonconverted=T)
all_data8$long_dd <- mgsub(all_data8$long_dd,
  c("E","\\","\"","\'","/","_","NR","d","*",",","A","a","?"," .")," ")
all_data8$long_dd[which(grepl("^ *[0][1-9]+",all_data8$long_dd))] <- gsub(
  "^ *[0]","",all_data8$long_dd[which(grepl("^ *[0][1-9]+",all_data8$long_dd))])
all_data8$long_dd[which(grepl("^W *[0][1-9]+",all_data8$long_dd))] <- gsub(
  "^W *[0]","-",all_data8$long_dd[which(grepl("^W *[0][1-9]+",
    all_data8$long_dd))])
all_data8$long_dd[grep("W",all_data8$long_dd,ignore.case=T)] <-
  paste("-",all_data8$long_dd[grep("W",all_data8$long_dd,ignore.case=T)],sep="")
all_data8$long_dd <- gsub("W","",all_data8$long_dd)
all_data8$long_dd <- mgsub(all_data8$long_dd,c("--","- "),"-")
all_data8$long_dd <- str_squish(all_data8$long_dd)
sort(unique(all_data8$long_dd))

# convert decimal-minutes-seconds (dms) to decimal degrees (dd)
#   [d, m, and s must be in the same cell, with 1 space between each value]
#   format = ## ## ## (DMS) OR ## ##.### (DM)
  # mark rows that need to be converted
convert <- all_data8[which(grepl(" ",all_data8$lat_dd) |
  grepl(" ",all_data8$long_dd)),]
  nrow(convert) #1885
unique(convert$lat_dd)
good <- anti_join(all_data8, convert)
  # separate by dec_min_sec and deg_dec_min then convert to decimal degrees
    # latitude
dms <- convert[which(str_count(convert$lat_dd," ") == 2),]; nrow(dms) #1474
ddm <- convert[which(str_count(convert$lat_dd," ") == 1),]; nrow(ddm) #390
other <- convert[which((str_count(convert$lat_dd," ") != 1 &
  str_count(convert$lat_dd," ") != 2) | is.na(str_count(convert$lat_dd," "))),]
  nrow(other) #21
dms$lat_dd = measurements::conv_unit(dms$lat_dd, from = 'deg_min_sec',
  to = 'dec_deg')
ddm$lat_dd = measurements::conv_unit(ddm$lat_dd, from = 'deg_dec_min',
  to = 'dec_deg')
convert <- rbind(dms,ddm,other); nrow(convert) #1885
    # longitude
dms <- convert[which(str_count(convert$long_dd," ") == 2),]; nrow(dms) #1477
ddm <- convert[which(str_count(convert$long_dd," ") == 1),]; nrow(ddm) #390
other <- convert[which((str_count(convert$long_dd," ") != 1 &
  str_count(convert$long_dd," ") != 2) | is.na(str_count(convert$long_dd," "))),]
  nrow(other) #18
  dms$long_dd = measurements::conv_unit(dms$long_dd, from = 'deg_min_sec',
    to = 'dec_deg')
  ddm$long_dd = measurements::conv_unit(ddm$long_dd, from = 'deg_dec_min',
    to = 'dec_deg')
  convert <- rbind(dms,ddm,other); nrow(convert) #1885
  # join everything back together
all_data9 <- rbind(good,convert); nrow(all_data9) #100224

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
#     G     H
# 11504 25191

# where prov_type is "H" but lat-long is given, change to "H?"
all_data9$prov_type[which(all_data9$gps_det == "G" &
  all_data9$prov_type == "H")] <- "H?"
table(all_data9$prov_type)

##
## D) Collection year
##

all_data9$coll_year <- gsub("([0-9]+);","",all_data9$coll_year)
unique(all_data9$coll_year)
all_data9$coll_year <- as.numeric(all_data9$coll_year)

##
## E) Locality
##

# create all_locality column
  # replace non-ascii characters
all_data9$locality <- replace_non_ascii(all_data9$locality)
all_data9$municipality <- replace_non_ascii(all_data9$municipality)
all_data9$county <- replace_non_ascii(all_data9$county)
all_data9$state <- replace_non_ascii(all_data9$state)
all_data9$country <- replace_non_ascii(all_data9$country)
all_data9$orig_source <- replace_non_ascii(all_data9$orig_source)
all_data9$notes <- replace_non_ascii(all_data9$notes)
  # create all_locality column
all_data9 <- unite(all_data9, "all_locality",
  c(locality,municipality,county,state,orig_source,notes),sep = " | ", #country
  remove = F)

################################################################################
# 6. Write files
################################################################################

# write file to use independently
write.csv(all_data9, file.path(imls.local, "exsitu_compiled_standardized.csv"),
  row.names = F)
# write file of just oaks
oaks <- all_data9 %>% filter(!is.na(US_native_oak))
write.csv(oaks, file.path(imls.local, "oaks_exsitu_compiled_standardized.csv"),
  row.names = F)

# view summary table
summary <- all_data9 %>%
  group_by(US_native_oak,IMLS,taxon_type,prov_type,gps_det) %>%
  count() %>%
  filter(taxon_type=="species" | is.na(taxon_type)); as.data.table(summary)

# rename columns and save to in situ data folder
all_data10 <- all_data9 %>%
  #filter(!is.na(lat_dd) & !is.na(long_dd)) %>%
  filter(!is.na(list)) %>%
  dplyr::select(taxon_full_name_created,coll_year,lat_dd,long_dd,acc_num,
    genus_species,name_determ,inst_short,locality,county,state,country,
    municipality,orig_source,notes)
setnames(all_data10,
  old = c("taxon_full_name_created","coll_year","lat_dd","long_dd",
          "acc_num","genus_species","name_determ",
          "locality","county","state","country","municipality",
          "inst_short","orig_source","notes"),
  new = c("taxon_name","year","decimalLatitude","decimalLongitude",
          "nativeDatabaseID","species_name","taxonIdentificationNotes",
          "locality","county","stateProvince","country","municipality",
          "datasetName","locationNotes","verbatimLocality"),
  skip_absent=T)
all_data10$database <- "Ex_situ"
all_data10$establishmentMeans <- "MANAGED"
all_data10$basisOfRecord <- "LIVING_SPECIMEN/SEED_BANK"
  # write file
write.csv(all_data10, file.path(imls.raw, "datasets_raw",
  "exsitu_raw.csv"), row.names = F)










### not using these parts right now ###

################################################################################
# Remove duplicate records
################################################################################

# remove duplicates
all_data10 <- ddply(all_data9,
                  .(inst_short,taxon_name_acc,taxon_full_name_orig,
                    taxon_name,taxon_full_name_created,list,
                    genus,species,infra_rank,infra_name,hybrid,
                    prov_type,lat_dd,long_dd,all_locality,gps_det,
                    country,municipality,state,county,locality,assoc_sp,notes,
                    acc_num,lin_num,orig_source,rec_as,germ_type,garden_loc,
                    coll_year,coll_num,coll_name,
                    notes,condition,name_determ,habitat,trade_name),
                    summarise, sum_num_indiv = sum(num_indiv))
  str(all_data10); nrow(all_data10) #96888

# replace commas with semicolon, just to be sure CSV works properly
all_data10[] <- lapply(all_data10, function(x) gsub(",", ";", x))

# write file
write.csv(all_data10, "exsitu_compiled_readyToGeolocate.csv")

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





#################
## Working on comparing country each point is in to the country column data

library('rnaturalearth')
library('rnaturalearthdata')
library('sf')
world_polygons <- ne_countries(type = 'countries', scale = 'medium')
wgs.proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	# select rows with coordinates
	latlong <- all_data9 %>% filter(!is.na(long_dd) & !is.na(lat_dd))
	# turn occurrence point data into a SpatialPointsDataFrame
	pts_df <- SpatialPointsDataFrame(latlong[,55:56], latlong, proj4string = wgs.proj)
	# reproject SpatialPointsDataFrame to specified projection
	#proj_df <- spTransform(sp_df,buff_proj)

## convert it to 'sf'
pts_df = st_as_sf(pts_df)
world_polygons = st_as_sf(world_polygons)
## intersect polygons with points, keeping the information from both
intersect = st_intersection(world_polygons, pts_df)
## transform into a 'data.frame' by removing the geometry
st_geometry(intersect) = NULL
t <- tidyr::unite(intersect,"country", c("name","country"),
  sep=" | ",remove=F,na.rm=F)
table(t$country)






################################################################################
# 3. OLD : Filter by target species names
################################################################################


# for comparison as we remove rows:
#nrow(all_data6) #101649

# create table with institutions and genera, for analysis
#genera <- all_data6 %>% filter(all_data6$genus_new %in%
#  c("Acer","Magnolia","Malus","Quercus","Tilia","Ulmus"))
#genera_g <- unique(genera %>% summarise(inst_short,genus_new))
#  str(genera_g)
#write.csv(genera_g, "genera_per_institution.csv")

# select rows that should be added back in, even if don't match genus_species
#add_back <- all_data6[which(all_data6$genus_new == "Acer" |
#                            all_data6$genus_new == "Magnolia"),]

# keep only rows with target species name
#all_data6 <- all_data6 %>% filter(all_data6$species_new %in%
#  unique(taxon_list$species))
#nrow(all_data6) #38681

# keep only rows for target genus_species
#taxon_list$genus_species <- paste(taxon_list$genus,taxon_list$species)
#all_data6 <- all_data6 %>% filter(all_data6$genus_species %in%
#  unique(taxon_list$genus_species))
#nrow(all_data6) #35222

# add selected rows back in
#all_data6 <- rbind(all_data6,add_back)
#  nrow(all_data6) #81210

# !!! check to see if institutions got excluded, and manually check those files
#   as needed, to see if issues
#setdiff(unique(all_data2$inst_short),unique(all_data6$inst_short))



### REMOVE UNWANTED CULTIVAR SYMBOLS ###

#all_data5 <- all_data4
# replace apostrophe
#all_data5$cultivar_new <- all_data5$cultivar
#all_data5$cultivar_new <- gsub("\\'s","s",all_data5$cultivar_new)
# remove characters after the second quote and before first quote
#all_data5$cultivar_new <-
#  gsub("([[:alpha:]])\\'.*","\\1",all_data5$cultivar_new)
#all_data5$cultivar_new <-
#  gsub("([[:alpha:]])\\\".*","\\1",all_data5$cultivar_new)
#all_data5$cultivar_new <- gsub("[[:alpha:]]\\'","",all_data5$cultivar_new)
# replace quotes and parentheses
#all_data5$cultivar_new <- mgsub(all_data5$cultivar_new,
#  c("\'","\"","(",")","[","]"), "")
# replace "_" with "X"
#all_data5$cultivar_new <- gsub("_","X",all_data5$cultivar_new)
# replace extra characters with ""
#all_data5$cultivar_new <- mgsub(all_data5$cultivar_new, c("M. ","√î","√µ"), "")
# replace non-cultivar names with NA
#all_data5$cultivar_new <- mgsub(all_data5$cultivar_new,
#  c("M. "," cultivar",":","cv.","cv","cvs.","yunnanensis X insignis"),"")
#all_data5$cultivar_new <- mgsub(all_data5$cultivar_new,
#  c("^var*","^subsp*","^ssp*","^\\."),"",fixed=FALSE)
#all_data5$cultivar_new <- gsub("^$",NA,all_data5$cultivar_new)
# capitalize
#all_data5$cultivar_new <- str_to_title(all_data5$cultivar_new)
# remove leading/trailing whitespace
#all_data5$cultivar_new <- str_squish(all_data5$cultivar_new)
#  sort(unique(all_data5$cultivar_new))
