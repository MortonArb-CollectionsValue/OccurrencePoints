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
  'measurements', 'naniar','CoordinateCleaner','rnaturalearth',
  'rnaturalearthdata','maps','raster','spatialEco')
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
  main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Consortia/Ex situ analysis"
  script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
# source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
# source('scripts/0-1_set_workingdirectory.R')

# set target genus/genera name (for file reading and writing)
target_genus <- "Magnolia"

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
  print(paste0("Number of files: ",length(file_dfs)))
    #sapply(file_dfs, nrow) # can look at number of rows in each csv
  for(file in seq_along(file_dfs)){
    df <- file_dfs[[file]]
    # add file name as column, to record home institution for each record
    df$filename <- rep(file_list[file],nrow(df))
    # remove file path portion
    df$filename <- mgsub(df$filename,c(paste0(path,"/"),".csv"),"")
    # add year of submission
    df$submission_year <- submission_year
    # remove extra blank columns
    t <- grepl("^X",names(df))
    if(length(unique(t))>1){
      print(df$filename[1])
      df <- df[, -grep("^X", names(df))]
    }
    # replace old df with new df
    file_dfs[[file]] <- df
    #print(head(file_dfs[[file]],n=2))
  }
  # stack all datasets using rbind.fill, which keeps non-matching columns
  #   and fills with NA; 'Reduce' iterates through and merges with previous
  # this may take a few minutes if you have lots of data
  all_data <- Reduce(rbind.fill, file_dfs)
    print(paste0("Number of rows: ",nrow(all_data)))
    print(paste0("Number of columns: ",ncol(all_data)))
  return(all_data)
}

# merge similar columns (you may not need to do this if no schema
#   mistakes were made when manually editing column names).
# Can do this step with lots of unites (see below) or from table ?????
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

################################################################################
################################################################################
# 1. Read in and stack all accessions data
################################################################################

### FIRST: After receiving accession data from institutions, you need to process
#   it manually. See here for instructions:
#   https://docs.google.com/spreadsheets/d/1QLxxWu-bUIRcrjHiaWeSz9n1ms4EQB3yQ8P8PBBx3Zk/edit?usp=sharing

#  read in data from multiple surveys and stack
raw_2020 <- read.exsitu.csv(file.path(main_dir,"inputs",
  "exsitu_standard_column_names","data_2020"), "2020")
  #names(raw_2020)
  raw_2020 <- raw_2020 %>% dplyr::select(-Other.standard.names.to.use.if.futher.fields.are.provided....)
raw_2019 <- read.exsitu.csv(file.path(main_dir,"inputs",
  "exsitu_standard_column_names","data_2019"), "2019")
  #names(raw_2019)
raw_2018 <- read.exsitu.csv(file.path(main_dir,"inputs",
  "exsitu_standard_column_names","data_2018_GapAnalysis10Genera"), "2018")
  raw_2018 <- raw_2018 %>% dplyr::select(-author,-datum,-Diameter,-family,
    -garden_lat,-garden_long,-inst_name,-isnt_name,-specific2)
  #names(raw_2018)
raw_2017 <- read.exsitu.csv(file.path(main_dir,"inputs",
  "exsitu_standard_column_names","data_2017_OakGapAnalysis"), "2017")
  #names(raw_2017)
  raw_2017 <- raw_2017 %>% dplyr::select(-common_name,-plant_age,-plant_ht,
    -plant_ht_units,-author,-species_distrib,-garden_lat,-garden_long,
    -altitude,-altitude_units,-plant_dbh,-plant_dbh_units,-spatial_proj,
    -syn,-id_year,-voucher)
# if genus is blank in 2017 data, make it "Quercus"
raw_2017[which(!is.na(raw_2017$species) &
                is.na(raw_2017$genus)),]$genus <- "Quercus"
# stack all data
to_stack <- list(raw_2020,raw_2019,raw_2018,raw_2017)
all_data_raw <- Reduce(rbind.fill,to_stack)

# create new version before big changes, so can easily go back to original
all_data <- all_data_raw

# replace non-ascii characters
  # first fix some common lat/long character issues
all_data$orig_lat <- mgsub(all_data$orig_lat,
  c("\"","ç","d","'","°","º","Â","¼","،","¡","_","´","*","À","?","`")," ")
all_data$orig_long <- mgsub(all_data$orig_long,
  c("\"","ç","d","'","°","º","Â","¼","،","¡","_","´","*","À","?","`")," ")
  # replace all non-ascii
all_data <- as.data.frame(lapply(all_data,replace_non_ascii),stringsAsFactors=F)

# check out column names
#sort(colnames(all_data))
# IF NEEDED: remove extra columns (can be created through Excel to CSV issues)
#  all_data <- all_data[, -grep("^X", names(all_data))]
# IF NEEDED: separate column into multiple
  all_data <- all_data %>% separate("specific",
    c("infra_rank_add","infra_name_add"),sep=" ",remove=T,fill="right")
# IF NEEDED: see which datasets have extraneous columns so you can fix manually
#  as desired; change line below as needed
  #unique(all_data$filename[all_data$ï..taxon_full_name!=""])
# IF NEEDED: merge similar columns (you may not need to do this if no schema
#   mistakes were made when manually editing column names).
## IMLS ##
  all_data <- tidyr::unite(all_data,"acc_num", c("acc_num","acc_no",
    "ï..acc_num"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"lin_num", c("lin_num","lin_no"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"inst_short", c("inst_short",
    "ï..inst_short"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"orig_source", c("orig_source","donor_name",
    "source2","source","donor"),
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
  all_data <- tidyr::unite(all_data,"habitat", c("habitat","site_notes",
    "habitat2"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"notes", c("notes","Notes","orig_notes",
    "coord_det","note"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"taxon_full_name", c("taxon_full_name",
    "sp_full_name","ï..sp_full_name","ï..taxon_full_name"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"infra_name", c("infra_name",
    "infra_name_add","intra_name","specific_name"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"infra_rank", c("infra_rank",
    "infra_rank_add","intra_rank","specific_rank"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"name_determ", c("name_determ","id_by",
    "id_notes","uncert_id"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"num_indiv", c("num_indiv","num_plants",
    "no_alive","no_plants","num_alive"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"coll_num", c("coll_num","coll_no",
    "coll_yr"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"coll_year", c("coll_year","planted_year",
    "acq_year","aqu_year","aqu_yr"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"condition", c("condition","plant_status",
    "status"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"hybrid", c("hybrid","hybrid.1"),
    sep=";",remove=T,na.rm=T)
  all_data <- tidyr::unite(all_data,"locality", c("locality","locallity",
    "locality2"),
    sep=";",remove=T,na.rm=T)
# IF NEEDED: remove unused columns or rename columns
  #all_data <- all_data[ , -which(names(all_data) %in% c("col1","col2"))]
  #colnames(all_data)[colnames(all_data)=="elevation"] <- "altitude"

### CHECK THINGS OUT ###
sort(colnames(all_data)); ncol(all_data)
# There should be max of 37 columns, including no more than:
  # acc_num,assoc_sp,coll_name,coll_num,coll_year,condition,coord_precision,
  # country,county,cultivar,dataset_year,filename,garden_loc,genus,germ_type,
  # habitat,hybrid,infra_name,infra_rank,inst_short,lin_num,locality,
  # municipality,name_determ,notes,num_indiv,orig_lat,orig_long,orig_source,
  # private,prov_type,rec_as,species,state,submission_year,taxon_full_name,
  # trade_name

# fill in inst_short column with filename if none provided
all_data$inst_short[all_data$inst_short==""] <-
  all_data$filename[all_data$inst_short==""]
# remove rows with no inst_short
all_data <- all_data[which(all_data$inst_short!=""),]
# IF NEEDED:remove duplicates in PCN datasets if institution submitted their own
#     data separately
nrow(all_data) #159435
  # Magnolia
magnolia_remove <- c("GreenBayBG","JCRaulstonArb","QuarryhillBG",
  "UBritishColumbiaBG","MortonArb")
all_data <- all_data[!(all_data$filename=="PCNMagnolia" &
  all_data$inst_short %in% magnolia_remove),]
  # Acer
acer_remove <- c("QuarryhillBG","UBritishColumbiaBG","USNatlArb","MortonArb")
all_data <- all_data[!(all_data$filename=="PCNAcer" &
  all_data$inst_short %in% acer_remove),]
  # Quercus
quercus_remove <- c("MissouriBG","RanchoSantaAnaBG","MortonArb")
all_data <- all_data[!(all_data$filename=="PCNQuercus" &
  all_data$inst_short %in% quercus_remove),]
  # Hackfalls Arb
all_data <- all_data[!(all_data$filename=="CultivatedOaks" &
  all_data$inst_short == "HackfallsArb"),]
nrow(all_data) #156362

### CHECK ALL INSTITUTIONS ARE HERE ###
sort(unique(all_data$inst_short))

# add unique ID column
nms <- names(all_data)
all_data <- all_data %>% mutate(UID=paste0('id', sprintf("%08d",
  1:nrow(all_data)))) %>% dplyr::select(c('UID', all_of(nms)))

# remove leading, trailing, and middle (e.g., double space) whitespace,
#   to prevent future errors
all_data <- as.data.frame(lapply(all_data, function(x) str_squish(x)),
  stringsAsFactors=F)
# replace "" cells with NA in whole dataset
all_data[all_data == ""] <- NA

################################################################################
# 2. Standardize taxon name columns
################################################################################

### !!! Currently removing hyrbids and cultivars without species name !!!

##
## A) Fill in taxon_full_name column for rows that only have parts
##    (e.g., genus, species, rank, etc.)

all_data2 <- all_data

# preserve original taxon name
all_data2$taxon_full_name_orig <- all_data2$taxon_full_name

# create concatenated taxon_full_name column
all_data2 <- tidyr::unite(all_data2, "taxon_full_name_concat",
  c(genus,species,infra_rank,infra_name,cultivar), sep=" ", remove=F,
  na.rm=T)
# when blank, fill taxon_full_name column with concatenated full name
all_data2[is.na(all_data2$taxon_full_name),]$taxon_full_name <-
  all_data2[is.na(all_data2$taxon_full_name),]$taxon_full_name_concat

##
## B) Standardize taxon_full_name column then split into parts;
##      remove hybrids and cultivars without species name
##

### REPLACE ANYTHING IN HYBRID COLUMN THAT MARKS IT AS A NON-HYBRID ###
sort(unique(all_data2$hybrid))
all_data2 <- replace.value(all_data2, "hybrid", c("species"), NA)
# remove rows with anything in 'hyrbid' column
all_data2 <- all_data2 %>% filter(is.na(hybrid))
nrow(all_data2)

# add space after periods in taxon_full_name
all_data2$taxon_full_name <- gsub(".",". ",all_data2$taxon_full_name,fixed=T)
# replace unwanted characters in taxon_full_name
all_data2$taxon_full_name <- mgsub(all_data2$taxon_full_name,
  c("(",")",";","[","]",",","^","#")," ")#"\'","\"",
all_data2$taxon_full_name <- str_squish(all_data2$taxon_full_name)
all_data2$taxon_full_name <- gsub("Quercus Quercus","Quercus",
  all_data2$taxon_full_name)
all_data2$taxon_full_name <- mgsub(all_data2$taxon_full_name,
  c("Magnolia Michelia","Magnolia Manglietia","Magnolia Mangletia",
    "Magnolia Magnolia","Magnolia Parakmeria","Magnolia M."), "Magnolia")

# trim whitespace
all_data2 <- as.data.frame(lapply(all_data2,str_squish),stringsAsFactors=F)
# view matches before replacing
#str_view_all(all_data2$taxon_full_name, pattern = " _([a-z]|[A-Z])", match = T)
#str_view_all(all_data2$taxon_full_name, pattern = " X([a-z]|[A-Z])", match = T)
#str_view_all(all_data2$taxon_full_name, pattern = "\\*", match = T)
#str_view_all(all_data2$taxon_full_name, pattern = "xx", match = T)
  # fix things that aren't hybrids
all_data2$taxon_full_name <- mgsub(all_data2$taxon_full_name,
  c("_taki","_vihov","_ko","Xhinckleyi"), c("taki","vihov","ko","X hinckleyi"))
# replace hybrid symbols with "x" in taxon_full_name
all_data2$taxon_full_name <- mgsub(all_data2$taxon_full_name,
  c(" _"," hybrid "," X ","*","xx","xxx")," x ") #"×"

# remove hybrids based on "x" in taxon_full_name
all_data3 <- all_data2 %>% filter(!grepl(" x ",taxon_full_name))
nrow(all_data3)
# see hybrids removed:
#sort(unique(anti_join(all_data2,all_data3)$taxon_full_name))

# remove records with uncertain taxon name
all_data4 <- all_data3 %>% filter(!grepl("?",taxon_full_name,fixed=T))
nrow(all_data4)
# see records removed:
#sort(unique(anti_join(all_data3,all_data4)$taxon_full_name))

# separate out taxon full name and trim whitespace again
all_data4 <- all_data4 %>% separate("taxon_full_name",
  c("genus_new","species_new","extra1","extra2",
    "extra3","extra4","extra5","extra6","extra7"),sep=" ",extra="warn",
    remove=F,fill="right")
all_data4 <- as.data.frame(lapply(all_data4,str_squish),stringsAsFactors=F)

# remove cultivars with no species name
all_data5 <- all_data4 %>%
  filter(!grepl("\"",species_new) & !grepl("\'",species_new))
nrow(all_data5)
# see records removed:
#sort(unique(anti_join(all_data4,all_data5)$taxon_full_name))

### CHECK GENUS NAMES ###
# fix genus capitalization issues
all_data5$genus_new <- str_to_title(all_data5$genus_new)
sort(unique(all_data5$genus_new))
# IF NEEDED: fix misspellings or abbreviations
all_data5$genus_new <- mgsub(all_data5$genus_new,
  c("\"",
    "^M$","^M.$","Michelia","Manglietia","Mangletia","Parakmeria",
    "^Mag.$","^Mag$","Magnlia","Magnolis",
    "^Q$","^Q.$","Querces","Quercu","Qurercus","Querucs","Querus","Quercuss"),
  c("",
    "Magnolia","Magnolia","Magnolia","Magnolia","Magnolia","Magnolia",
    "Magnolia","Magnolia","Magnolia","Magnolia",
    "Quercus","Quercus","Quercus","Quercus","Quercus","Quercus","Quercus",
    "Quercus"),
  fixed=F)
# keep only rows from target genera
target_genera <- c("Quercus","Acer","Magnolia","Malus","Tilia","Ulmus",
  "Kalopanax","Dipteronia","Cyclobalanopsis","Michelia","Manglietia")
#  "Pyrus","Crataegus","Docyniopsis","Eriolobus","Eulomalus","Euphorbia",
#  "Microptelea","Planera","Sinomalus","Tithymalus")
all_data6 <- all_data5 %>% filter(genus_new %in% target_genera)
nrow(all_data6)
# see records removed:
#sort(unique(anti_join(all_data5,all_data6)$taxon_full_name))

# look at genera and submission year for each institution
summary <- unique(data.frame(inst_short = all_data6$inst_short,
  genera = all_data6$genus_new,
  submission_year = all_data6$submission_year))
summary <- summary %>%
  arrange(genera, submission_year) %>%
  group_by(inst_short) %>%
  mutate(genera = paste(genera,collapse = ", "),
         submission_year = paste(submission_year,collapse = ", ")) %>%
  ungroup() %>%
  arrange(inst_short) %>%
  distinct(inst_short,.keep_all=T)
as.data.frame(summary)

### CHECK TO MAKE SURE NO GOOD SPECIES ARE BEING REMOVED ###
sort(unique(all_data6[which(grepl("[A-Z]",all_data6$species_new)),]$species_new))
# IF NEEDED: replace errors
all_data6$species_new <- gsub("bicolorWilld.","bicolor",all_data6$species_new)
all_data6$species_new <- gsub("cheniiNakai","chenii",all_data6$species_new)
all_data6$species_new <- gsub("Ilex","ilex",all_data6$species_new)
all_data6$species_new <- gsub("Iyrata","iyrata",all_data6$species_new)
all_data6$species_new <- gsub("Kobus","kobus",all_data6$species_new)
all_data6$species_new <- gsub("Liliflora","liliflora",all_data6$species_new)
all_data6$species_new <- gsub("Liliiflora","liliiflora",all_data6$species_new)
all_data6$species_new <- gsub("oocarpaE","oocarpa",all_data6$species_new)
all_data6$species_new <- gsub("platanoAdes","platano",all_data6$species_new)
all_data6$species_new <- gsub("Salicifolia","salicifolia",all_data6$species_new)
all_data6$species_new <- gsub("seemanniiE","seemannii",all_data6$species_new)
all_data6$species_new <- gsub("shirasaWanum","shirasa",all_data6$species_new)
all_data6$species_new <- gsub("Stellata","stellata",all_data6$species_new)
all_data6$species_new <- gsub("asheiA","ashei",all_data6$species_new)
all_data6$species_new <- gsub("Cathcartii","cathcartii",all_data6$species_new)
all_data6$species_new <- gsub("grandifloraA","grandiflora",all_data6$species_new)
all_data6$species_new <- gsub("IN","",all_data6$species_new)
all_data6$species_new <- gsub("Ex","",all_data6$species_new)
all_data6$species_new <- gsub("macrophyllaA","macrophylla",all_data6$species_new)
# remove remaining cultivars without species name
all_data7 <- all_data6 %>% filter(!grepl("[A-Z]",species_new))
# see records removed:
#sort(unique(anti_join(all_data6,all_data7)$taxon_full_name))

# remove records without specific/certain species name
all_data8 <- all_data7 %>%
  filter(species_new != "sp" & species_new != "species" &
         species_new != "cv" & species_new != "aff" &
         species_new != "unknown" & species_new != "undetermined" &
         species_new != "cf" & !grepl(".",species_new, fixed=T) &
         !grepl("[0-9]",species_new) & !is.na(species_new))
# see records removed:
#sort(unique(anti_join(all_data7,all_data8)$taxon_full_name))

##
## C) Find infrataxa
##

## look for infrataxa key words
# make data in all "extra" columns lower case
sp_col <- grep("^species_new$", colnames(all_data8))
all_data8[,sp_col:(sp_col+5)] <- as.data.frame(sapply(
  all_data8[,sp_col:(sp_col+5)], tolower), stringsAsFactors=F)
# create matrix of all "extra" species name columns, to search for
#   infraspecific key words
search.col <- matrix(cbind(all_data8$extra1,all_data8$extra2,all_data8$extra3,
  all_data8$extra4,all_data8$extra5,all_data8$extra6,all_data8$extra7),
  nrow=nrow(all_data8))
#str(search.col)
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
all_data8$infra_rank_new <- NA
all_data8$infra_rank_new[matches_i] <- all_data8[matches_i]
#all_data8$infra_rank_new[matches_h] <- "x"
#unique(all_data8$infra_rank_new) # check results

# create new infra_name column and fill with next column over from "extra"
#   contents that matched infraspecific key word
all_data8$infra_name_new <- NA
matches_i[,2] <- matches_i[,2]+1
all_data8$infra_name_new[matches_i] <- all_data8[matches_i]
#sort(unique(all_data8$infra_name_new))

# standardize infraspecific rank names
all_data8$infra_rank_new <- replace(all_data8$infra_rank_new,
  grep("^v$|^v.$|^var$|^variety$|^va$",all_data8$infra_rank_new), "var.")
all_data8$infra_rank_new <- replace(all_data8$infra_rank_new,
  grep("^subspecies$|^subsp$|^ssp$|^ssp.$|^subs.$|^spp.$",
  all_data8$infra_rank_new), "subsp.")
all_data8$infra_rank_new <- replace(all_data8$infra_rank_new,
 grep("^forma$|^form$|^fma$|^fo$|^fo.$|^f$",all_data8$infra_rank_new), "f.")
#unique(all_data8$infra_rank_new)

##
## D) Create final taxon full name
##

# create new taxon full name column
all_data8$taxon_full_name <- NA
  # select rows with infraspecific name and concatenate
yes_infra <- which(!is.na(all_data8$infra_rank_new) &
  !is.na(all_data8$infra_name_new))
all_data8$taxon_full_name[yes_infra] <- paste(all_data8$genus_new[yes_infra],
  all_data8$species_new[yes_infra], all_data8$infra_rank_new[yes_infra],
  all_data8$infra_name_new[yes_infra],sep=" ")
  # select rows without infraspecific name and concatenate
all_data8$taxon_full_name[-yes_infra] <- paste(all_data8$genus_new[-yes_infra],
  all_data8$species_new[-yes_infra],sep=" ")
# check out results
#sort(unique(all_data8$taxon_full_name))
# create genus_species column
all_data8$genus_species <- paste(all_data8$genus_new,all_data8$species_new)

################################################################################
# 3. Join to target taxa list
################################################################################

all_data9 <- all_data8

# read in target taxa list
taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list",
  paste0("target_",target_genus,"_taxa_with_syn.csv")), header = T,
  na.strings = c("","NA"),colClasses = "character")
if(length(unique(grepl("^X", names(taxon_list))))>1){
  taxon_list <- taxon_list[, -grep("^X", names(taxon_list))]
}
head(taxon_list)

# rename some taxon name columns to preserve originals
setnames(all_data9,
  old = c("genus","species","infra_rank","infra_name"),
  new = c("genus_orig","species_orig","infra_rank_orig","infra_name_orig"))
setnames(all_data9,
  old = c("genus_new","species_new","infra_rank_new","infra_name_new",
    "taxon_full_name"),
  new = c("genus","species","infra_rank","infra_name","taxon_name"))

# join dataset to species list
  # full join to taxon list
all_data9 <- left_join(all_data9,taxon_list)
  # join again just by species name if no taxon match
need_match <- all_data9[which(is.na(all_data9$list)),]
  nrow(need_match) #61035
    # remove columns from first taxon name match
need_match <- need_match[,1:(ncol(all_data9)-ncol(taxon_list)+1)]
    # rename column for matching
need_match <- need_match %>% rename(taxon_full_name = taxon_name)
need_match$taxon_name <- need_match$genus_species
    # new join
need_match <- left_join(need_match,taxon_list)
  # bind together new matches and previously matched
matched <- all_data9[which(!is.na(all_data9$list)),]
matched$taxon_full_name <- matched$taxon_name
all_data10 <- rbind(matched,need_match)
  table(all_data10$list) # desiderata: 30989 | synonym: 8228
  # see how many rows have taxon name match
nrow(all_data10[which(!is.na(all_data10$list)),]) #39217 ; 45533 with gloabl oak

# remove unused columns
  # columns to keep
all_data10 <- all_data10 %>% dplyr::select(
  # key data
  UID,inst_short,submission_year,species_name_acc,target_species,
  prov_type,num_indiv,acc_num,
  # locality
  orig_lat,orig_long,locality,municipality,county,state,country,
  assoc_sp,habitat,coord_precision,
  # source
  orig_source,lin_num,coll_num,coll_name,coll_year,
  # material info
  germ_type,garden_loc,rec_as,condition,name_determ,
  # other metadata
  notes,dataset_year,private,filename,list,
  # taxon name
  taxon_name_acc,taxon_full_name,genus,species,infra_rank,infra_name,
  taxon_full_name_orig,taxon_full_name_concat,genus_species,
  # not very important
  trade_name,cultivar)

### CHECK UNMATCHED SPECIES, TO ADD TO SYNONYM LIST AS NECESSARY ###
check <- all_data10 %>% filter(is.na(list) & genus == target_genus)
sort(unique(check$taxon_full_name))

# write file
#write.csv(all_data7, file.path(local_dir, "exsitu_compiled_taxaMatched.csv"),
#  row.names = F)

################################################################################
# 5. Standardize important columns
################################################################################

all_data11 <- all_data10

# add institution metadata
inst_data <- read.csv(file.path(main_dir,"inputs","respondent_institution_data_table",
  "respondent_institution_data_table_BATCH1.csv"))
str(inst_data)
all_data11 <- left_join(all_data11,inst_data)
str(all_data11)

##
## A) Provenance type
##

# look at column contents and change below phrases as needed
all_data11$prov_type <- str_to_lower(all_data11$prov_type)
sort(unique(all_data11$prov_type))
## IF NEEDED: transfer contents of one column to another column, if data
#   needs to be preserved but is in wrong place
  #all_data11$notes[grep("Of known, direct wild origin - Florence County, SC.",
  #all_data11$prov_type)] <- "Florence County, SC"

# standardize column by searching for keywords and replacing with standard value
  # remove confusing words/phrases
all_data11$prov_type <- mgsub(all_data11$prov_type,
  c(". accession not of wild source"," accession not of wild source",
    " accession not of a wild source"), "")
  # ex wild (Z)
all_data11$prov_type <- ifelse(grepl(paste(
  c("indirect","ex wild","^z$","cultivated from wild","propagatedfromwild",
    "ex W","g from w plant","c ex w"),
  collapse = "|"), all_data11$prov_type),"Z",all_data11$prov_type)
  # wild (W)
all_data11$prov_type <- ifelse(grepl(paste(
  c("wild","wld","collect","^w$","(w)","wd","nativecoll","coll2000"),
  collapse = "|"), all_data11$prov_type),"W",all_data11$prov_type)
  # native to site (N)
all_data11$prov_type <- ifelse(grepl(paste(
  c("native","existing","^n$"),
  collapse = "|"), all_data11$prov_type),"N",all_data11$prov_type)
  # cultivated (H)
all_data11$prov_type <- ifelse(grepl(paste(
  c("cultiva","garden","nursery","^c$","^g$","^h$","horticult","purchase",
  "coll"),
  collapse = "|"), all_data11$prov_type),"H",all_data11$prov_type)
  # unknown (U) ; everything else is unknown
all_data11$prov_type <- ifelse(all_data11$prov_type!= "W" &
  all_data11$prov_type != "Z" & all_data11$prov_type != "H" &
  all_data11$prov_type != "N","U",all_data11$prov_type)
all_data11$prov_type[which(is.na(all_data11$prov_type))] <- "U"

# check results
unique(all_data11$prov_type)

##
## B) Number of Individuals
##

# look at column contents
sort(unique(all_data11$num_indiv))
## IF NEEDED: replace unwanted characters
  all_data11$num_indiv <- mgsub(all_data11$num_indiv,
  c("+3","+","ca ","*"," in terra"," In terra","-3?","-Jan","?deck",
    " & 2","-Apr",":04","mass of ","?","deck","-10","inG7","Alive;",
    "A","-Mar","/4"," ","innur","inpots","inhoutunia","(4)","(4B&B)","(1)"), "")
  sort(unique(all_data11$num_indiv))

# change type to numeric and replace NA with 1
all_data11$num_indiv <- as.numeric(all_data11$num_indiv)
all_data11$num_indiv[which(is.na(all_data11$num_indiv))] <- 1

# check results
sort(unique(all_data11$num_indiv))

##
## C) Latitude and Longitude
##

# preserve original lat and long columns
all_data11$lat_dd <- all_data11$orig_lat
all_data11$long_dd <- all_data11$orig_long

# replace comma with decimal (european notation)
all_data11$lat_dd <- mgsub(all_data11$lat_dd, c(","), ".")
all_data11$long_dd <- mgsub(all_data11$long_dd, c(","), ".")

# replace unwanted characters
  ## latitude
  # replace random unnecessary characters
all_data11$lat_dd <- mgsub(all_data11$lat_dd,
  c("N","\\","/","M","A",": ","E","AZ","R","d","a"," .")," ")
    # remove leading zero
all_data11$lat_dd[which(grepl("^ *[0][1-9]+",all_data11$lat_dd))] <- gsub(
  "^ *[0]","",all_data11$lat_dd[which(grepl("^ *[0][1-9]+",all_data11$lat_dd))])
all_data11$lat_dd[which(grepl("^S *[0][1-9]+",all_data11$lat_dd))] <- gsub(
  "^S *[0]","-",all_data11$lat_dd[which(grepl("^S *[0][1-9]+",all_data11$lat_dd))])
    # add negative sign if south and remove "S"
all_data11$lat_dd[grep("S",all_data11$lat_dd,ignore.case=T)] <-
  paste("-",all_data11$lat_dd[grep("S",all_data11$lat_dd,ignore.case=T)],sep="")
all_data11$lat_dd <- gsub("S","",all_data11$lat_dd)
all_data11$lat_dd <- gsub("--","-",all_data11$lat_dd)
    # remove double spaces or leading/trailing whitespace
all_data11$lat_dd <- str_squish(all_data11$lat_dd)
#sort(unique(all_data11$lat_dd))
  # check source of specific values that aren't formatted correctly
#all_data11[which(all_data11$lat_dd == "422538"),]
  ## longitude
all_data11$long_dd <- replace_non_ascii(all_data11$long_dd,
  replacement=" ", remove.nonconverted=T)
all_data11$long_dd <- mgsub(all_data11$long_dd,
  c("E","\\","/","NR","d","A","a"," .","o","O")," ")
all_data11$long_dd[which(grepl("^ *[0][1-9]+",all_data11$long_dd))] <- gsub(
  "^ *[0]","",all_data11$long_dd[which(grepl("^ *[0][1-9]+",all_data11$long_dd))])
all_data11$long_dd[which(grepl("^W *[0][1-9]+",all_data11$long_dd))] <- gsub(
  "^W *[0]","-",all_data11$long_dd[which(grepl("^W *[0][1-9]+",
    all_data11$long_dd))])
all_data11$long_dd[grep("W",all_data11$long_dd,ignore.case=T)] <-
  paste("-",all_data11$long_dd[grep("W",all_data11$long_dd,ignore.case=T)],sep="")
all_data11$long_dd <- gsub("W","",all_data11$long_dd)
all_data11$long_dd <- mgsub(all_data11$long_dd,c("--","- "),"-")
all_data11$long_dd <- str_squish(all_data11$long_dd)
#sort(unique(all_data11$long_dd))

# convert decimal-minutes-seconds (dms) to decimal degrees (dd)
#   [d, m, and s must be in the same cell, with 1 space between each value]
#   format = ## ## ## (DMS) OR ## ##.### (DM)
  # mark rows that need to be converted
convert <- all_data11[which(grepl(" ",all_data11$lat_dd) |
  grepl(" ",all_data11$long_dd)),]
  nrow(convert) #1885
unique(convert$lat_dd)
good <- anti_join(all_data11, convert)
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
all_data12 <- rbind(good,convert); nrow(all_data12) #100224

# check validity of lat and long
all_data12$lat_dd <- as.numeric(all_data12$lat_dd)
  #sort(unique(all_data12$lat_dd))
all_data12$long_dd <- as.numeric(all_data12$long_dd)
  #sort(unique(all_data12$long_dd))
  # if coords are both 0, set to NA
zero <- which(all_data12$lat_dd == 0 & all_data12$long_dd == 0)
all_data12$lat_dd[zero] <- NA; all_data12$long_dd[zero] <- NA
  # flag non-numeric and not available coordinates and lat > 90, lat < -90,
  # lon > 180, and lon < -180
coord_test <- cc_val(all_data12, lon = "long_dd",lat = "lat_dd",
  value = "flagged", verbose = TRUE) #Flagged 496009 records.
  # try switching lat and long for invalid points and check validity again
all_data12[!coord_test,c("lat_dd","long_dd")] <-
  all_data12[!coord_test,c("long_dd","lat_dd")]
coord_test <- cc_val(all_data12,lon = "long_dd",lat = "lat_dd",
  value = "flagged",verbose = TRUE) #Flagged 496002 records.
  # make coords NA if they are still flagged
all_data12[!coord_test,c("lat_dd","long_dd")] <- c(NA,NA)

# check if geolocated points are in water and mark
world_polygons <- ne_countries(type = 'countries', scale = 'medium')
geo_pts <- all_data12 %>% filter(!is.na(lat_dd) & !is.na(long_dd))
in_water <- geo_pts[is.na(map.where(world_polygons,
  geo_pts$long_dd,geo_pts$lat_dd)),]
nrow(in_water)
all_data12$flag <- ""
all_data12[which(all_data12$UID %in% in_water$UID),]$flag <-
  "Given lat-long is in water"
table(all_data12$flag)
#all_data12[which(all_data12$UID %in% in_water$UID),]$lat_dd <- NA
#all_data12[which(all_data12$UID %in% in_water$UID),]$long_dd <- NA

# mark lat-long for records with same inst lat-long and wild lat-long
all_data12$lat_round <- round(all_data12$lat_dd,digits=1)
all_data12$long_round <- round(all_data12$long_dd,digits=1)
all_data12$inst_lat_round <- round(all_data12$inst_lat,digits=1)
all_data12$inst_long_round <- round(all_data12$inst_long,digits=1)
garden_latlong <- all_data12 %>% filter(lat_round == inst_lat_round &
  long_round == inst_long_round & prov_type != "N")
unique(garden_latlong$inst_short)
nrow(garden_latlong)
all_data12[which(all_data12$UID %in% garden_latlong$UID),]$flag <-
  "Given lat-long is at institution, use only if native to grounds"
#all_data12[all_data12$UID %in% garden_latlong$UID,]$lat_dd <- NA
#all_data12[all_data12$UID %in% garden_latlong$UID,]$long_dd <- NA
table(all_data12$flag)

# add country-level information to check if lat-long in right spot
# create SpatialPointsDataFrame
proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
geo_pts_spatial <- SpatialPointsDataFrame(geo_pts[,c("long_dd",
  "lat_dd")], geo_pts, proj4string = CRS(proj4string4poly))
# add country polygon data to each point based on lat-long location
load(file.path(main_dir, "inputs", "gis_data", "admin_shapefiles.RData"))
geo_pts <- point.in.poly(geo_pts_spatial, adm0.poly, sp=TRUE)@data
# try switching lat and long for points in Antarctica
geo_pts[which(geo_pts$country.name == "Antarctica"),c("lat_dd","long_dd")]<-
  geo_pts[which(geo_pts$country.name == "Antarctica"),c("long_dd","lat_dd")]
# round 2: add country-level information to check if lat-long in right spot
geo_pts <- geo_pts %>%
  dplyr::select(-country.name,-country.iso_a2,-country.iso_a3,-country.continent)
geo_pts_spatial <- SpatialPointsDataFrame(geo_pts[,c("long_dd",
  "lat_dd")], geo_pts, proj4string = CRS(proj4string4poly))
geo_pts <- point.in.poly(geo_pts_spatial, adm0.poly, sp=TRUE)@data
geo_pts <- geo_pts %>% dplyr::select(UID,country.name) %>%
  rename(latlong_country = country.name)
all_data12 <- full_join(all_data12,geo_pts)

# add gps_det (gps determination) column
all_data12$gps_det <- NA
all_data12$gps_det[which(all_data12$prov_type == "H")] <- "H"
all_data12$gps_det[which(!is.na(all_data12$lat_dd) &
  !is.na(all_data12$long_dd))] <- "G"
table(all_data12$gps_det)
#     G     H
# 11504 25191

# where prov_type is "H" but lat-long is given, change to "H?"
all_data12$prov_type[which(all_data12$gps_det == "G" &
  all_data12$prov_type == "H")] <- "H?"
table(all_data12$prov_type)

##
## D) Collection year
##

all_data12$coll_year <- mgsub(all_data12$coll_year,
  c("([0-9]+);","about ","ca.","Unknown","original","Estate","estate"),"")
all_data12$coll_year[which(all_data12$coll_year == "")] <- NA
unique(all_data12$coll_year)
#all_data12$coll_year <- as.numeric(all_data12$coll_year)

##
## E) Locality
##

# create all_locality column
all_data12 <- unite(all_data12, "all_locality",
  c(locality,municipality,county,state,country,orig_source,notes),sep = " | ",
  remove = F)
# remove NA in concatenated locality column
all_data12$all_locality <- gsub("NA","",all_data12$all_locality)
# if no locality info at all, make it NA
all_data12$all_locality <- gsub(" |  |  |  |  |  | ", NA,
  all_data12$all_locality, fixed = T)

##
## F) Condition
##

# get rid of dead or removed germplasm
nrow(all_data12)
all_data12$condition <- str_to_lower(all_data12$condition)
all_data12 <- all_data12 %>%
  filter(!grepl("dead|removed|rmvd",condition))
sort(unique(all_data12$condition))
nrow(all_data12)

##
## SELECT AND ORDER FINAL COLUMNS
##

all_data13 <- all_data12 %>% dplyr::select(
  # key data
  UID,inst_short,submission_year,species_name_acc,target_species,
  prov_type,gps_det,flag,latlong_country,lat_dd,long_dd,coord_precision,
  all_locality,
  # locality
  locality,municipality,county,state,country,orig_lat,orig_long,
  orig_source,notes,assoc_sp,habitat,num_indiv,acc_num,
  # source
  lin_num,coll_num,coll_name,coll_year,
  # material info
  germ_type,garden_loc,rec_as,condition,name_determ,
  # other metadata
  dataset_year,private,filename,list,
  # taxon name
  taxon_name_acc,taxon_full_name,genus,species,infra_rank,infra_name,
  taxon_full_name_orig,taxon_full_name_concat,cultivar,trade_name,
  # institution metadata
  inst_name,inst_country,inst_lat,inst_long)
str(all_data13)

# write file
write.csv(all_data13, file.path(main_dir,"Compiled ex situ data",
  paste0(target_genus,"_BATCH1_exsitu_compiled_standardized.csv")),row.names = F)

##
## SPLIT BY SPECIES
##

all_data14 <- all_data13 %>% filter(target_species == "Y")

# create one CSV for each target species
sp_split <- split(all_data14, as.factor(all_data14$species_name_acc))
names(sp_split) <- gsub(" ","_",names(sp_split))

# write files
if(!dir.exists(file.path(main_dir,"Compiled ex situ data","CSV_by_target_species")))
  dir.create(file.path(main_dir,"Compiled ex situ data","CSV_by_target_species"),
  recursive=T)
lapply(seq_along(sp_split), function(i) write.csv(sp_split[[i]],
  file.path(main_dir,"Compiled ex situ data","CSV_by_target_species",
  paste0(names(sp_split)[[i]], ".csv")),row.names = F))














################################################################################
# 6. Write files
################################################################################

# condense duplicates
  # create rounded latitude and longitude columns for removing duplicates
  #   number of digits can be changed based on how dense you want data
all_data12$lat_round <- round(all_data12$lat_dd,digits=3)
all_data12$long_round <- round(all_data12$long_dd,digits=3)
  # remove duplicates
all_data13 <- all_data12 %>%
  group_by(taxon_name_acc,lat_round,long_round,all_locality,prov_type,
    inst_short) %>%
  #mutate(inst_short_all = paste(inst_short,collapse = "; "),
  #  prov_type_all = paste(prov_type,collapse = "; "),
  #  locality_all = paste(all_locality,collapse = "; "),
  #  acc_num_all = paste(acc_num,collapse = "; ")) %>%
  summarise(sum_num_plt = sum(num_indiv)) %>%
  ungroup()
  str(all_data13); nrow(all_data13)

# write file to use independently
write.csv(all_data10, file.path(local, "exsitu_compiled_standardized.csv"),
  row.names = F)


old_data <- read.csv(file.path(local, "GA2_exsitu_compiled_targetSpecies_standardized_nodup_8_22_3.csv"),
  header = T, na.strings = c("","NA"), colClasses = "character")
new_data <- read.csv(file.path(local, "exsitu_compiled_standardized_7_15_Sassafras.csv"),
  header = T, na.strings = c("","NA"))#, colClasses = "character")
chart <- read.csv(file.path(local, "exsitu_compiled_standardized_7_15_20_ForCharts.csv"),
  header = T, na.strings = c("","NA"))#, colClasses = "character")

all <- full_join(new_data,old_data)

new_data$lat_round <- round(new_data$lat_round,digits=1)
new_data$long_round <- round(new_data$long_round,digits=1)

new_data2 <- new_data %>%
  filter(sum_num_plt>0) %>%
  group_by(taxon_name_acc,lat_round,long_round) %>%
  #mutate(inst_short_all = paste(inst_short,collapse = "; "),
  summarise(sum_num_indiv = sum(sum_num_plt),
            inst_short_all = paste(inst_short,collapse="; "),
            prov_type_all = paste(prov_type,collapse = "; "),
            gps_det_all = paste(gps_det,collapse = "; "),
            locality_all = paste(all_locality,collapse = "; ")) %>%
  ungroup()
str(new_data2)

write.csv(new_data2, file.path(GA2_folder,
  "exsitu_compiled_standardized_dupsRemoved_7_15_20_Sassafras.csv"),
  row.names = T)

chart <- chart %>%
  filter(sum_num_plt>0) %>%
  group_by(taxon_name_acc,prov_type,inst_short,inst_continent,inst_country) %>%
  summarise(sum_num_indiv = sum(sum_num_plt)) %>%
  ungroup()
str(chart)

write.csv(chart, file.path(GA2_folder,
  "exsitu_compiled_standardized_7_15_20_ForCharts.csv"),
  row.names = T)



# view summary table
summary <- all_data12 %>%
  group_by(US_native_oak,IMLS,taxon_type,prov_type,gps_det) %>%
  count() %>%
  filter(taxon_type=="species" | is.na(taxon_type)); as.data.table(summary)

# rename columns and save to in situ data folder
all_data10 <- all_data12 %>%
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
  new = c("taxon_name","year","lat_dd","long_dd",
          "nativeDatabaseID","species_name","taxonIdentificationNotes",
          "locality","county","stateProvince","country","municipality",
          "datasetName","locationNotes","verbatimLocality"),
  skip_absent=T)
all_data10$database <- "Ex_situ"
all_data10$establishmentMeans <- "MANAGED"
all_data10$basisOfRecord <- "LIVING_SPECIMEN/SEED_BANK"
  # write file
write.csv(all_data10, file.path(raw, "datasets_raw",
  "exsitu_raw.csv"), row.names = F)










### not using these parts right now ###

################################################################################
# Remove duplicate records
################################################################################

# remove duplicates
all_data10 <- ddply(all_data12,
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
  write.csv(morton, "exsitu_compiled_Mortoncsv")





#################
## Working on comparing country each point is in to the country column data

library('rnaturalearth')
library('rnaturalearthdata')
library('sf')
world_polygons <- ne_countries(type = 'countries', scale = 'medium')
wgs.proj <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
	# select rows with coordinates
	latlong <- all_data12 %>% filter(!is.na(long_dd) & !is.na(lat_dd))
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
