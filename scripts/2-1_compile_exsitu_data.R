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
target_genus <- "Acer"

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
    # add accession number if there isn't one
    if("acc_num" %in% names(df) & nrow(df[which(is.na(df$acc_num)),]) > 0){
      df[which(is.na(df$acc_num)),]$acc_num <- paste0("added",
        sprintf("%08d", 1:nrow(df[which(is.na(df$acc_num)),])))
    } else if ("acc_no" %in% names(df) & nrow(df[which(is.na(df$acc_no)),]) > 0){
      df[which(is.na(df$acc_no)),]$acc_no <- paste0("added",
        sprintf("%08d", 1:nrow(df[which(is.na(df$acc_no)),])))
    } else if (!("acc_num" %in% names(df)) & !("acc_no" %in% names(df))){
      df$acc_num <- paste0("added", sprintf("%08d", 1:nrow(df)))
    } else {
      #print(paste("NO ACC NUM EDITS:",df$filename[1]))
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
  all_data <- tidyr::unite(all_data,"acc_num", c("acc_num","acc_no"),
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
    "sp_full_name","ï..sp_full_name"),
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
magnolia_remove <- c("JCRaulstonArb","QuarryhillBG",
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
  c(" _"," hybrid "," X ","*","xx","xxx"," A")," x ") #"×"

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
         species_new != "cv" & #species_new != "aff" &
         species_new != "unknown" & species_new != "undetermined" &
         species_new != "cf" & !grepl(".",species_new, fixed=T) &
         !grepl("[0-9]",species_new) & !is.na(species_new))
  # there is an accepted Acer name with "aff" -- add the period back
all_data8$taxon_full_name <- gsub(" aff "," aff. ",all_data8$taxon_full_name)
all_data8$species_new <- gsub("aff","aff.",all_data8$species_new)
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
  inst_short,submission_year,species_name_acc,target_species,
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

### keep only matched names
all_data11 <- all_data10 %>% filter(!is.na(list))

# write file
#write.csv(all_data7, file.path(local_dir, "exsitu_compiled_taxaMatched.csv"),
#  row.names = F)

################################################################################
# 5. Standardize important columns
################################################################################

#all_data11 <- all_data10

# add institution metadata
inst_data <- read.csv(file.path(main_dir,"inputs","respondent_institution_data_table",
  "respondent_institution_data_table_2020.csv"),stringsAsFactors = F)
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

sort(unique(all_data11$num_indiv))
  ## IF NEEDED: replace unwanted characters
  all_data11$num_indiv <- mgsub(all_data11$num_indiv,
  c("+3","+","ca ","*"," in terra"," In terra","-3?","-Jan","?deck","~","E",
    " & 2","-Apr",":04","mass of ","?","deck","-10","inG7","Alive;","RJ",
    "A","-Mar","/4"," ","innur","inpots","inhoutunia","(4)","(4B&B)","(1)"), "")
  sort(unique(all_data11$num_indiv))
  # change type to numeric and replace NA with 1
all_data11$num_indiv <- as.numeric(all_data11$num_indiv)
all_data11$num_indiv[which(is.na(all_data11$num_indiv))] <- 1

# check results
sort(unique(all_data11$num_indiv))
nrow(all_data11) #93958

# remove records with no individuals
all_data11 <- all_data11[which(all_data11$num_indiv > 0),]
nrow(all_data11) #92182

##
## ** ADD Unique ID Column
##

# add unique ID column
  # create UID with institution name, acc num, provenance type, and taxon name
  # also remove duplicates based on new UID and sum individuals
  # first, fix up number of individuals column before summing
  # now create UID and remove dups
nms <- names(all_data11)
nrow(all_data11)
all_data11 <- all_data11 %>%
  arrange(orig_lat,locality) %>%
  mutate(UID = paste(inst_short,acc_num,prov_type,taxon_name_acc,sep="~")) %>%
  group_by(UID) %>%
  mutate(num_indiv = sum(as.numeric(num_indiv))) %>%
  distinct(UID,.keep_all=T) %>%
  ungroup() %>%
  dplyr::select(c("UID",all_of(nms)))
nrow(all_data11)

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
# remove extra elements so its just year
all_data12$coll_year <- gsub(";[1-2][0-9][0-9][0-9]","",all_data12$coll_year)
all_data12$coll_year <- gsub("[0-9][0-9]/[0-9][0-9]/","",all_data12$coll_year)
all_data12$coll_year <- gsub("[0-9]/[0-9][0-9]/","",all_data12$coll_year)
all_data12$coll_year <- gsub("[0-9][0-9]/[0-9]/","",all_data12$coll_year)
all_data12$coll_year <- gsub("[0-9]/[0-9]/","",all_data12$coll_year)
all_data12$coll_year <- gsub("[1-9] [A-Z][a-z][a-z] ","",all_data12$coll_year)
all_data12$coll_year <- gsub("[A-Z][a-z][a-z] ","",all_data12$coll_year)
all_data12$coll_year <- gsub("[1-9]-[A-Z][a-z][a-z]-","",all_data12$coll_year)
# make column numeric
all_data12$coll_year <- as.numeric(all_data12$coll_year)
  # assume 2000s if values is less than 21
all_data12$coll_year[which(all_data12$coll_year < 10)] <-
  paste0("200",as.character(all_data12$coll_year[which(all_data12$coll_year < 10)]))
all_data12$coll_year <- as.numeric(all_data12$coll_year)
all_data12$coll_year[which(all_data12$coll_year < 21)] <-
  paste0("20",as.character(all_data12$coll_year[which(all_data12$coll_year < 21)]))
all_data12$coll_year <- as.numeric(all_data12$coll_year)
  # assume 1900s if values is greater than or equal to 21
all_data12$coll_year[which(all_data12$coll_year < 100)] <-
  paste0("19",as.character(all_data12$coll_year[which(all_data12$coll_year < 100)]))
all_data12$coll_year <- as.numeric(all_data12$coll_year)
unique(all_data12$coll_year)

##
## E) Locality
##

# create all_locality column
all_data12$latitude <- round(all_data12$lat_dd,digits=3)
all_data12$longitude <- round(all_data12$long_dd,digits=3)
all_data12 <- unite(all_data12, "all_locality",
  c(locality,municipality,county,state,country,orig_source,#notes,
    latitude,longitude),sep = " | ",remove = F)
# remove NA in concatenated locality column
all_data12$all_locality <- gsub("NA","",all_data12$all_locality)
# if no locality info at all, make it NA
all_data12$all_locality[which(all_data12$all_locality ==
  " |  |  |  |  |  |  | ")] <- NA

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

all_data12 <- as.data.frame(lapply(all_data12, function(x) str_squish(x)),
  stringsAsFactors=F)
all_data12 <- as.data.frame(lapply(all_data12, function(x) gsub(",",";",x)),
  stringsAsFactors=F)

all_data13 <- all_data12 %>% dplyr::select(
  # key data
  UID,inst_short,submission_year,species_name_acc,target_species,
  prov_type,gps_det,flag,lat_dd,long_dd,all_locality,
  # locality
  locality,municipality,county,state,country,orig_lat,orig_long,
  orig_source,notes,assoc_sp,habitat,num_indiv,acc_num,coord_precision,
  latlong_country,
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
#write.csv(all_data13, file.path(main_dir,"Compiled ex situ data",
#  paste0(target_genus,"_exsitu_compiled_standardized.csv")),row.names = F)

##
## RENAME FOR GEOLOCATE AND SPLIT BY SPECIES
##

  # add GEOLocate standard columns
all_data12$correction.status <- NA
all_data12$precision <- NA
all_data12$error.polygon <- NA
all_data12$multiple.results <- NA

all_data14 <- all_data12 %>%
  # filter to remove cultivated records and those without locality info
  filter(target_species == "Y") %>%
  filter(prov_type != "H") %>%
  filter(!is.na(all_locality)) %>%
  # rename to GEOLocate standard columns
  rename(locality.string = all_locality,
         uncertainty = coord_precision) %>%
  # order with NA lat-long records on top
  arrange(locality.string) %>%
  arrange(!is.na(latitude),latitude) %>%
  # replace NA with "" to maek simpler to view in GEOLocate
  replace(., is.na(.), "") %>%
  # group by all non-ID fields
  group_by(locality.string,country,state,county,latitude,longitude,
  uncertainty,flag,gps_det,prov_type,coll_name,coll_year,
  inst_short,filename,submission_year,inst_lat,inst_long,
  inst_country,list,species_name_acc,taxon_full_name) %>%
  # concatenate values in ID fields
  mutate(UID = paste(UID, collapse="|"),
    acc_num = paste(acc_num, collapse="|"),
    lin_num = paste(lin_num, collapse="|"),
    coll_num = paste(coll_num, collapse="|"),
    sum_num_indiv = sum(as.numeric(num_indiv))) %>%
  ungroup() %>%
  # remove duplicates
  distinct(locality.string,country,state,county,latitude,longitude,
  uncertainty,flag,gps_det,prov_type,coll_name,coll_year,
  inst_short,filename,submission_year,inst_lat,inst_long,
  inst_country,list,species_name_acc,taxon_full_name,.keep_all=T) %>%
  # reorder columns
  dplyr::select(
    ## GeoLocate
    locality.string,country,state,county,latitude,longitude,
    correction.status,precision,error.polygon,multiple.results,uncertainty,
    ## record metadata
    flag,gps_det,prov_type,acc_num,lin_num,coll_num,coll_name,coll_year,
    sum_num_indiv,
    ## institituion metadata
    inst_short,filename,submission_year,inst_lat,inst_long,inst_country,
    ## taxon name & record ID
    list,species_name_acc,taxon_full_name,UID) %>%
  # rename concatenated fields to make that clear
  rename(acc_num_CONCAT = acc_num,
    lin_num_CONCAT = lin_num,
    coll_num_CONCAT = coll_num,
    UID_CONCAT = UID)

# remove dot in column names (replace with space) for GEOLocate
names(all_data14) <- gsub(x = names(all_data14),pattern = "\\.",
  replacement = " ")
# remove extra separators in concatenated columns
all_data14$acc_num_CONCAT <- mgsub(all_data14$acc_num_CONCAT,c("||","^|$"),"",fixed=F)
all_data14$lin_num_CONCAT <- mgsub(all_data14$lin_num_CONCAT,c("||","^|$"),"",fixed=F)
all_data14$coll_num_CONCAT <- mgsub(all_data14$coll_num_CONCAT,c("||","^|$"),"",fixed=F)
all_data14$UID_CONCAT <- mgsub(all_data14$UID_CONCAT,c("||","^|$"),"",fixed=F)
str(all_data14)

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




### Read geolocated CSVs back in and add geolocate info to rest of data

# read in geolocated CSVs
file_list <- list.files(
  path=file.path(main_dir,"Compiled ex situ data","Geolocated_CSV_by_target_species",target_genus),
  pattern=".csv",full.names=TRUE)
file_dfs <- lapply(file_list,read.csv,header=TRUE,fileEncoding="LATIN1",
  strip.white=TRUE,stringsAsFactors=F,na.strings=c("","NA"))
# check that geolocated CSVs each have same number of rows as inital export;
# if they don't, you'll need to manually see where the mistake is
for(i in 1:length(sp_split)){
  for(j in 1:nrow(sp_split[[i]])){
    if(sp_split[[i]]$inst_short[j] !=file_dfs[[i]]$inst_short[j]){
      print(sp_split[[i]]$species_name_acc[1])
    } else {
      print(i)
    }
  }
}

# bind geolocated CSVs together
post_geo <- Reduce(rbind.fill, file_dfs)
# fix a few inconsistencies
  ## provenance type column
unique(post_geo$prov_type)
    # check "H?" rows to see if should be "W" and
    # if all are "X" gps_det, change prov_type to "H"
post_geo[which(post_geo$prov_type == "H?"),]
post_geo[which(post_geo$prov_type == "H?"),]$prov_type <- "H"
    # check prov_type for rows with coordinates
unique(post_geo[which(!is.na(post_geo$latitude)),]$prov_type)
post_geo[which(!is.na(post_geo$latitude) & post_geo$prov_type == "U"),]
  ## gps determination column
unique(post_geo$gps_det)
post_geo[which(is.na(post_geo$gps_det)),]$latitude
post_geo[which(is.na(post_geo$gps_det)),]$gps_det <- "X"
  ## uncertainty column
post_geo$uncertainty <- gsub(" m","",post_geo$uncertainty)
post_geo[which(post_geo$uncertainty == "0"),]$uncertainty <- NA
post_geo$uncertainty <- as.numeric(post_geo$uncertainty)
  ## lat and long
sort(unique(post_geo$latitude))
sort(unique(post_geo$longitude))

# keep only edited columns
geolocated <- post_geo %>%
  rename(UID = UID_CONCAT) %>%
  dplyr::select(country,state,county,latitude,longitude,precision,
    uncertainty,gps_det,prov_type,UID) #multiple.results
head(as.data.frame(geolocated),n = 40)

## for Quercus, because geolocated before changed UID system
# bind itial export together, to join UID to geolocated data
#pre_geo <- Reduce(rbind.fill, sp_split)
#pre_geo <- pre_geo %>% dplyr::select(UID_CONCAT)
# add UID to geolocated rows and separate to accession level again
#geolocated <- cbind(post_geo,pre_geo)

# separate to accession level again
geolocated2 <- separate_rows(geolocated, UID, sep="\\|")
geolocated2 <- separate_rows(geolocated2, UID, sep="; ")
geolocated2 <- geolocated2 %>%
  rename(lat_dd = latitude, long_dd = longitude,
    coord_precision = uncertainty)

# see if all UIDs are matching up
yes_geo <- all_data13 %>% filter(UID %in% geolocated2$UID)
  # should be character(0)
setdiff(geolocated2$UID,yes_geo$UID)
# add gelocated rows to data
yes_geo <- yes_geo %>% dplyr::select(-prov_type,-gps_det,-lat_dd,
  -long_dd,-county,-state,-country,-coord_precision)
yes_geo <- full_join(yes_geo,geolocated2)
# bind all data together (geolocated and garden origin)
no_geo <- all_data13 %>% filter(!(UID %in% geolocated2$UID))
no_geo$precision <- NA
all_data15 <- rbind(yes_geo,no_geo)

# make gps_det "X" if NA -- not doing this now to distinquish records
#   that have been checked to see if can geolocate versus those that haven't
#all_data15[which(is.na(all_data15$gps_det)),]$gps_det <- "X"
unique(all_data15$gps_det)

# arrange columns
all_data15 <- all_data15 %>%
  arrange(species_name_acc,UID) %>%
  rename(gps_notes = flag) %>%
  dplyr::select(
    # key data
    UID,inst_short,submission_year,species_name_acc,target_species,
    prov_type,gps_det,lat_dd,long_dd,coord_precision,precision,gps_notes,
    all_locality,
    # locality
    locality,municipality,county,state,country,latlong_country,
    orig_lat,orig_long,
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
all_data15[which(all_data15$gps_notes == ""),]$gps_notes <- NA
str(all_data15)

# write file
write.csv(all_data15, file.path(main_dir,"Compiled ex situ data",
  "ALL DATA - POST GEOLOCATE",
  paste0(target_genus,"_POSTGEO_exsitu_compiled_standardized.csv")),
  row.names = F)

# create one CSV for each target species
all_data_target <- all_data15 %>% filter(target_species == "Y")
sp_split2 <- split(all_data_target,
  as.factor(all_data_target$species_name_acc))
names(sp_split2) <- gsub(" ","_",names(sp_split2))

# write files
if(!dir.exists(file.path(main_dir,"Compiled ex situ data",
  "ALL DATA - POST GEOLOCATE","target_species")))
  dir.create(file.path(main_dir,"Compiled ex situ data",
  "ALL DATA - POST GEOLOCATE","target_species"),recursive=T)
lapply(seq_along(sp_split2), function(i) write.csv(sp_split2[[i]],
  file.path(main_dir,"Compiled ex situ data","ALL DATA - POST GEOLOCATE",
  "target_species",
  paste0(names(sp_split2)[[i]], "_ALL_POSTGEO.csv")),row.names = F))
