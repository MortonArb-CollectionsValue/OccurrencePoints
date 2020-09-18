################################################################################

## 1-0_get_taxonomic_info.R
### Authors: Emily Beckman & Shannon Still ### Date: 5/30/2020

### DESCRIPTION:
  # This script takes a list of taxa and uses the taxize package to pull
  #   taxonomic information from multiple databases
  # Information pulled includes:
    # - Acceptance and authors from Tropicos, Integrated Taxonomic
    #   Information Service (ITIS), Kew’s Plants of the World (POW), and
    #   The Plant List (TPL)
    # - Synonyms from Tropicos, ITIS, and POW
  # The output can either be used directly in following scripts
  #   or can be reviewed and revised by hand (recommended)

### DATA IN:
  # target_taxa.csv (list of target taxa) or create list by hand in script
    # one column: "taxon_name_acc" (genus, species, infra rank, and infra name,
    # all separated by one space each; hybrid symbol should be " x ", rather
    # than "_" or "✕", and go between genus and species)

### DATA OUT:
  # target_taxa_with_syn.csv

################################################################################
# Load libraries
################################################################################

# rm(list=ls())
my.packages <- c('plyr', 'tidyverse', 'rgbif', 'data.table', 'taxize',
  'anchors', 'batchtools', 'textclean', 'stringi', "devtools")
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Gap Analysis/occurrence_points"
#script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
# source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
source('scripts/0-1_set_workingdirectory.R')

################################################################################
# Load functions
################################################################################
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))

# remove speices/taxa that did not have any synonyms, create data frame of
#   synonyms, and add column stating which database it came from
synonyms.compiled <- function(syn_output,db_name){
  found <- NA
  for(i in 1:length(syn_output)){
    if(length(syn_output[[i]])>1){
      if(syn_output[[i]][1,3]!="no syns found"){
        found <- c(found,i)
        syn_output[[i]]$taxon_name <- rep(names(syn_output[i]),
                                          nrow(syn_output[[i]]))
      }
    }
  }
  found <- found[-1]
  syn_output_df <- Reduce(rbind.fill, syn_output[found])
  syn_output_df$database <- db_name
  return(syn_output_df)
}

################################################################################
################################################################################
# 1. Load/create target taxa list
################################################################################

# CHANGE THIS LIST OF FAMILIES BASED ON TAXA YOURE LOOKING FOR:
#tpl_families() # list of families in database
families <- c("Fagaceae","Rosaceae","Ulmaceae","Malvaceae")
#families <- "Sapindaceae"
#families <- c("Juglandaceae","Fagaceae","Leguminosae","Lauraceae","Pinaceae",
#  "Taxaceae")

# read in taxa list
taxa_list_acc <- read.csv(file.path(main_dir,"inputs","taxa_list",
  "target_taxa.csv"), header = T, colClasses="character")
nrow(taxa_list_acc)
# make sure there aren't extra spaces within species names
taxa_list_acc[,1] <- str_squish(taxa_list_acc[,1])
# create list of target taxa names
taxa_names <- taxa_list_acc[,1]
  # use this instead if you want to select names based on values in other col:
  #taxa_names <- taxa_list_acc[which(taxa_list_acc$can_match == "match"),]
  #taxa_names <- taxa_names[,1]

## OR: you can create vector of taxa names here instead of reading in
#taxa_names <- c("name1","name2","name3")

# create list of target species names, with infraspecific taxa removed
species_names <- taxa_names[
  !grepl(" var. ",taxa_names) &
  !grepl(" subsp.",taxa_names) &
  !grepl(" f. ",taxa_names)]

# create list of target species names only, with hybrids removed
species_only <- species_names[
  !grepl(" x ",species_names)]

################################################################################
# 2. Find taxonomic status and synonyms for target taxa
################################################################################

###############
### A) Tropicos (from Missouri Botanical Garden)
### https://www.missouribotanicalgarden.org/media/fact-pages/tropicos.aspx
###############

# IF NEEDED: can save Tropicos API key as .txt and source from local drive
## check environment for the tropicos_key object
    ## "topicos_key.txt" should be a simple text file with only the key.
    ##    It should be stored in the local drive (local_dir) with file path set
    ##      in script 0-1_set_workingdirectory.R
if(file.exists(file.path(local_dir, "tropicos_key.txt"))){
  tpkey <- read_lines(file.path(local_dir, "tropicos_key.txt"))
  print("Good, you have your own dang Tropicos key!")
} else {print("Get your own dang Tropicos key!")}
  # or you can set API key in your R environment and restart R
    #taxize::use_tropicos() # get API
    #usethis::edit_r_environ() # set API
      # TROPICOS_KEY='________' # paste this in

# Tropicos does not search for infrataxa, so we will use species list

# replace characters to match Tropicos system
species_names <- gsub(" x "," × ",species_names,fixed=T)

## GET TAXONOMIC STATUS

tp_names_raw <- data.frame()
for(i in 1:length(species_names)){
  if(exists("tpkey")){
    output_new <- tp_search(species_names[[i]], key=tpkey)
  } else {
    output_new <- tp_search(species_names[[i]])
  }
  output_new$taxon_name_acc <- species_names[[i]]
  tp_names_raw <- rbind.fill(tp_names_raw,output_new)
  print(species_names[i])
}
head(tp_names_raw); class(tp_names_raw); names(tp_names_raw)
# standardize column names for joining later
tp_names <- tp_names_raw
setnames(tp_names,
  old = c("scientificname","nameid",
          "nomenclaturestatusname","scientificnamewithauthors"),
  new = c("taxon_name_match","match_id",
          "acceptance","match_name_with_authors"))
tp_names$database <- "tropicos"
# replace characters in taxa names
tp_names[] <- lapply(tp_names, function(x) gsub(" × "," x ", x))
tp_names[] <- lapply(tp_names, function(x) gsub(" fo. "," f. ", x))
# remove duplicates except those matching legitimate names
tp_names_noDup <- tp_names
  # remove rows with no match
tp_names_noDup <- tp_names_noDup[which(
  !is.na(tp_names_noDup$taxon_name_match)),]
  # OPTIONAL; IF NOT LOOKING FOR CHILDREN: remove subsp., var., and f.
tp_names_noDup <- tp_names_noDup %>%
  filter(!grepl("subsp.",taxon_name_match,fixed=T) &
         !grepl("var.",taxon_name_match,fixed=T) &
         !grepl("f.",taxon_name_match,fixed=T))
  # remove taxon_name_acc duplicates that aren't Legitimate
tp_names_noDup$dup <- c(duplicated(tp_names_noDup$taxon_name_acc,fromLast=T)
  | duplicated(tp_names_noDup$taxon_name_acc))
tp_names_noDup <- setdiff(tp_names_noDup,tp_names_noDup[which(
  tp_names_noDup$acceptance != "Legitimate" & tp_names_noDup$dup == T),])
# add column with authors
tp_names_noDup$match_name_with_authors <- paste(
  tp_names_noDup$taxon_name_match,tp_names_noDup$author)
# keep only necessary columns
tp_names_noDup <- tp_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
tp_names_noDup$acceptance <- str_to_lower(tp_names_noDup$acceptance)

## GET SYNONYMS

if(exists("tpkey")){
  tp_syn <- synonyms(species_names, db="tropicos", key=tpkey)
} else {
  tp_syn <- synonyms(species_names, db="tropicos")
}
rm(tpkey)

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove species/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
tp_syn_df <- synonyms.compiled(tp_syn,"tropicos")
colnames(tp_syn_df)
# standardize column names for joining later
setnames(tp_syn_df,
  old = c("taxon_name","nameid","scientificname",
          "scientificnamewithauthors"),
  new = c("taxon_name_acc","match_id","taxon_name_match",
          "match_name_with_authors"))
tp_syn_df$acceptance <- "synonym"
# replace characters in taxa names
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" × "," x ", x))
tp_syn_df[] <- lapply(tp_syn_df, function(x) gsub(" fo. "," f. ", x))
# keep only necessary columns
tp_syn_df <- tp_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
tp_syn_df$acceptance <- str_to_lower(tp_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

tp_all <- rbind.fill(tp_names_noDup,tp_syn_df)
head(tp_all)

###############
### B) Integrated Taxonomic Information Service (ITIS)
### https://www.itis.gov
###############

# replace characters to match ITIS system
taxa_names <- gsub(" x "," X ",taxa_names,fixed=T)
taxa_names <- gsub(" subsp. "," ssp. ",taxa_names)

## GET TAXONOMIC STATUS

# takes a while if lots of names
itis_names_raw <- itis_terms(taxa_names,what="scientific")
  itis_names_raw <- ldply(itis_names_raw, data.frame) # list to data frame
  itis_names_raw <- itis_names_raw[,c(1:2,4:6)]
head(itis_names_raw); class(itis_names_raw); names(itis_names_raw)
# standardize column names for joining later
itis_names <- itis_names_raw
setnames(itis_names,
  old = c(".id","scientificName","nameUsage","tsn"),
  new = c("taxon_name_acc","taxon_name_match","acceptance","match_id"))
itis_names$database <- "itis"
# replace characters in taxa names
itis_names[] <- lapply(itis_names, function(x) gsub(" X "," x ", x))
itis_names[] <- lapply(itis_names, function(x) gsub(" ssp. "," subsp. ", x))
# remove duplicates except those matching legitimate names
itis_names_noDup <- itis_names
  # remove rows with no match
itis_names_noDup <- itis_names_noDup[which(
  !is.na(itis_names_noDup$taxon_name_match)),]
  # OPTIONAL, IF NOT LOOKING FOR CHILDREN: remove subsp., var., and f.
itis_names_noDup <- itis_names_noDup %>%
  filter(!grepl("subsp.",taxon_name_match,fixed=T) &
         !grepl("var.",taxon_name_match,fixed=T) &
         !grepl("f.",taxon_name_match,fixed=T))
  # remove taxon_name_acc duplicates that aren't Legitimate
itis_names_noDup$dup <- c(duplicated(itis_names_noDup$taxon_name_acc,fromLast=T)
  | duplicated(itis_names_noDup$taxon_name_acc))
itis_names_noDup <- setdiff(itis_names_noDup,itis_names_noDup[which(
  itis_names_noDup$acceptance != "accepted" & itis_names_noDup$dup == T),])
# add column with authors
itis_names_noDup$match_name_with_authors <- paste(
  itis_names_noDup$taxon_name_match,itis_names_noDup$author)
# keep only necessary columns
itis_names_noDup <- itis_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
itis_names_noDup$acceptance <- str_to_lower(itis_names_noDup$acceptance)

## GET SYNONYMS

itis_syn <- synonyms(taxa_names, db="itis", accepted = F)

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# remove species/taxa that did not have any synonyms,
#   create data frame of synonyms,
#   and add column stating which database it came from
itis_syn_df <- synonyms.compiled(itis_syn,"itis")
colnames(itis_syn_df)
# standardize column names for joining later
setnames(itis_syn_df,
  old = c("taxon_name","syn_name","syn_tsn","syn_author"),
  new = c("taxon_name_acc","taxon_name_match","match_id","author"))
# keep only necessary columns
itis_syn_df <- itis_syn_df[,c("taxon_name_acc","taxon_name_match","author",
  "match_id","database")]
itis_syn_df$acceptance <- "synonym"
# replace characters in taxa names
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" X "," x ", x))
itis_syn_df[] <- lapply(itis_syn_df, function(x) gsub(" ssp. "," subsp. ", x))
# add column with authors
itis_syn_df$match_name_with_authors <- paste(
  itis_syn_df$taxon_name_match,itis_syn_df$author)
# remove records where taxa name and syn name are the same
itis_syn_df <- itis_syn_df[which(itis_syn_df$taxon_name_acc !=
  itis_syn_df$taxon_name_match),]
# keep only necessary columns
itis_syn_df <- itis_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","acceptance","match_name_with_authors","database")]
itis_syn_df$acceptance <- str_to_lower(itis_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

itis_all <- rbind.fill(itis_names_noDup,itis_syn_df)
head(itis_all)

###############
### C) Kew’s Plants of the World (POW)
### http://www.plantsoftheworldonline.org
###############

# replace characters to match POW system
taxa_names <- gsub(" X "," x ",taxa_names,fixed=T)
taxa_names <- gsub(" ssp. "," subsp. ",taxa_names)

## GET TAXONOMIC STATUS AND SYNONYMS

pow_names <- data.frame()
pow_syn <- data.frame()
for(i in 1:length(taxa_names)){
  id <- get_pow(taxa_names[[i]])[1]
  if(!is.na(id)){
    output_new <- pow_lookup(id)
      acc <- data.frame(
        "taxon_name_match" = output_new$meta$name,
        "match_id" = output_new$meta$fqId,
        "acceptance" = output_new$meta$taxonomicStatus,
        "author" = output_new$meta$authors,
        "taxon_name_acc" = taxa_names[[i]]
      )
    not_acc <- data.frame(output_new$meta$accepted)
    if(length(not_acc)>0){
      acc <- data.frame(
        "taxon_name_match" = output_new$meta$accepted$name,
        "match_id" = output_new$meta$accepted$fqId,
        "acceptance" = output_new$meta$taxonomicStatus,
        "author" = output_new$meta$accepted$author,
        "taxon_name_acc" = taxa_names[[i]]
      )
    }
    syn <- data.frame(output_new$meta$synonyms)
    if(length(syn)>0){
      syn$taxon_name_acc <- taxa_names[[i]]
    }
    pow_names <- rbind.fill(pow_names,acc)
    pow_syn <- rbind.fill(pow_syn,syn)
  }
}

# !! STOP BEFORE RUNNING NEXT SECTION -- YOU MAY HAVE TO ANSWER SOME PROMPTS

# fix up acceptance df
  # add column stating which database it came from
pow_names$database <- "pow"
  # add column with authors
pow_names$match_name_with_authors <- paste(
  pow_names$taxon_name_match,pow_names$author)
  # remove duplicates except those matching legitimate names
pow_names_noDup <- pow_names
  # keep only necessary columns
pow_names_noDup <- pow_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
pow_names_noDup$acceptance <- str_to_lower(pow_names_noDup$acceptance)
  # OPTIONAL, IF NOT LOOKING FOR CHILDREN: remove subsp., var., and f.
pow_names_noDup <- pow_names_noDup %>%
  filter(!grepl("subsp.",taxon_name_match,fixed=T) &
         !grepl("var.",taxon_name_match,fixed=T) &
         !grepl("f.",taxon_name_match,fixed=T))

# fix up synonyms df
  # add column stating which database it came from
pow_syn_df <- pow_syn
pow_syn_df$database <- "pow"
  # standardize column names for joining later
setnames(pow_syn_df,
  old = c("name","fqId","taxonomicStatus"),
  new = c("taxon_name_match","match_id","acceptance"))
  # add column with authors
pow_syn_df$match_name_with_authors <- paste(
  pow_syn_df$taxon_name_match,pow_syn_df$author)
  # remove records where taxa name and syn name are the same
pow_syn_df <- pow_syn_df[which(pow_syn_df$taxon_name_acc !=
  pow_syn_df$taxon_name_match),]
  # keep only necessary columns
pow_syn_df <- pow_syn_df[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
pow_syn_df$acceptance <- str_to_lower(pow_syn_df$acceptance)

## BIND TOGETHER STATUS AND SYNONYMS

pow_all <- rbind.fill(pow_names_noDup,pow_syn_df)
head(pow_all)

###############
### D) The Plant List (TPL)
### http://www.theplantlist.org
###############

# GET ALL DATA FOR TARGET FAMILIES
#   There is not an easy function for pulling synonyms from TPL :`(

tpl_names <- data.frame()
for(i in 1:length(families)){
  output_new <- tpl_get("files",family=families[i])
  output_new <- read.csv(paste("files/",families[i],".csv",sep=""), header = T,
    colClasses="character")
  tpl_names <- rbind.fill(tpl_names,output_new)
}
# standardize column names for joining later
setnames(tpl_names,
  old = c("ID","Taxonomic.status.in.TPL","Authorship"),
  new = c("match_id","acceptance","author"))
tpl_names$database <- "tpl"
# create concatenated taxon_name_acc col
tpl_names <- unite(tpl_names, "taxon_name_acc",
  c(Genus,Species.hybrid.marker,Species,Infraspecific.rank,
    Infraspecific.epithet), sep = " ", remove = F, na.rm = T)
# replace hybrid character
tpl_names$taxon_name_acc <- gsub(" × "," x ",
  tpl_names$taxon_name_acc,fixed=T)
# trim whitespace
tpl_names$taxon_name_acc <- str_squish(tpl_names$taxon_name_acc)
# fill other columns
tpl_names$taxon_name_match <- tpl_names$taxon_name_acc
tpl_names$match_name_with_authors <- paste(tpl_names$taxon_name_acc,
  tpl_names$author)
colnames(tpl_names)
# remove duplicates
tpl_names_noDup <- tpl_names
tpl_names_noDup$dup <- c(duplicated(tpl_names_noDup$taxon_name_acc,
  fromLast = TRUE) | duplicated(tpl_names_noDup$taxon_name_acc))
tpl_names_noDup <- setdiff(tpl_names_noDup,tpl_names_noDup[
  which(tpl_names_noDup$acceptance != "Accepted" & tpl_names_noDup$dup == T),])
# keep only necessary columns
tpl_names_noDup <- tpl_names_noDup[,c("taxon_name_acc","taxon_name_match",
  "match_id","database","acceptance","match_name_with_authors")]
tpl_names_noDup$acceptance <- str_to_lower(tpl_names_noDup$acceptance)
# join with taxa list and remove non-matches
tpl_all <- tpl_names_noDup %>% filter(tpl_names_noDup$taxon_name_acc %in%
  taxa_names)
head(tpl_all)

################################################################################
# 3. Bind all taxonomic status info and synonyms together
################################################################################

# create dataframe of all synonyms found
datasets <- list(tp_all,itis_all,pow_all,tpl_all)
all_data_raw <- Reduce(rbind.fill,datasets)
all_data <- all_data_raw
  names(all_data)
  str(all_data)

# add a space after every period and fix some other inconsistencies,
#  to standardize authors more
all_data$match_name_with_authors <- gsub(".",". ",
  all_data$match_name_with_authors,fixed=T)
all_data$match_name_with_authors <- str_squish(
  all_data$match_name_with_authors)
all_data$match_name_with_authors <- gsub(". )",".)",
  all_data$match_name_with_authors,fixed=T)
all_data$match_name_with_authors <- gsub("(pro sp.)","",
  all_data$match_name_with_authors,fixed=T)
# replace accented characters
all_data$match_name_with_authors <- stringi::stri_trans_general(
  all_data$match_name_with_authors, "Latin-ASCII")

# keep unique values and concatenate database, acceptance, id, authors
all_data <- all_data %>%
  dplyr::group_by(taxon_name_acc,taxon_name_match) %>%
  dplyr::summarize(
    database = paste(database, collapse = ','),
    acceptance = paste(acceptance,collapse = ','),
    ref_id = paste(match_id,collapse = ','),
    match_name_with_authors = paste(match_name_with_authors,collapse = '|')) %>%
  dplyr::ungroup()
# remove duplicates in database column
add <- setDT(all_data)[, list(database = toString(sort(unique(strsplit(database,
  ',')[[1]])))), by = ref_id]
all_data <- subset(all_data, select=-database)
all_data <- join(all_data,add)
unique(all_data$database)
unique(all_data$acceptance)
# add "database_count" column tallying number of items (databases) per taxon
all_data$database_count <- str_count(all_data$database, ',')+1
all_data[which(all_data$database == "NA"),]$database_count <- 0
str(all_data)
# remove duplicates in authorship column
add <- setDT(all_data)[, list(match_name_with_authors =
  toString(sort(unique(strsplit(match_name_with_authors,
    '\\|')[[1]])))), by = ref_id]
all_data <- subset(all_data, select=-match_name_with_authors)
all_data <- join(all_data,add)
head(all_data$match_name_with_authors)

# join with initial taxa list again
taxa_list_acc$taxon_name_match <- taxa_list_acc$taxon_name_acc
all_data <- full_join(all_data,taxa_list_acc)
all_data[which(is.na(all_data$database)),]$acceptance <- "no match"
# separate out taxon_name_match
all_data <- all_data %>% separate("taxon_name_match",
  c("genus","species","infra_rank","infra_name"),sep=" ",extra="warn",
  remove=F,fill="right")
all_data$genus_species_match <- paste(all_data$genus,all_data$species,sep=" ")
# separate out taxon_name_acc to create genus_species column
all_data <- all_data %>% separate("taxon_name_acc",
  c("genus_acc","species_acc"),sep=" ",extra="warn",remove=F,fill="right")
all_data$genus_species_acc <- paste(all_data$genus_acc,all_data$species_acc,
  sep=" ")
# add column stating if synonym or desiderata
all_data$list <- "synonym"
all_data[which(all_data$taxon_name_acc == all_data$taxon_name_match),]$list <-
  "desiderata"
# write file
#write.csv(all_data,file.path(main_dir,"inputs","taxa_list",
#  "target_taxa_with_syn_all.csv"),row.names=F)

# IF DESIRED:
  ## remove forms
nrow(all_data)
all_data <- all_data[which(is.na(all_data$infra_rank) |
  all_data$infra_rank != "f."),]
nrow(all_data)
  ## remove synonyms with less than two sources
all_data <- all_data[which(all_data$database_count > 1 |
  grepl("homotypic",all_data$acceptance) |
  grepl("accepted",all_data$acceptance)),]
nrow(all_data)
  ## remove records where same syn match name matches more than 1 taxon_name_acc
all_data$dup <- c(duplicated(all_data$taxon_name_match,fromLast=T)
  | duplicated(all_data$taxon_name_match))
all_data <- setdiff(all_data,all_data[which(
  all_data$list == "synonym" & all_data$dup == T),])
nrow(all_data)
  ## remove var. and subsp. synonyms when species is already represented
all_data <- setdiff(all_data,all_data[which(
  all_data$genus_species_acc == all_data$genus_species_match &
  grepl("\\.",all_data$taxon_name_match)),])
nrow(all_data)

# final ordering of names and column selection
all_data <- all_data %>%
  dplyr::arrange(taxon_name_acc) %>%
  dplyr::select(taxon_name_acc,taxon_name_match,genus_species_acc,genus,species,
    infra_rank,infra_name,list,database,acceptance,database_count,
    match_name_with_authors)
setnames(all_data,
  old = c("taxon_name_match","genus_species_acc"),
  new = c("taxon_name","species_name_acc"))
# write file
write.csv(all_data,file.path(main_dir,"inputs","taxa_list",
  "target_taxa_with_syn.csv"),row.names=F)
