################################################################################

## 0-2_laod_IMLS_functions.R
### Authors: Shannon Still & Emily Beckman ### Date: 05/21/2020

### DESCRIPTION:
# This script loads all of the functions to use for the project

################################################################################
# Load functions
################################################################################

################################################################################
################################################################################
## fxn: synonyms.compiled
# remove speices/taxa that did not have any synonyms (they create errors
#   in next step), create data frame of synonyms, and add column stating
#   which database it came from
################################################################################
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
## fxn: remove.empty.col
# searches for data frame columns with only NAs and removes them
################################################################################
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

################################################################################
################################################################################
## fxn: percent.filled
# calculates percent of each data frame column that is not NA
################################################################################
percent.filled <- function(df){
  for(i in 1:ncol(df)){
    print(paste(names(df)[i],": ",
                round((nrow(df)-sum(is.na(df[,i])))/nrow(df),3)*100,"%",sep=""))
  }
}

################################################################################
################################################################################
## fxn: children.compiled
# # remove speices/taxa that did not have any children (they create errors
# in next step), create data frame of children, and add column stating
# which database it came from
################################################################################
children.compiled <- function(child_output,db_name,greater_than){
  found <- NA
  for(i in 1:length(child_output)){
    if(length(child_output[[i]])>greater_than){
      found <- c(found,i)
      child_output[[i]]$taxon_name_acc <- rep(names(child_output[i]),
                                              nrow(child_output[[i]]))
    }
  }
  found <- found[-1]
  child_output_df <- Reduce(rbind.fill, child_output[found])
  child_output_df$database <- db_name
  return(child_output_df)
}


################################################################################
################################################################################
## fxn: XXXXX
# XXXXX
################################################################################
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
    data <- rbind(data, target_sp)
  }

  # remove state file to make space for reading in next one
  rm(state_df)
  # take a look at how much data were pulled
  print(paste(nrow(data), file_name))
  return(data)
  rm(sp)
}

################################################################################
################################################################################
## fxn: XXXXX
# XXXXX
################################################################################
