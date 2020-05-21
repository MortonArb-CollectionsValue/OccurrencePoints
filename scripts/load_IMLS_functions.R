# load_IMLS_functions.R

## This script is to load all of the functions to use for the project ##
##    Call this script from the main script that runs all scripts, or at least at beginning of a specific script

################
### FUNCTIONS ###
#################


####################################################################################
####################################################################################
## fxn: synonyms.compiled
    # remove speices/taxa that did not have any synonyms (they create errors
    #   in next step), create data frame of synonyms, and add column stating
    #   which database it came from
####################################################################################
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

####################################################################################
####################################################################################
## fxn: remove.empty.col
# searches for data frame columns with only NAs and removes them
####################################################################################
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

####################################################################################
####################################################################################
## fxn: percent.filled
# calculates percent of each data frame column that is not NA
####################################################################################
percent.filled <- function(df){
  for(i in 1:ncol(df)){
    print(paste(names(df)[i],": ",
                round((nrow(df)-sum(is.na(df[,i])))/nrow(df),3)*100,"%",sep=""))
  }
}

####################################################################################
####################################################################################
## fxn: XXXXX
# XXXXX
####################################################################################

