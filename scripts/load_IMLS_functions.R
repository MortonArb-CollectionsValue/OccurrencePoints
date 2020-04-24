# load_IMLS_functions.R

## This script is to load all of the functions to use for the project ##
##    Call this script from the main script that runs all scripts, or at least at beginning of a specific script

################
### FUNCTIONS ###
#################


####################################################################################
#######################################
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


