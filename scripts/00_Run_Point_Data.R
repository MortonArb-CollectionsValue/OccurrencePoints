# STILL WORKING -- NOT READY YET

################################################################################

### Authors: Emily Beckman & Shannon Still ### Date: 05/21/2020

### DESCRIPTION:
  # This is the file to run all of the scripts for the point data aggregation
  #   and cleaning. This also is an outline for what is happening and why.

##############################
##############################
## Source the working directory file to set up installation/run
###############
#setwd("/Users/emily/Documents/GitHub/IMLS_CollectionsValue")
source('scripts/set_workingdirectory.R')

##############################
##############################
## 01_get_taxonomic_info.R ##
###############
## Run the script to set get the raw data for all taxa
source('scripts/1_get_taxonomic_info.R')

##############################
##############################
## 02_get_raw_occurrence_points.R ##
###############
## Run the retrieve raw EOs and then sort, clean, and filter
source('scripts/2_get_raw_occurrence_points.R')

# ##############################
# ##############################
# ## 03_compile_raw_occurrence_points.R ##
# ###############
# ## Run the retrieve raw EOs and then sort, clean, and filter
# source('scripts/3_compile_raw_occurrence_points.R')
#
# ##############################
# ##############################
# ## 03_compile_raw_occurrence_points.R ##
# ###############
# ## Run the retrieve raw EOs and then sort, clean, and filter
# source('scripts/03_compile_raw_occurrence_points.R')
#
# ##############################
# ##############################
# ## 03_compile_raw_occurrence_points.R ##
# ###############
# ## Run the retrieve raw EOs and then sort, clean, and filter
# source('scripts/03_compile_raw_occurrence_points.R')
#
# ##############################
# ##############################
# ## 03_compile_raw_occurrence_points.R ##
# ###############
# ## Run the retrieve raw EOs and then sort, clean, and filter
# source('scripts/03_compile_raw_occurrence_points.R')
#
#
#
