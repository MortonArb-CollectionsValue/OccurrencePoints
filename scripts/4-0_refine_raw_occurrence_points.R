################################################################################
## THIS SCRIPT MAY NOT BE NECESSARY. MUCH WAS DISTRIBUTED TO OTHER SCRIPTS. 
## MOST OF WHAT IS LEFT IS ACTUALLY IN OTHER SCRIPTS AS WELL.

################################################################################
##  4-0_refine_raw_occurrence_points.R
################################################################################

### Author: Shannon M. Still & Emily Beckman  ### Date: 08/04/2020

### DESCRIPTION:
# This script is meant for massaging geographical data in both spatial
#   polygon and tabular formats

### INPUT:
# output from 3_compile_raw_occurrence_points.R
# tabular data:
#   target_taxa_with_syn.csv
#   globaltreesearch_country_distribution.csv
# spatialpolygon data ...

### OUTPUTS:
# XXXXX.RData
# occurrence_point_count_per_sp.csv

################################################################################
# Load packages
################################################################################

rm(list=ls())

my.packages <- c('raster', 'sp', 'tools', 'spatialEco', 'rgdal', 'geosphere', 'readxl', 'writexl', 'dplyr', 'tidyr', 'tidyverse', 'housingData', 'data.table', 'textclean', 'CoordinateCleaner', 'countrycode', 'usmap')
#'sf',
# install.packages (my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
rm(my.packages)
# my.packages <- c('countrycode', 'usmap')
# install.packages (my.packages) #Turn on to install current versions
# lapply(my.packages, require, character.only=TRUE)
#     rm(my.packages)

################################################################################
# Set your working directory and project folders based on computer
#    Either run script or set manually
################################################################################

source('scripts/0-1_set_workingdirectory.R')

################################################################################
# Load functions
################################################################################

source('scripts/0-2_load_IMLS_functions.R')

################################################################################
## Read in/load data
################################################################################
load(file.path(main_dir, "outputs", "working", "IMLS_data_workng.RData"))

# 
# # taxon table
# taxon_list <- read.csv(file.path(main_dir, "inputs", "taxa_list", "target_taxa_with_syn.csv"),
#                        header = T, na.strings = c("","NA"), colClasses = "character")
# 
# # GlobalTreeSearch country distribution file
# gts_list <- read.csv(file.path(main_dir, "inputs", "known_distribution",
#                                "globaltreesearch_country_distribution.csv"),
#                      header = T, na.strings = c("","NA"), colClasses = "character")
# 
# ################################################################################
# # Split Global Tree Search countries by their delimiter (;) and create
# #   separated country list for each taxon
# ################################################################################
# 
# gts_all <- gts_list %>%
#   mutate(native_distribution=strsplit(as.character(native_distribution),"; ")) %>%
#   unnest(native_distribution)
# 
# 
################################################################################
# # TO DO:

## Maybe this early part of this script could go to 01_get_taxonomic_info.R
# #Create tables for walkover
# #   - Make taxon name table for cross-referencing synonyms, including country/state/county of origin
# #   - Assign country codes to species in species table (ISO code)
#

## Maybe the rest (after taxonomic part at beginning) could go to 03_compile_raw_occurrence_points.R or stay as 3.5?
# # Assign coordinates to records that are missing coordinates
# ## based on country/state/county
#
# #Flag occurrences where stated country/state/county don't match those of coordinates provided
# #Flag occurrences where coordinates provided are within a specific distance of the centroid of county/state/country
#     #these coordinates are likely automatically assigned
# #If more than one record for a given taxon is within a specific area (certain distance apart or within same grid cell)
#     # based on distance to centroids
#     # distance to other coordinates
#     # within same grid cell as another coordinate of same taxon
#     #choose a record as authoritative
#     #coordinate uncertainty - (smallest coordinate uncertainty the best?)
#     #data source of record
#     #FIA > voucher from specific collecteors > research collection > CCH > GBIF
#     #type of record (herbarium voucher versus ...)
#     #year recorded (newer likely more accurate)
#     #issue indicated
#     ##basis of record
#
# #Flag occurrences outside their native distribution
#
# #   - Coordinate uncertainty in meters
# #(remove record if coordinate uncertainty is larger than specified acceptable uncertainty)
#
#


# some functions from the CoordinateCleaner package we could try...

# flag records assigned to the location of zoos, botanical gardens,
# herbaria, universities and museums, based on a global database of ~10,000
# such biodiversity institutions
coord_test <- cc_inst(x, lon = "decimalLongitude", lat = "decimalLatitude",
                      species = "species_name_acc", buffer = 100, geod = TRUE, ref = NULL,
                      verify = FALSE, verify_mltpl = 10, value = "flagged", verbose = TRUE)
geo_pts[!coord_test,1:24]

# flag records that are outliers in geographic space according to the method
# defined via the method argument
coord_test <- cc_outl(x, lon = "decimalLongitude", lat = "decimalLatitude",
                      species = "species_name_acc", method = "quantile", mltpl = 5, tdi = 1000,
                      value = "flagged", sampling_thresh = 0, verbose = TRUE,
                      min_occs = 7, thinning = FALSE, thinning_res = 0.5)

# flags records from inside urban areas, based on a geographic gazetteer
coord_test <- cc_urb(x, lon = "decimalLongitude", lat = "decimalLatitude",
                     ref = NULL, value = "flagged", verbose = TRUE)


################################################################################
# X) final step to write files out
################################################################################

## write out final clean files into new folder
## following code is meant to be used in iterative process
## data_in = main data folder
## out.fld.nm = output folder for cleaned point data
## eo.spdf@data = data.frame/data containing points.
## we also could store data in spatialpoint/polygon data files

write.csv(eo.spdf@data, file.path(data_in, out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)
# writeOGR(eo.spdf, file.path(data_in, out.fld.nm, paste0(f.nm, ".csv")))
