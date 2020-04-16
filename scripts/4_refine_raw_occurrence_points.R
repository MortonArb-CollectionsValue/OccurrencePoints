### Author:               ###  Date: 04/16/2020                                |

### DESCRIPTION:
  # This script

### INPUT:
  #

### OUTPUTS:
  #

#################
### LIBRARIES ###
#################

library(plyr)
library(tidyverse) #ggplot2,dplyr,tidyr,readr,purrr,tibble,stringr,forcats
#library(housingData)
#library(data.table)
#library(textclean)
#library(CoordinateCleaner)


################################################################################
# A)
################################################################################

setwd("./../..")
setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")

# TO DO:
#   - Crop lat/lat by "accepted" distribution for each species
#   - Coordinate uncertainty in meters (remove point if larger than specific
#     value?)
#   -


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
