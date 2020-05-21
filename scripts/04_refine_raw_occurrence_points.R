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

my.packages <- c('plyr', 'tidyverse')
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)
#library(housingData); library(data.table); library(textclean); library(CoordinateCleaner)

################################################################################
source('scripts/set_workingdirectory.R')
# setwd("./../..")
# setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")
# data_in <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points"
################################################################################
# A)
################################################################################

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
