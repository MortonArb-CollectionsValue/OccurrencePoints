################################################################################

## 3-1_geo_refinement.R
### Authors: Shannon M. Still & Emily Beckman ### Date: 08/10/2020

### DESCRIPTION:
  # This script is meant for massaging geographical data in both spatial
  #   polygon and tabular formats.

### DATA IN:
  # output from 3_compile_raw_occurrence_points.R
  # tabular data:
  # - target_taxa_with_syn.csv
  # - globaltreesearch_country_distribution.csv
  # - spatialpolygon data ...
  #

### DATA OUT:
  # XXXXX.RData
  # occurrence_point_count_per_species.csv

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("raster", "sp", "tools", "spatialEco", "rgdal", "geosphere",
  "readxl", "writexl", "dplyr", "tidyr", "tidyverse", "housingData", "maps",
  "data.table", "textclean", "CoordinateCleaner", "countrycode", "usmap")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

################################################################################
# Set working directory
################################################################################

# source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
source("scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################

source(file.path(script_dir,"0-2_load_IMLS_functions.R"))


################################################################################
################################################################################
# 1. Bring in data (load from saved .RData file).
#     d.rm: set the distance in meters to check from a point
#     (for comparing centroids to EOs)
#     proj4string4poly: set the projection to use for coordinates
################################################################################
  load(file.path(main_dir, "inputs", "gis_data", "IMLS_GIS_data.RData"))

  ## set the distance in meters to check from a point (for comparing centroids to EOs)
  d.rm <- 1000

  ## create proj4string to set coords
  proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    #######

    ## create new folder for revised points, if not already existing
  out.fld.nm <- "spp_edited_points"
  if(dir.exists(file.path(main_dir, "outputs", out.fld.nm))) print("directory already created") else
    dir.create(file.path(main_dir, "outputs", out.fld.nm), recursive=TRUE)

    #######

## this is start of iterative loop from the United States and other countries
      ## get list of files to iterate
    all.spp.files <- list.files(path=file.path(main_dir, "outputs", "working", "split_by_sp"),
        ignore.case=FALSE, full.names=FALSE, recursive=TRUE) #pattern=".thresh",
                # can use this "pattern=..." to find only files that have a specific pattern
    # all.spp.files <- all.spp.files[1:5]

      f.subs <- file_path_sans_ext(all.spp.files)
      gts_start <- gts_all[gts_all$taxon %in% gsub("_", " ", f.subs),]

  ## subset the species list to only those on GTS list within US, going to adm2 (county-level)
    gts_b <- gts_start
      to_geo_b <- unique(gsub(" ", "_", gts_b$taxon[gts_b$native_distribution %in% "United States"]))
    gts_c <- gts_start
      to_geo_c <- unique(gsub(" ", "_", gts_b$taxon[!gts_b$taxon %in% to_geo_b]))


nms0 <- c("iso2c", "iso3c", "NAME_0", "adm0_type", "ne_id",
          "long_centroid", "lat_centroid", "UID_adm0")
nms1 <- c("iso2c", "iso3c", "NAME_0", "NAME_1", "adm1_type", "adm1_code",
          "iso_3166_2",  "ne_id", "latitude", "longitude",
          "long_centroid", "lat_centroid", "UID_adm1")
nms2 <- c("iso3c", "NAME_0", "NAME_1", "NAME_2", "adm2_type",
          "long_centroid", "lat_centroid", "UID_adm2", "ID_0", "ID_1", "ID_2")

adm0.poly@data <- adm0.poly@data %>% rename(UID_adm0=UID)
adm1.poly@data <- adm1.poly@data %>% rename(UID_adm1=UID)
adm2.poly@data <- adm2.poly@data %>% rename(UID_adm2=UID)
################################################################################

rm(adm0, adm1, adm2)
# rm(adm0, adm0.spdf, adm1, adm1.spdf, adm2, adm2.spdf)
################################################################################
# set up for all taxa (3-2b)
  ## to county/adm2 level where approriate
  ## United States goes down to county centroids
################################################################################
to.adm <- unique(gsub(" ", "_", gts_start$taxon))
    # to.adm <- to.adm[1:5]

    cat("Starting ", "all ", "taxa (", length(to.adm), " total)", ".\n\n", sep="")
  for (i in 1:length(to.adm)){
    source(file.path(script_dir, "3-2b_geo_refinement.R"))
  }
################################################################################
# end United States (3-2b) iteration
################################################################################
# ################################################################################
# # set up for United States (3-2b) and then for World (3-2c)
#  ## to county/adm2 level where approriate
#   ## United States goes down to county centroids
# ################################################################################
# to.adm <- to_geo_b
#     # to.adm <- to.adm[1:5]
#
#     cat("Starting ", "United States ", "taxa (", length(to.adm), " total)", ".\n\n", sep="")
#   for (i in 1:length(to.adm)){
#     source(file.path(script_dir, "3-2b_geo_refinement.R"))
#   }
# ################################################################################
# # end United States (3-2b) iteration
# ################################################################################
#       # rm(i, out.fld.nm, f.nm, f.nms, f.subs, out.uid)
#       # rm(adm0, adm1, adm2, adm0.poly, adm1.poly, adm2.poly, d.mat)
#
# ################################################################################
# # set up for the World (3-2c)
# ################################################################################
# to.adm <- to_geo_c
#     # to.adm <- to.adm[1:5]
# # i <- 43 # (in US dataset, 43 is Quercus_lobata)
#     # i <- 1
#   for (i in 1:length(to.adm)){
#     source(file.path(script_dir, "3-2b_geo_refinement.R"))
#     # source(file.path(script_dir, "3-2c_geo_refinement.R"))
#   }
#
# ################################################################################
# # end world (3-2c) iteration
# ################################################################################
#
# ################################################################################
# #
# ################################################################################
#
#
#   # rm(i, out.fld.nm, f.nm, f.nms, f.subs, out.uid)
#   # rm(adm0, adm1, adm2, adm0.poly, adm1.poly, adm2.poly, d.mat)
#   #
# ################################################################################
# # end script
# ################################################################################
