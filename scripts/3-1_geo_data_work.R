################################################################################

## 3.1_geo_data_work.R
### Authors: Shannon M. Still & Emily Beckman ### Date: 08/04/2020

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

# either set manually:
#main_dir <- "./Desktop"
#script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
# source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
source("scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################
source(file.path(script_dir,"0-2_load_IMLS_functions.R"))


################################################################################
# 1. 
################################################################################

  load(file.path(main_dir, "inputs", "gis_data", "IMLS_GIS_data.RData"))
    
  ## set the distance in meters to check from a point (for comparing centroids to EOs)
  d.rm <- 1000

  ## create proj4string to set coords
      proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

    #######
## this is start of iterative loop from the United States and other countries
      ## get list of files to iterate
    all.spp.files <- list.files(path=file.path(main_dir, "outputs", "working", "split_by_sp"), 
        ignore.case=FALSE, full.names=FALSE, recursive=TRUE) #pattern=".thresh", 
                # can use this "pattern=..." to find only files that have a specific pattern
    # all.spp.files <- all.spp.files[1:5]
    ## subset the species list to only those on GTS list within US

      f.subs <- file_path_sans_ext(all.spp.files)

      gts_sub <- gts_all[gts_all$taxon %in% gsub("_", " ", f.subs),]
      # f.nms <- gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution == "United States"])
      # gts_sub <- gts_sub[!is.na(gts_sub),]
    ## subset the species list to only those on GTS list within US, going to adm2 (county-level)
        to.adm2 <- unique(gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution == "United States"]))
        to.adm2 <- to.adm2[!is.na(to.adm2)]
    ## subset the species list to only those on GTS list to centroid at state level (adm1)
        to.adm1 <- gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution != "United States"])
        to.adm1 <- to.adm1[!is.na(to.adm1)]
    ## subset the species list to only those on GTS list outside US, going to adm0 (country-level)
        to.adm0 <- unique(gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution != "United States"]))
        to.adm0 <- to.adm0[!is.na(to.adm0)]
        # to.adm0 <- unique(gsub(" ", "_", gts_sub$taxon[!gts_sub$taxon %in% gsub("_", " ", to.adm2)]))

  # if(gts_sub$native_distribution == "United States"){

    ## create new folder for revised points, if not already existing
    out.fld.nm <- "spp_edited_points"
    if(dir.exists(file.path(main_dir, "outputs", out.fld.nm))) print("directory already created") else
      dir.create(file.path(main_dir, "outputs", out.fld.nm), recursive=TRUE)

    ## to county/adm2 level
    ## United States goes down to county centroids
    cat("Starting ", "United States ", "taxa (", length(to.adm2), " total)", ".\n\n", sep="")
i <- 43
      # for (i in 1:length(to.adm2)){
      #   f.nms <- to.adm2
      #   f.nm <- f.nms[i]
      #   cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
      # }

## let us iterate!
nms0 <- c("iso3c", "iso2c", "NAME_0", "iso3n", "ne_id", "long_centroid", "lat_centroid")
nms1 <- c("adm1_code", "iso_3166_2", "iso2c", "adm1_type", "latitude", "longitude", "iso3c", "NAME_0", "NAME_1", "ne_id", "long_centroid", "lat_centroid")
    
# for (i in 1:length(to.adm2)){
      f.nms <- to.adm2
      f.nm <- f.nms[i]
        cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")

  ## bring in records (load from *.RData file or bring in from text file)
  eo.df  <- read.csv(file.path(main_dir, "outputs", "working", "split_by_sp", paste0(f.nm, ".csv")))
      if(nrow(eo.df) < 2) next

    ## extract administrative area (0,1,2) based upon inforamtion provided in record for country/state/county.
    proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    eo.spdf <- SpatialPointsDataFrame(eo.df[,c("decimalLongitude", "decimalLatitude")], eo.df,
                                      proj4string = CRS(proj4string4poly))

    ##set a variable to flag occurrences that are not in right place
    eo.spdf$occ_flag <- NA
      eo.spdf$occ_flag <- as.character(eo.spdf$occ_flag)

    ## flag records where point is outside polygons of land
      eo.spdf$in_water <- NA
        eo.spdf$in_water <- as.character(eo.spdf$in_water)
      eo.spdf$in_water <- is.na(map.where(adm0.poly, eo.spdf$decimalLongitude,
                                        eo.spdf$decimalLatitude))

      out.uid <- as.character(eo.spdf$UID[eo.spdf$in_water == TRUE])

      # plot(adm0.poly[adm0.poly$name == "United States of America",])
      # points(eo.spdf[eo.spdf$in_water == TRUE,], col="blue")
      # points(eo.spdf[eo.spdf$in_lakes == TRUE,], col="blue")

      if(length(out.uid) > 0){
        eo.spdf$occ_flag[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates are in water")
      }

    # ## flag records where point is in a lake
    #   eo.spdf$in_lakes <- NA
    #     eo.spdf$in_lakes <- as.character(eo.spdf$in_lakes)
    #     wrld.lakes <- map("lakes")
    #
    #   eo.spdf$in_lakes <- is.na(map.where(wrld.lakes, eo.spdf$decimalLongitude,
    #                                       eo.spdf$decimalLatitude))
    #
    #   out.uid <- as.character(eo.spdf$UID[eo.spdf$in_lakes == FALSE])
    #
    #   if(length(out.uid) > 0){
    #   eo.spdf$occ_flag[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates are in a lake.")
    #   }
      eo.spdf@data <- select(eo.spdf@data, -in_water)#, -in_lakes
        head(eo.spdf@data)

    if(length(eo.spdf[!is.na(eo.spdf$occ_flag),]) > 0){
      out.fld.nm <- "spp_edited_points"
        if(dir.exists(file.path(main_dir, "outputs", "working", "records_to_examine"))) print("directory already created") else
          dir.create(file.path(main_dir, "outputs", "working", "records_to_examine"), recursive=TRUE)
        if(file.exists(file.path(main_dir, "outputs", "working", "records_to_examine", "water_points_", Sys.Date(), ".csv"))){
            write.csv(eo.spdf[!is.na(eo.spdf$occ_flag),], file.path(main_dir, "outputs", "working", "records_to_examine", 
                paste0("water_points_", Sys.Date(), ".csv")),
                  row.names = F, append = TRUE)
      } else write.csv(eo.spdf[!is.na(eo.spdf$occ_flag),], file.path(main_dir, "outputs", "working", "records_to_examine", 
                paste0("water_points_", Sys.Date(), ".csv")),
                  row.names = F)
    }

      # eo.spdf$dist_out <- NA
      #   eo.spdf$dist_out <- as.character(eo.spdf$dist_out)
      #
      # out.water <- eo.spdf[!is.na(eo.spdf$occ_flag),]
      # # out.water <- out.water[1:10,]
      #     plot(out.water)
      # dist.water <- dist2Line(out.water, adm0.poly, distfun=distGeo)
      # eo.spdf$dist.water[!is.na(eo.spdf$occ_flag)] <- dist.water

      # ##or make buffer around outside and then take points within
      #
      #   out.water[!is.na(out.water$occ_flag),] <-
      #   #<- dist.water[,1]
    ## get points that are in each polygon
    eo.0 <- point.in.poly(eo.spdf, adm0.poly, sp=TRUE)
    eo.1 <- point.in.poly(eo.spdf, adm1.poly, sp=TRUE)
    eo.2 <- point.in.poly(eo.spdf, adm2.poly, sp=TRUE)

    ## compare the calculated point to the provided admin area and flag those that do not match
        ##assign the native country (ISO0), state (ISO1), and county (ISO2) to the SPDF or dataset

    gts_sub <- gts_all[gts_all$taxon %in% eo.2@data$species_name_acc,]
    # eo.2@data <- eo.2@data %>% select(-FIPS)
    eo.post <- left_join(eo.2@data, gts_list[,c(2,4)], by=c("species_name_acc" = "taxon")) %>% select(-long_centroid, -lat_centroid)
      # eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% strsplit(native_distribution, split = "; "), TRUE, FALSE)))
    eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% gts_sub$native_distribution, TRUE, FALSE)))
      out.uid <- as.character(eo.spdf$UID[eo.spdf$ISO1_match == TRUE])

  ## flag records where country doesn"t match and then output into folder (as *.csv)
    eo.post$occ_flag[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates not in the same country (adm0) provided in dataset.")
    eo.out <- select(eo.post, -ISO1_match)#, -in_water, -in_lakes

    # ## Could flag records where state (adm1) or county (adm2) do not match using similar code to above.
    # eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_1 %in% gts_sub$native_distribution, TRUE, FALSE)))

  write.csv(eo.out, file.path(main_dir, "outputs", out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)


  eo.spdf <- SpatialPointsDataFrame(eo.out[,c("decimalLongitude", "decimalLatitude")], eo.out,
                                    proj4string = CRS(proj4string4poly))
  
  ## make a distance matrix
    d.mat <- distm(eo.spdf, adm2.spdf)
      row.names(d.mat) <- eo.spdf$UID
      colnames(d.mat) <- adm2.spdf$UID

## if some values are within, then find which are within distance and flag them
        ## could set another field to flag occurrences that are outtside their bounds
            # eo.spdf$occ_flag <- ""

  ## check for any that fit a specific requirement (distance away)
      if(isTRUE(any(d.mat[d.mat < d.rm]))){

            ## set the values greater than the distance to NA
            cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
                d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
                d.mat[d.mat > d.rm] <- NA
                  df <- data.frame(d.mat)

                    # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
                    eo.spdf$occ_flag[eo.spdf$UID %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0("Check record for proximity to county/adm2 centroid (within ", d.rm, " meters).")

      } else {
      cat("No points are within ", d.rm, " meters of a centroid for any administrative area.\n\n", sep="")
      }

      write.csv(eo.spdf@data, file.path(main_dir, "outputs", out.fld.nm, 
            paste0(f.nm, ".csv")), row.names=FALSE)

      cat("Ending ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
      
    # }
  # save(gts_all, taxon_list, adm0.spdf, adm1.spdf, adm2.spdf, proj4string4poly, file="IMLS_data_workng.RData")
  save(gts_all, taxon_list, adm0.spdf, adm1.spdf, adm2.spdf, proj4string4poly, file=file.path(main_dir, "outputs",
    "working", "IMLS_data_workng_US.RData"))

  rm(i, out.fld.nm, f.nm, f.nms, f.subs, out.uid)
  rm(adm0, adm1, adm2, adm0.poly, adm1.poly, adm2.poly, d.mat)


# ## reorder columns
# h.nms <- c("UID", "species_name_acc", "taxon_name", "scientificName",
#   "taxonIdentificationNotes", "database", "year", "basisOfRecord", "establishmentMeans",
#   "decimalLatitude", "decimalLongitude", "coordinateUncertaintyInMeters",
#   "geolocationNotes", "localityDescription", "county", "stateProvince", "country", "countryCode",
#   "locationNotes", "datasetName", "publisher", "nativeDatabaseID", "references",
#   "informationWithheld", "issue", "taxon_name_full", "list", "coords_error", "occ_flag")
#   
################################################################################
################################################################################
## for records that are flagged, what is the distance away from their actual listed location?
    ## if in water, how far away from land
    ## if not in country, where are they located and how far is that from the correct admin area?

# unique(eo.spdf$occ_flag)

################################################################################
# # # # ###   NEEDS TO BE COMPLETED   ###
# # # #   ## Assign coordinates to records that are missing coordinates
# # # #     ## based on country/state/county
# # # #   load(file.path(imls.meta, "gis_data", "IMLS_GIS_data.RData"))
# # # #
# # # #   ## bring in records (at least those missing coordinates)
################################################################################
    # # # #
# ################################################################################
# ###   NEEDS TO BE COMPLETED   ###
#   ## Flag occurrences outside their native distribution
#       ## based on country/state/county

  # spp.test <- c("Quercus georgiana", "Quercus imbricaria", "Quercus arkansana", "Quercus falcata", "Quercus stellata", "Quercus acutissima")



# ################################################################################
#
#   cat("Starting ", "rest of world ", "taxa (", length(to.adm0), " total)", ".\n\n", sep="")
#   # i <- 14
#       for (i in 1:length(to.adm0)){
#         f.nms <- to.adm0
#         f.nm <- f.nms[i]
#             cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
#
#       ## bring in records (load from *.RData file or bring in from text file)
#       eo.df  <- read.csv(file.path(main_dir, "outputs", "working", "split_by_sp", paste0(f.nm, ".csv")))
#         if(nrow(eo.df)<2) next
#       # eo.df  <- eo.df %>% select(-FIPS)
#       ## extract administrative area (0,1,2) based upon inforamtion provided in record for country/state/county.
#       proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#       eo.spdf <- SpatialPointsDataFrame(eo.df[,c("decimalLongitude", "decimalLatitude")], eo.df,
#                                         proj4string = CRS(proj4string4poly))
#       eo.0 <- point.in.poly(eo.spdf, adm0.poly, sp=TRUE)
#       ## compare the calculated area to the provided area and flag those that do not match
#       ##assign the native country (ISO0), state (ISO1), and county (ISO2) to the SPDF or dataset
#
#       gts_sub <- gts_all[gts_all$taxon %in% eo.0@data$taxon_name,]
#
#       eo.post <- left_join(eo.0@data, gts_list[,c(2,4)], by=c("species_name_acc" = "taxon"))
#
#       eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(name %in% gts_sub$native_distribution, TRUE, FALSE)))
#       # eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% strsplit(native_distribution, split = "; "), TRUE, FALSE)))
#
#       # write_xlsx(eo.post, path=file.path(main_dir, "outputs", "working", "match_work.xlsx"))
#
#       ## flag records where country doesn"t match and then output into folder (as *.csv)
#       eo.out <- eo.post %>% filter(ISO1_match==TRUE) %>% select(names(eo.df))
#
#       ## remove records where country doesn"t match and then output into folder (as *.csv)
#       # eo.out <- eo.post %>% filter(ISO1_match==TRUE) %>% select(names(eo.df))
#
#       write.csv(eo.out, file.path(main_dir, "outputs", out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)
#
#
#       eo.spdf <- SpatialPointsDataFrame(eo.out[,c("decimalLongitude", "decimalLatitude")], eo.out,
#                                         proj4string = CRS(proj4string4poly))
#
#       ## make a distance matrix
#       d.mat <- distm(eo.spdf, adm2.spdf)
#       row.names(d.mat) <- eo.spdf$UID
#       colnames(d.mat) <- adm2.spdf$UID
#       ## if some values are within, then find which are within distance and flag them
#       ## set field for flag
#       eo.spdf$occ_flag <- ""
#
#       ## check for any that fit a specific requirement (distance away)
#       if(isTRUE(any(d.mat[d.mat < d.rm]))){
#
#         ## set the values greater than the distance to NA
#         cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
#         d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
#         d.mat[d.mat > d.rm] <- NA
#         df <- data.frame(d.mat)
#
#         # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
#         eo.spdf$occ_flag[eo.spdf$UID %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0("Check record for proximity to county/adm2 centroid (within ", d.rm, " meters).")
#
#       } else {
#         cat("No points are within ", d.rm, " meters of a centroid for any administrative area.\n\n", sep="")
#       }
#
#       write.csv(eo.spdf@data, file.path(main_dir, "outputs", out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)
#
#       cat("Ending ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
#
#       }

################################################################################
################################################################################
#   ## STILL WORKING ON THIS SECTION ## DON"T KNOW THAT THIS IS NECESSARY. WE COULD LOOK FOR DUPLICATES WITHIN EACH RASTER CELL
# ## calculate distance between points of the dataset (entered as spatialPolygon*)
#   ## distance between points of same taxon (use raster package pointDistance for calculation)
#   eo.spdf2 <- eo.spdf[1:100,]
#     plot(eo.spdf2)
#   ## d.rm is distance (meters) from another point where there are too close together and you want to remove
#   d.rm <- 10000
#
#   # spdf <- eo.spdf2
#   d.mat <- distm(eo.spdf2)
#   # d.mat <- pointDistance(eo.spdf2, eo.spdf2, lonlat=TRUE, allpairs=TRUE)
#   row.names(d.mat) <- eo.spdf2$UID
#   colnames(d.mat) <- eo.spdf2$UID
#   d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
#   d.mat[d.mat > d.rm] <- NA
#   # d.mat[d.mat < d.rm] <- TRUE
#   d.mat <- as.data.frame(d.mat)
#
#
#
#   # # names(d.mat) <- eo.spdf2$UID
#   # dst <- as.dist(d.mat)
#   # # d.mat[d.mat > d.rm] <- NA
#   # # d.mat[d.mat > d.rm] <- NA
#   # # d.mat
#   # dst[dst > d.rm] <- NA
#   # dst
#   #
#   which(isTRUE(d.mat) | isTRUE(d.mat[nrow(d.mat):1, ])[nrow(d.mat):1])
#   adj1[vec1,vec1]
#
#   vec1 <- colnames(d.mat)
#   d.mat[vec1,vec1]
#
#
#   df <- data.frame(a = c(1,2,3,4,1,5,6,4,2,1))
#   which(duplicated(df) | duplicated(df, fromLast = TRUE))
#
#   # i <- 2
#   # ## for each row, create new field for those other points within defined distance (d.rm)
#   # n <- nrow(d.mat)
#   #
#   # for(i in 1:n){
#   #   if(d.mat[,i])
#   #     d.mat[,i] <= d.rm
#   #
#   # }
#   # class(dst)
################################################################################

# # TO DO:
#

## Maybe this early part of this script could go to 01_get_taxonomic_info.R
# #Create tables for walkover
# #   - Make taxon name table for cross-referencing synonyms, including country/state/county of origin
# #   - Assign country codes to species in species table (ISO code)
#

## Maybe the rest (after taxonomic part at beginning) could go to 03_compile_raw_occurrence_points.R or stay as 3.5?
# # Assign coordinates to records that are missing coordinates
# ## based on country/state/county
#
# #Flag occurrences where stated country/state/county don"t match those of coordinates provided
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
#
# #Flag occurrences outside their native distribution
#
# #   - Coordinate uncertainty in meters
# #(remove record if coordinate uncertainty is larger than specified acceptable uncertainty)
#