################################################################################

## 3.1_geo_data_work.R
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
## this is start of iterative loop from the United States and other countries
      ## get list of files to iterate
    all.spp.files <- list.files(path=file.path(main_dir, "outputs", "working", "split_by_sp"), 
        ignore.case=FALSE, full.names=FALSE, recursive=TRUE) #pattern=".thresh", 
                # can use this "pattern=..." to find only files that have a specific pattern
    # all.spp.files <- all.spp.files[1:5]
  
      f.subs <- file_path_sans_ext(all.spp.files)
      gts_sub <- gts_all[gts_all$taxon %in% gsub("_", " ", f.subs),]
      
      gts_sub <- gts_sub[1:5,]
      
if(gts_sub$native_distribution == "United States"){

  ## subset the species list to only those on GTS list outside US, going to adm0 (country-level)
      to.adm0 <- unique(gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution == "United States"]))
      to.adm0 <- to.adm0[!is.na(to.adm0)]
  ## subset the species list to only those on GTS list to centroid at state level (adm1)
      to.adm1 <- gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution == "United States"])
      to.adm1 <- to.adm1[!is.na(to.adm1)]
  ## subset the species list to only those on GTS list within US, going to adm2 (county-level)
      to.adm2 <- unique(gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution == "United States"]))
      to.adm2 <- to.adm2[!is.na(to.adm2)]

  ## create new folder for revised points, if not already existing
  out.fld.nm <- "spp_edited_points"
  if(dir.exists(file.path(main_dir, "outputs", out.fld.nm))) print("directory already created") else
    dir.create(file.path(main_dir, "outputs", out.fld.nm), recursive=TRUE)

    ## to county/adm2 level
    ## United States goes down to county centroids
    cat("Starting ", "United States ", "taxa (", length(to.adm2), " total)", ".\n\n", sep="")

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
##      let's iterate United States
################################################################################
# i <- 43 # (in US dataset, 43 is Quercus_lobata)
i <- 1
for (i in 1:length(to.adm2)){
      f.nms <- to.adm2
      f.nm <- f.nms[i]
        cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")

  ## bring in records (load from *.RData file or bring in from text file)
  eo.df  <- read.csv(file.path(main_dir, "outputs", "working", "split_by_sp", 
              paste0(f.nm, ".csv")))
      if(nrow(eo.df) < 2) next

    ## extract administrative area (0,1,2) based upon inforamtion provided in 
    ##      record for country/state/county.
    proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    eo.spdf <- SpatialPointsDataFrame(eo.df[,c("decimalLongitude", "decimalLatitude")], eo.df,
                                      proj4string = CRS(proj4string4poly))

    ##set a variable to flag occurrences that are not in right place
    eo.spdf$occ_flag <- as.character(NA)

    ## flag records where point is outside polygons of land
      ## set column for match EOs in water
      eo.spdf$in_water <- as.character(NA)
      eo.spdf$in_water <- is.na(map.where(adm0.poly, eo.spdf$decimalLongitude,
                                        eo.spdf$decimalLatitude))

      out.uid <- as.character(eo.spdf$UID[eo.spdf$in_water == TRUE])
      
      if(length(out.uid) > 0){
        eo.spdf$occ_flag[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates are in water")
      }

##########
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
##########
      
      eo.spdf@data <- select(eo.spdf@data, -in_water)#, -in_lakes
        head(eo.spdf@data)

    ## set columns for match adm0, adm1 and adm2 to add to list 
    ##      and remove columns when done
    eo.spdf$match0 <- as.character(NA)
    eo.spdf$match1 <- as.character(NA)
    eo.spdf$match2 <- as.character(NA)

  ## write out the files if they have one or more EOs in water 
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

#######################################
    eo.post <- left_join(eo.2@data, gts_list[,c(2,4)], by=c("species_name_acc" = "taxon")) %>% select(-long_centroid, -lat_centroid)
      # eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% strsplit(native_distribution, split = "; "), TRUE, FALSE)))
  ## flag records where GTS country doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO0_match0=(ifelse(NAME_0 %in% gts_sub$native_distribution, TRUE, FALSE)))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match0 == TRUE])
    eo.post$match0[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same country (adm0) provided by Global Tree Search.")

  ## flag records where country doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO0_match1=(isTRUE(as.character(NAME_0) != as.character(country))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same country (adm0) provided in dataset.")

  ## flag records where GTS country doesn't given dataset country
    eo.post <- eo.post %>% mutate(ISO0_match2=(ifelse(NAME_0 %in% gts_sub$native_distribution, TRUE, FALSE)))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match2 == TRUE])
    eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given country provided by Global Tree Search not in the same country (adm0) provided in dataset.")

    eo.post <- eo.post %>% select(-ISO0_match0, -ISO0_match1, -ISO0_match2) %>% 
                mutate(adm0_match1=match0, adm0_match2=match1, adm0_match3=match2, 
                  match0=as.character(NA), match1=as.character(NA), match2=as.character(NA))
    
#######################################
  ## let's try state level (adm1)
    
  # ## flag records where GTS state (adm1) doesn't match the coordinates and then output into folder
  #   eo.post <- eo.post %>% mutate(ISO1_match0=(ifelse(NAME_1 %in% gts_sub$native_distribution, TRUE, FALSE)))
  #     out.uid <- as.character(eo.spdf$UID[eo.spdf$ISO1_match0 == TRUE])
  #   eo.post$match0[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates not in the same state (adm1) provided by Global Tree Search.")

  ## flag records where country doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO1_match1=(isTRUE(as.character(NAME_1) != as.character(stateProvince))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO1_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same state (adm1) provided in dataset.")

  # ## flag records where GTS state (adm1) doesn't given dataset state (adm1)
  #   eo.post <- eo.post %>% mutate(ISO1_match2=(isTRUE(as.character(NAME_1) != as.character(country))))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO1_match2 == TRUE])
  #   eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given state provided by Global Tree Search not in the same state (adm1) provided in dataset.")

    eo.post <- eo.post %>% select(-ISO1_match1) %>% 
                mutate(adm1_match2=match1, match1=as.character(NA))
#######################################
  ## let's try county level (adm2)
    
  # ## flag records where GTS state (adm1) doesn't match the coordinates and then output into folder
  #   eo.post <- eo.post %>% mutate(ISO2_match0=(ifelse(NAME_2 %in% gts_sub$native_distribution, TRUE, FALSE)))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO1_match0 == TRUE])
  #   eo.post$match0[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same county (adm2) provided by Global Tree Search.")

  ## flag records where county (adm2) doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO2_match1=(isTRUE(as.character(NAME_2) != as.character(county))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO1_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same county (adm2) provided in dataset.")

  # ## flag records where GTS county (adm2) doesn't given dataset county (adm2)
  #   eo.post <- eo.post %>% mutate(ISO2_match2=(isTRUE(as.character(NAME_2) != as.character(country))))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO1_match2 == TRUE])
  #   eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given county provided by Global Tree Search not in the same county (adm2) provided in dataset.")

    eo.post <- eo.post %>% select(-ISO2_match1) %>% 
                mutate(adm2_match2=match1, match1=as.character(NA))
#######################################

  eo.out <- eo.post
  write.csv(eo.out, file.path(main_dir, "outputs", out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)

  eo.spdf <- SpatialPointsDataFrame(eo.out[,c("decimalLongitude", "decimalLatitude")], eo.out,
                                    proj4string = CRS(proj4string4poly))

##############################################################################
## country (adm0) centroid comparison  
  ## make a distance matrix
    d.mat <- distm(eo.spdf, adm0.spdf)
      row.names(d.mat) <- eo.spdf$UID
      colnames(d.mat) <- adm0.spdf$UID

  ## check for any that fit a specific requirement (distance away)
      if(isTRUE(any(d.mat[d.mat < d.rm]))){

            ## set the values greater than the distance to NA
            cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
                d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
                d.mat[d.mat > d.rm] <- NA
                  df <- data.frame(d.mat)

                    # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
                    eo.spdf$occ_flag[eo.spdf$UID %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0("Check record for proximity to country/adm0 centroid (within ", d.rm, " meters).")

      } else {
      cat("No points are within ", d.rm, " meters of a centroid for any country (adm0) administrative area.\n\n", sep="")
      }
#######################################
## state (adm1) centroid comparison  
  ## make a distance matrix
    d.mat <- distm(eo.spdf, adm1.spdf)
      row.names(d.mat) <- eo.spdf$UID
      colnames(d.mat) <- adm1.spdf$UID

  ## check for any that fit a specific requirement (distance away)
      if(isTRUE(any(d.mat[d.mat < d.rm]))){

            ## set the values greater than the distance to NA
            cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
                d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
                d.mat[d.mat > d.rm] <- NA
                  df <- data.frame(d.mat)

                    # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
                    eo.spdf$occ_flag[eo.spdf$UID %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0("Check record for proximity to state/adm1 centroid (within ", d.rm, " meters).")

      } else {
      cat("No points are within ", d.rm, " meters of a centroid for any state (adm1)  administrative area.\n\n", sep="")
      }
#######################################
## county (adm2) centroid comparison  
  ## make a distance matrix
    d.mat <- distm(eo.spdf, adm2.spdf)
      row.names(d.mat) <- eo.spdf$UID
      colnames(d.mat) <- adm2.spdf$UID

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
##############################################################################
## reorder columns
h.nms <- c("UID", "species_name_acc", "taxon_name", "scientificName", "taxonIdentificationNotes", "database", "year", 
  "basisOfRecord", "establishmentMeans", "decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters", 
  "geolocationNotes", "county", "stateProvince", "country", "countryCode", "locationNotes", "datasetName", 
  "publisher", "nativeDatabaseID", "references", "informationWithheld", "issue", "taxon_name_full", "list", 
  "all_source_databases", "ID_0", "iso3c", "NAME_0", "ID_1", "NAME_1", "ID_2", "NAME_2", "UID_adm2", "native_distribution", 
  "coords_error", "occ_flag", "adm0_match1", "adm0_match2", "adm0_match3", "adm1_match2", "adm2_match2")
  
  eo.spdf@data <- eo.spdf@data %>% select(all_of(h.nms))
  
      write.csv(eo.spdf@data, file.path(main_dir, "outputs", out.fld.nm, 
            paste0(f.nm, ".csv")), row.names=FALSE)

      cat("Ending ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
      
}
  # save(gts_all, taxon_list, adm0.spdf, adm1.spdf, adm2.spdf, proj4string4poly, file="IMLS_data_workng.RData")
  # save(gts_all, taxon_list, adm0.spdf, adm1.spdf, adm2.spdf, proj4string4poly, file=file.path(main_dir, "outputs",
    # "working", "IMLS_data_workng_US.RData"))
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
  } else  {     #if(gts_sub$native_distribution != "United States"){
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
#######################################
    ## subset the species list to only those on GTS list outside US, going to adm0 (country-level)
        to.adm0 <- unique(gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution != "United States"]))
        to.adm0 <- to.adm0[!is.na(to.adm0)]
    ## subset the species list to only those on GTS list to centroid at state level (adm1)
        to.adm1 <- gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution != "United States"])
        to.adm1 <- to.adm1[!is.na(to.adm1)]
    ## subset the species list to only those on GTS list within US, going to adm2 (county-level)
        to.adm2 <- unique(gsub(" ", "_", gts_sub$taxon[gts_sub$native_distribution != "United States"]))
        to.adm2 <- to.adm2[!is.na(to.adm2)]


    ## create new folder for revised points, if not already existing
    out.fld.nm <- "spp_edited_points"
    if(dir.exists(file.path(main_dir, "outputs", out.fld.nm))) print("directory already created") else
      dir.create(file.path(main_dir, "outputs", out.fld.nm), recursive=TRUE)

    ## to county/adm2 level
    ## United States goes down to county centroids
    cat("Starting ", "non-United States ", "taxa (", length(to.adm1), " total)", ".\n\n", sep="")

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
##      let's iterate!
################################################################################
# i <- 43 # (in US dataset, 43 is Quercus_lobata)
for (i in 1:length(to.adm1)){
      f.nms <- to.adm1
      f.nm <- f.nms[i]
        cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")

  ## bring in records (load from *.RData file or bring in from text file)
  eo.df  <- read.csv(file.path(main_dir, "outputs", "working", "split_by_sp", 
              paste0(f.nm, ".csv")))
      if(nrow(eo.df) < 2) next

    ## extract administrative area (0,1,2) based upon inforamtion provided in 
    ##      record for country/state/county.
    proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    eo.spdf <- SpatialPointsDataFrame(eo.df[,c("decimalLongitude", "decimalLatitude")], eo.df,
                                      proj4string = CRS(proj4string4poly))

    ##set a variable to flag occurrences that are not in right place
    eo.spdf$occ_flag <- as.character(NA)

    ## flag records where point is outside polygons of land
      ## set column for match EOs in water
      eo.spdf$in_water <- as.character(NA)
      eo.spdf$in_water <- is.na(map.where(adm0.poly, eo.spdf$decimalLongitude,
                                        eo.spdf$decimalLatitude))

      out.uid <- as.character(eo.spdf$UID[eo.spdf$in_water == TRUE])
      
      if(length(out.uid) > 0){
        eo.spdf$occ_flag[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates are in water")
      }

      eo.spdf@data <- select(eo.spdf@data, -in_water)#, -in_lakes
        head(eo.spdf@data)

    ## set columns for match adm0, adm1 and adm2 to add to list 
    ##      and remove columns when done
    eo.spdf$match0 <- as.character(NA)
    eo.spdf$match1 <- as.character(NA)
    eo.spdf$match2 <- as.character(NA)

  ## write out the files if they have one or more EOs in water 
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

    ## get points that are in each polygon
    eo.0 <- point.in.poly(eo.spdf, adm0.poly, sp=TRUE)
    eo.1 <- point.in.poly(eo.spdf, adm1.poly, sp=TRUE)
    eo.2 <- point.in.poly(eo.spdf, adm2.poly, sp=TRUE)

    ## compare the calculated point to the provided admin area and flag those that do not match
        ##assign the native country (ISO0), state (ISO1), and county (ISO2) to the SPDF or dataset

    gts_sub <- gts_all[gts_all$taxon %in% eo.1@data$species_name_acc,]

#######################################
    eo.post <- left_join(eo.1@data, gts_list[,c(2,4)], by=c("species_name_acc" = "taxon")) %>% select(-long_centroid, -lat_centroid)
      # eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% strsplit(native_distribution, split = "; "), TRUE, FALSE)))
  ## flag records where GTS country doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO0_match0=(ifelse(NAME_0 %in% gts_sub$native_distribution, TRUE, FALSE)))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match0 == TRUE])
    eo.post$match0[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same country (adm0) provided by Global Tree Search.")

  ## flag records where country doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO0_match1=(isTRUE(as.character(NAME_0) != as.character(country))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same country (adm0) provided in dataset.")

  ## flag records where GTS country doesn't given dataset country
    eo.post <- eo.post %>% mutate(ISO0_match2=(ifelse(NAME_0 %in% gts_sub$native_distribution, TRUE, FALSE)))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match2 == TRUE])
    eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given country provided by Global Tree Search not in the same country (adm0) provided in dataset.")

    eo.post <- eo.post %>% select(-ISO0_match0, -ISO0_match1, -ISO0_match2) %>% 
                mutate(adm0_match1=match0, adm0_match2=match1, adm0_match3=match2, 
                  match0=as.character(NA), match1=as.character(NA), match2=as.character(NA))
    
#######################################
  ## let's try state level (adm1)
    
  # ## flag records where GTS state (adm1) doesn't match the coordinates and then output into folder
  #   eo.post <- eo.post %>% mutate(ISO1_match0=(ifelse(NAME_1 %in% gts_sub$native_distribution, TRUE, FALSE)))
  #     out.uid <- as.character(eo.spdf$UID[eo.spdf$ISO1_match0 == TRUE])
  #   eo.post$match0[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates not in the same state (adm1) provided by Global Tree Search.")

  ## flag records where dataset state doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO1_match1=(isTRUE(as.character(NAME_1) != as.character(stateProvince))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO1_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same state (adm1) provided in dataset.")

  # ## flag records where GTS state (adm1) doesn't given dataset state (adm1)
  #   eo.post <- eo.post %>% mutate(ISO1_match2=(isTRUE(as.character(NAME_1) != as.character(country))))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO1_match2 == TRUE])
  #   eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given state provided by Global Tree Search not in the same state (adm1) provided in dataset.")

    eo.post <- eo.post %>% select(-ISO1_match1) %>% 
                mutate(adm1_match2=match1, match1=as.character(NA))
#######################################
  ## let's try county level (adm2)
    
  # ## flag records where GTS state (adm1) doesn't match the coordinates and then output into folder
  #   eo.post <- eo.post %>% mutate(ISO2_match0=(ifelse(NAME_2 %in% gts_sub$native_distribution, TRUE, FALSE)))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO1_match0 == TRUE])
  #   eo.post$match0[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same county (adm2) provided by Global Tree Search.")
# 
#   ## flag records where county (adm2) doesn't match the coordinates and then output into folder
#     eo.post <- eo.post %>% mutate(ISO2_match1=(isTRUE(as.character(NAME_2) != as.character(county))))
#       out.uid <- as.character(eo.post$UID[eo.post$ISO1_match1 == TRUE])
#     eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same county (adm2) provided in dataset.")
# 
  # ## flag records where GTS county (adm2) doesn't given dataset county (adm2)
  #   eo.post <- eo.post %>% mutate(ISO2_match2=(isTRUE(as.character(NAME_2) != as.character(country))))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO1_match2 == TRUE])
  #   eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given county provided by Global Tree Search not in the same county (adm2) provided in dataset.")

    eo.post <- eo.post %>% #select(-ISO2_match1) %>%
                mutate(adm2_match2=match1, match1=as.character(NA))
#######################################
  eo.out <- eo.post
  write.csv(eo.out, file.path(main_dir, "outputs", out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)


  eo.spdf <- SpatialPointsDataFrame(eo.out[,c("decimalLongitude", "decimalLatitude")], eo.out,
                                    proj4string = CRS(proj4string4poly))

##############################################################################
## country (adm0) centroid comparison  
  ## make a distance matrix
    d.mat <- distm(eo.spdf, adm0.spdf)
      row.names(d.mat) <- eo.spdf$UID
      colnames(d.mat) <- adm0.spdf$UID

  ## check for any that fit a specific requirement (distance away)
      if(isTRUE(any(d.mat[d.mat < d.rm]))){

            ## set the values greater than the distance to NA
            cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
                d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
                d.mat[d.mat > d.rm] <- NA
                  df <- data.frame(d.mat)

                    # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
                    eo.spdf$occ_flag[eo.spdf$UID %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0("Check record for proximity to country/adm0 centroid (within ", d.rm, " meters).")

      } else {
      cat("No points are within ", d.rm, " meters of a centroid for any country (adm0) administrative area.\n\n", sep="")
      }
#######################################
## state (adm1) centroid comparison  
  ## make a distance matrix
    d.mat <- distm(eo.spdf, adm1.spdf)
      row.names(d.mat) <- eo.spdf$UID
      colnames(d.mat) <- adm1.spdf$UID

  ## check for any that fit a specific requirement (distance away)
      if(isTRUE(any(d.mat[d.mat < d.rm]))){

            ## set the values greater than the distance to NA
            cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
                d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
                d.mat[d.mat > d.rm] <- NA
                  df <- data.frame(d.mat)

                    # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
                    eo.spdf$occ_flag[eo.spdf$UID %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0("Check record for proximity to state/adm1 centroid (within ", d.rm, " meters).")

      } else {
      cat("No points are within ", d.rm, " meters of a centroid for any state (adm1) administrative area.\n\n", sep="")
      }
#######################################
## county (adm2) centroid comparison  
  ## make a distance matrix
    d.mat <- distm(eo.spdf, adm2.spdf)
      row.names(d.mat) <- eo.spdf$UID
      colnames(d.mat) <- adm2.spdf$UID

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
##############################################################################
## reorder columns
h.nms <- c("UID", "species_name_acc", "taxon_name", "scientificName", "taxonIdentificationNotes", "database", "year", 
  "basisOfRecord", "establishmentMeans", "decimalLongitude", "decimalLatitude", "coordinateUncertaintyInMeters", 
  "geolocationNotes", "county", "stateProvince", "country", "countryCode", "locationNotes", "datasetName", 
  "publisher", "nativeDatabaseID", "references", "informationWithheld", "issue", "taxon_name_full", "list", 
  "all_source_databases", "ID_0", "iso3c", "NAME_0", "ID_1", "NAME_1", "ID_2", "NAME_2", "UID_adm2", "native_distribution", 
  "coords_error", "occ_flag", "adm0_match1", "adm0_match2", "adm0_match3", "adm1_match2", "adm2_match2")
  eo.spdf                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
  eo.spdf@data <- eo.spdf@data %>% select(all_of(h.nms))
      
      write.csv(eo.spdf@data, file.path(main_dir, "outputs", out.fld.nm, 
            paste0(f.nm, ".csv")), row.names=FALSE)

      cat("Ending ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
      
      }
  }  ## end world loop

  rm(i, out.fld.nm, f.nm, f.nms, f.subs, out.uid)
  rm(adm0, adm1, adm2, adm0.poly, adm1.poly, adm2.poly, d.mat)

################################################################################
# end script
################################################################################

