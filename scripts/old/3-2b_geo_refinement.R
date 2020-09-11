################################################################################

## 3-2b_geo_refinement.R
### Authors: Shannon M. Still & Emily Beckman ### Date: 08/10/2020

### DESCRIPTION:
  #

### DATA IN:
  #

### DATA OUT:
  #

################################################################################
## remove some fields to reduce size of the data
################################################################################

re.nms <- c("UID", "species_name_acc", "taxon_name", "scientificName", "taxonIdentificationNotes", "database",
  "year", "basisOfRecord", "establishmentMeans", "decimalLatitude", "decimalLongitude",
  "coordinateUncertaintyInMeters", "geolocationNotes", "county", "stateProvince", "country",
  "countryCode", "locationNotes", "datasetName", "publisher", "nativeDatabaseID", "references",
  "informationWithheld", "issue", "taxon_name_full", "list", "coords_error", "lat_round", "long_round",
  "all_source_databases", "occ_flag", "adm0_match1", "adm0_match2", "adm0_match3", "adm0_type", "NAME_0",
  "adm0_a3", "name", "iso2c", "iso_a3", "iso3n", "iso3c", "ne_id", "UID_adm0", "native_distribution")
re.nms2 <- c("UID", "species_name_acc", "taxon_name", "scientificName", "taxonIdentificationNotes", "database",
  "year", "basisOfRecord", "establishmentMeans", "decimalLatitude", "decimalLongitude",
  "coordinateUncertaintyInMeters", "geolocationNotes", "county", "stateProvince", "country",
  "countryCode", "locationNotes", "datasetName", "publisher", "nativeDatabaseID", "references",
  "informationWithheld", "issue", "taxon_name_full", "list", "coords_error", "lat_round", "long_round",
  "all_source_databases", "occ_flag", "adm0_match1", "adm0_match2", "adm0_match3", "featurecla",
  "scalerank", "labelrank", "sovereignt", "sov_a3", "adm0_dif", "level", "adm0_type", "NAME_0",
  "adm0_a3", "geou_dif", "geounit", "gu_a3", "su_dif", "subunit", "su_a3", "brk_diff", "name",
  "name_long", "brk_a3", "brk_name", "brk_group", "abbrev", "postal", "formal_en", "formal_fr",
  "name_ciawf", "note_adm0", "note_brk", "name_sort", "name_alt", "mapcolor7", "mapcolor8",
  "mapcolor9", "mapcolor13", "pop_est", "pop_rank", "gdp_md_est", "pop_year", "lastcensus",
  "gdp_year", "economy", "income_grp", "wikipedia", "fips_10_", "iso2c", "iso_a3", "iso_a3_eh",
  "iso3n", "un_a3", "wb_a2", "wb_a3", "woe_id", "woe_id_eh", "woe_note", "adm0_a3_is", "iso3c",
  "adm0_a3_un", "adm0_a3_wb", "continent", "region_un", "subregion", "region_wb", "name_len",
  "long_len", "abbrev_len", "tiny", "homepart", "min_zoom", "min_label", "max_label", "ne_id",
  "wikidataid", "name_ar", "name_bn", "name_de", "name_en", "name_es", "name_fr", "name_el",
  "name_hi", "name_hu", "name_id", "name_it", "name_ja", "name_ko", "name_nl", "name_pl", "name_pt",
  "name_ru", "name_sv", "name_tr", "name_vi", "name_zh", "UID_adm0", "native_distribution")
rm.nms <- re.nms2[!re.nms2 %in% re.nms]

################################################################################
## let's iterate United States
################################################################################

# i <- 43 # (in US dataset, 43 is Quercus_lobata)
# i <- 52
# i <- 11
      f.nms <- to.adm
      f.nm <- f.nms[i]
        cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")

## species native distribution
  s.nd.gts.gts   <- gts_list$gts_native_dist_iso2c[gts_list$taxon==gsub("_"," ", f.nm)]
## species native distribution list
  s.nd.gts.l <- unlist(strsplit(s.nd.gts, "; "))

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

#    ## flag records where point is outside polygons of land
#      ## set column for match EOs in water
#      eo.spdf$in_water <- as.character(NA)
#      eo.spdf$in_water <- is.na(map.where(adm0.poly, eo.spdf$decimalLongitude,
#                                        eo.spdf$decimalLatitude))
#
#      out.uid <- as.character(eo.spdf$UID[eo.spdf$in_water == TRUE])
#
#      if(length(out.uid) > 0){
#        eo.spdf$occ_flag[eo.spdf$UID %in% out.uid] <- paste0("Given coordinates are in water")
#      }

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

#      eo.spdf@data <- eo.spdf@data %>% select(-in_water)#, -in_lakes

  ## write out the files if they have one or more EOs in water
    if(length(eo.spdf[!is.na(eo.spdf$occ_flag),]) > 0){
      out.fld.nm <- "spp_edited_points"
        if(dir.exists(file.path(main_dir, "outputs", "working", "records_to_examine"))) print("directory already created") else
          dir.create(file.path(main_dir, "outputs", "working", "records_to_examine"), recursive=TRUE)
        if(file.exists(file.path(main_dir, "outputs", "working", "records_to_examine", paste0("water_points_", Sys.Date(), ".csv")))){
            write_excel_csv(eo.spdf@data[!is.na(eo.spdf$occ_flag),], file.path(main_dir, "outputs", "working", "records_to_examine",
                paste0("water_points_", Sys.Date(), ".csv")), append = TRUE)
            # write.csv(eo.spdf[!is.na(eo.spdf$occ_flag),], file.path(main_dir, "outputs", "working", "records_to_examine",
      # } else write.csv(eo.spdf[!is.na(eo.spdf$occ_flag),], file.path(main_dir, "outputs", "working", "records_to_examine",
      } else write_excel_csv(eo.spdf@data[!is.na(eo.spdf$occ_flag),], file.path(main_dir, "outputs", "working", "records_to_examine",
                paste0("water_points_", Sys.Date(), ".csv")))
    }

    ## set columns for match adm0, adm1 and adm2 to add to list
    ##      and remove columns when done
    eo.spdf$match0 <- as.character(NA)
    eo.spdf$match1 <- as.character(NA)
    eo.spdf$match2 <- as.character(NA)

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

    ## compare the calculated point to the provided admin area and flag those that do not match
        ##assign the native country (ISO0), state (ISO1), and county (ISO2) to the SPDF or dataset
#     eo.0 <- point.in.poly(eo.spdf, adm0.poly, sp=TRUE)
    # gts_sub <- gts_all[gts_all$taxon %in% gsub("_"," ", f.nm),]

#######################################
    ## get points that are in each polygon
    eo.post <- left_join(point.in.poly(eo.spdf, adm0.poly, sp=TRUE)@data, gts_list[,c(2,4)], by=c("species_name_acc" = "taxon")) %>% select(-long_centroid, -lat_centroid)

  ## flag records where GTS country doesn't match the coordinates and then output into folder
    # eo.post <- eo.post %>% mutate(ISO0_match0=(ifelse(NAME_0 %in% strsplit(native_distribution, split = "; "), TRUE, FALSE)))
    eo.post <- eo.post %>% mutate(ISO0_match0=(ifelse(NAME_0 %in% s.nd.gts.l, TRUE, FALSE)))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match0 == TRUE])
    eo.post$match0[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same country (adm0) provided by Global Tree Search.")

  ## flag records where country doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO0_match1=(isTRUE(as.character(NAME_0) != as.character(country))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same country (adm0) provided in dataset.")

  ## flag records where GTS country doesn't match given dataset country
    eo.post <- eo.post %>% mutate(ISO0_match2=(ifelse(NAME_0 %in% s.nd.gts.l, TRUE, FALSE)))
      out.uid <- as.character(eo.post$UID[eo.post$ISO0_match2 == TRUE])
    eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given country provided by Global Tree Search not in the same country (adm0) provided in dataset.")

    eo.out <- eo.post %>% select(-ISO0_match0, -ISO0_match1, -ISO0_match2) %>%
                rename(adm0_match1=match0, adm0_match2=match1, adm0_match3=match2) %>% select(-all_of(rm.nms))
#######################################
  ## let's try state level (adm1)
    eo.post <- left_join(point.in.poly(eo.spdf, adm1.poly, sp=TRUE)@data, gts_list[,c(2,4)], by=c("species_name_acc" = "taxon")) %>% select(-long_centroid, -lat_centroid)

  # ## flag records where GTS state (adm1) doesn't match the coordinates and then output into folder
    # eo.post <- eo.post %>% mutate(ISO1_match0=(ifelse(NAME_1 %in% strsplit(native_distribution, split = "; "), TRUE, FALSE)))
    eo.post <- eo.post %>% mutate(ISO1_match0=(ifelse(NAME_1 %in% s.nd.gts.l, TRUE, FALSE)))
      out.uid <- as.character(eo.post$UID[eo.post$ISO1_match0 == TRUE])
    eo.post$match0[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same state (adm1) provided by Global Tree Search.")

  ## flag records where stateProvidence doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO1_match1=(isTRUE(as.character(NAME_1) != as.character(stateProvince))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO1_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same state (adm1) provided in dataset.")

  # ## flag records where GTS state (adm1) doesn't given dataset state (adm1)
  #   eo.post <- eo.post %>% mutate(ISO1_match2=(isTRUE(as.character(NAME_1) != as.character(country))))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO1_match2 == TRUE])
  #   eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given state provided by Global Tree Search not in the same state (adm1) provided in dataset.")

    eo.post <- eo.post %>% rename(adm1_match2=match1) %>%
              select(UID, NAME_1, UID_adm1, adm1_match2)

  eo.out <- left_join(eo.out, eo.post, by="UID")

#######################################
  ## let's try county level (adm2)
    eo.post <- left_join(point.in.poly(eo.spdf, adm2.poly, sp=TRUE)@data, gts_list[,c(2,4)], by=c("species_name_acc" = "taxon")) %>% select(-long_centroid, -lat_centroid)

  # ## flag records where GTS state (adm1) doesn't match the coordinates and then output into folder
  #   eo.post <- eo.post %>% mutate(ISO2_match0=(ifelse(NAME_2 %in% gts_sub$native_distribution, TRUE, FALSE)))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO2_match0 == TRUE])
  #   eo.post$match0[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same county (adm2) provided by Global Tree Search.")

  ## flag records where county (adm2) doesn't match the coordinates and then output into folder
    eo.post <- eo.post %>% mutate(ISO2_match1=(isTRUE(as.character(NAME_2) != as.character(county))))
      out.uid <- as.character(eo.post$UID[eo.post$ISO2_match1 == TRUE])
    eo.post$match1[eo.post$UID %in% out.uid] <- paste0("Given coordinates not in the same county (adm2) provided in dataset.")

  # ## flag records where GTS county (adm2) doesn't given dataset county (adm2)
  #   eo.post <- eo.post %>% mutate(ISO2_match2=(isTRUE(as.character(NAME_2) != as.character(county))))
  #     out.uid <- as.character(eo.post$UID[eo.post$ISO2_match2 == TRUE])
  #   eo.post$match2[eo.post$UID %in% out.uid] <- paste0("Given county provided by Global Tree Search not in the same county (adm2) provided in dataset.")

    eo.post <- eo.post %>% mutate(adm2_match2=match1) %>%
            select(UID, ID_0, ID_1, ID_2, NAME_2, UID_adm2, adm2_match2)

#######################################
  eo.out <- left_join(eo.out, eo.post, by="UID")
      rm(eo.post)
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
      # if(isTRUE(any(d.mat[d.mat < d.rm]))){
      if(isTRUE(any(as.logical(d.mat[d.mat < d.rm])))){

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
        if(isTRUE(any(as.logical(d.mat[d.mat < d.rm])))){

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
      if(isTRUE(any(as.logical(d.mat[d.mat < d.rm])))){

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
  "all_source_databases", "ID_0", "iso3c", "NAME_0", "UID_adm0", "ID_1", "NAME_1", "UID_adm1",
  "ID_2", "NAME_2", "UID_adm2", "native_distribution",
  "coords_error", "occ_flag", "adm0_match1", "adm0_match2", "adm0_match3", "adm1_match2", "adm2_match2")

  eo.spdf@data <- eo.spdf@data %>% select(all_of(h.nms))

      write.csv(eo.spdf@data, file.path(main_dir, "outputs", out.fld.nm,
            paste0(f.nm, ".csv")), row.names=FALSE)

      cat("Ending ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")

      rm(f.nm, s.nd.gts, s.nd.gts.l, d.mat, df)
################################################################################
# end United States script
################################################################################
