## 3.5_geo_data_work.R
### Author:  Shannon M. Still     ###  Date: 05/19/2020                                |

### DESCRIPTION:
## This script is meant for massaging geographical data in both spatial polygon and tabular formats.

### INPUT:
# output from 3_compile_raw_occurrence_points.R
# tabular data:
#target_taxa_with_syn.csv
#globaltreesearch_country_distribution.csv
# spatialpolygon data ...
#

### OUTPUTS:
# XXXXX.RData
#occurrence_point_count_per_species.csv

#################
### LIBRARIES ###
#################

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
#######################################
# run code to set your working directory and project folders based upon computer being used
#      skip this if preferred, but then need to set your working directory and input/output folders manually
####################################################################################
# setwd("./../..")
# setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")
source('scripts/set_workingdirectory.R')
# setwd("/Volumes/GoogleDrive/Shared drives/IMLS MFA/insitu_occurrence_points")
# '/Users/aesculus/Box/Research/Active_Projects/IMLS MFA/IMLS_CollectionsValue'
################################################################################

#######################################
## load functions
####################################################################################
source('scripts/load_IMLS_functions.R')
####################################################################################

# # TO DO:
#
# #Create tables for walkover
# #   - Make taxon name table for cross-referencing synonyms, including country/state/county of origin
# #   - Assign country codes to species in species table (ISO code)
#
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
#
# #Flag occurrences outside their native distribution
#
# #   - Coordinate uncertainty in meters
# #(remove record if coordinate uncertainty is larger than specified acceptable uncertainty)
#
#
####################################################################################
# ## load .Rdata file
# # source(file.path(data_in, "EO_data.RData"))
# 
# ## taxon table
# taxon_list <- read.csv(file.path(data_in, 'insitu_occurrence_points', "target_taxa_with_syn.csv"), header = T, na.strings=c("","NA"),
#                        colClasses="character")
# gts_list <- read.csv(file.path(data_in, 'insitu_occurrence_points', 'globaltreesearch_country_distribution.csv'), header = T, na.strings=c("","NA"),
#                      colClasses="character")
# #
# # ################################################################################
# # # # Split Global Tree Search countries by their delimiter (;), and create separated country list for each taxon
# # ################################################################################
# gts_all <- gts_list %>% mutate(native_distribution = strsplit(as.character(native_distribution), "; ")) %>% unnest(native_distribution)
# 
# ## looking at all countries for all species. The following line was to split the countries for all species in Global Tree Seach data and write out.
#     spp_countries <- as.data.frame(sort(unique(gts_all$native_distribution)))
# 
#     ## write out GTS countries to check
#     write_xlsx(spp_countries, path=file.path(data_in, 'data_working', 'globaltreesearch_countries.xlsx'))
# 
# ### use countrycode package to translate country codes from the country names
# # countrycode(sort(unique(gts_list$native_distribution)), origin='country.name', destination='fips')
# country_set <- as.data.frame(sort(unique(gts_all$native_distribution))) %>%
#   add_column(iso3c = countrycode(sort(unique(gts_all$native_distribution)), origin='country.name', destination='iso3c')) %>%
#     add_column(iso2c = countrycode(sort(unique(gts_all$native_distribution)), origin='country.name', destination='iso2c')) %>%
#       add_column(iso3n = countrycode(sort(unique(gts_all$native_distribution)), origin='country.name', destination='iso3n')) %>%
#         add_column(fips = countrycode(sort(unique(gts_all$native_distribution)), origin='country.name', destination='fips'))
# names(country_set)[1] <- 'country_name'
# 
# ## Save the country codes for species, ISO2, ISO3, and numeric and character codes, FIPS code
#   write_xlsx(country_set, path=file.path(data_in, 'global_admin_areas.xlsx'))
# # ################################################################################
# # add the country code to the GTS list by matching taxon names
# # names(gts_list)
# 
# # gts_list <- gts_list %>%
# 
# ## add the country code to the taxon list by matching taxon names
# taxon_co <- left_join(taxon_list, gts_list[,c(2,4)], by=c('species_name_acc' = 'taxon'))
# 
# write_xlsx(taxon_co, path=file.path(data_in, "taxa_work.xlsx"))
# 
# ################################################################################
# ## bring in polygon for world regions and US (down to county level)
# world.poly <- readOGR(dsn=file.path(data_in, 'gis_data', 'world', 'TM_WORLD_BORDERS-0.3'), 'TM_WORLD_BORDERS-0.3')
# # plot(world.poly)
# us0.poly <- readOGR(dsn=file.path(data_in, 'gis_data', 'usa', 'USA_adm'), 'USA_adm0')
# us1.poly <- readOGR(dsn=file.path(data_in, 'gis_data', 'usa', 'USA_adm'), 'USA_adm1')
# us2.poly <- readOGR(dsn=file.path(data_in, 'gis_data', 'usa', 'USA_adm'), 'USA_adm2')
# 
#   # us1.poly[us1.poly$NAME_1 == 'California',]
#   # head(us1.poly@data)
#   # plot(us1.poly[us1.poly$NAME_1 == 'California',])
#   # us2.poly@data
# 
# # write_xlsx(us2.poly@data, path=file.path(data_in, "geo_work.xlsx"))
# 
# save(world.poly, us0.poly, us1.poly, us2.poly, gts_list, file=file.path(data_in, 'gis_data', 'IMLS_GIS_data.RData'))
# ################################################################################
# ## calculate centroid of polygons
#   ##first countries (adm0)
#   adm0 <- world.poly@data
#       adm0 <- adm0 %>% mutate(long_calc = unlist(lapply(adm0$ISO3, function(x) centroid(world.poly[world.poly$ISO3 == x,])[,1])), lat_calc = unlist(lapply(adm0$ISO3, function(x) centroid(world.poly[world.poly$ISO3 == x,])[,2])))
# 
#   ##second states (adm1)
#     us1 <- us1.poly@data
#       adm1 <- us1 %>% mutate(long_calc = unlist(lapply(us1$ID_1, function(x) centroid(us1.poly[us1.poly$ID_1 == x,])[,1])), lat_calc = unlist(lapply(us1$ID_1, function(x) centroid(us1.poly[us1.poly$ID_1 == x,])[,2])))
#   ##third counties/municipalities/parishes (adm2)
#     us2 <- us2.poly@data
#       adm2 <- us2 %>% mutate(long_calc = unlist(lapply(us2$ID_2, function(x) centroid(us2.poly[us2.poly$ID_2 == x,])[,1])), lat_calc = unlist(lapply(us2$ID_2, function(x) centroid(us2.poly[us2.poly$ID_2 == x,])[,2])))
# ## save these objects for later use
# save(world.poly, us0.poly, us1.poly, us2.poly, taxon_list, gts_list, gts_all, adm0, adm1, adm2, file=file.path(data_in, 'gis_data', 'IMLS_GIS_data.RData'))
#   rm(adm0, adm1, adm2, us1, us2, world.poly, us0.poly, us1.poly, us2.poly)
# ################################################################################
# # # # ###   NEEDS TO BE COMPLETED   ###
# # # #   ## Assign coordinates to records that are missing coordinates
# # # #     ## based on country/state/county
# # # #   load(file.path(data_in, 'gis_data', 'IMLS_GIS_data.RData'))
# # # # 
# # # #   ## bring in records (at least those missing coordinates)
# # # #   
# ################################################################################
# ###   NEEDS TO BE COMPLETED   ###
#   ## Flag occurrences outside their native distribution
#       ## based on country/state/county

  # spp.test <- c("Quercus georgiana", "Quercus imbricaria", "Quercus arkansana", "Quercus falcata", "Quercus stellata", "Quercus acutissima")
    

  load(file.path(data_in, 'gis_data', 'IMLS_GIS_data.RData'))
  
  ## set the distance in meters to check from a point (for comparing centroids to EOs)
  d.rm <- 1000
  
  world.poly@data <- world.poly@data %>% rename(NAME_0 = NAME)
  
  ## give a unique identifier to adm0 (country), adm1 (state) and adm2 (county level)
  
      adm0 <- adm0 %>% mutate(UID = paste0('adm0_', ISO2)) %>% select(UID, colnames(adm0))
      adm1 <- adm1 %>% mutate(UID = paste0('adm1_', ID_1)) %>% select(UID, colnames(adm1))
      adm2 <- adm2 %>% mutate(UID = paste0('adm2_', ID_2)) %>% select(UID, colnames(adm2))
  # adm2$UID
  ## create proj4string to set coords
      proj4string4poly <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
  
  ## Antarctica has NaN for centroid...remove it    
      adm0 <- adm0[!is.na(adm0$long_calc),]
  
  ## create spatialPolygonDataframes for adm0, adm1, adm2 levels
      adm0.spdf <- SpatialPointsDataFrame(adm0[,c('long_calc', 'lat_calc')], adm0,
                                          proj4string = CRS(proj4string4poly))
      adm1.spdf <- SpatialPointsDataFrame(adm1[,c('long_calc', 'lat_calc')], adm1,
                                          proj4string = CRS(proj4string4poly))
      adm2.spdf <- SpatialPointsDataFrame(adm2[,c('long_calc', 'lat_calc')], adm2,
                                          proj4string = CRS(proj4string4poly))
  
    #######
## this is start of iterative loop from the United States and other countries
    ## let's iterate! 
        ## get list of files to iterate
      all.spp.files <- list.files(path=file.path(data_in, 'insitu_occurrence_points/raw_split_by_sp'), ignore.case=FALSE, full.names=FALSE, recursive=TRUE) #pattern=".thresh", # can use this 'pattern=...' to find only files that have a specific pattern
          # all.poly.files <- list.files(path=file.path(f_out, 'poly_models_thresh'), pattern=".thresh", ignore.case=FALSE, full.names=FALSE, recursive=TRUE)
      all.spp.files <- all.spp.files[1:5]
      ## subset the species list to only those on GTS list within US

        f.subs <- file_path_sans_ext(all.spp.files)
    
        gts_sub <- gts_all[gts_all$taxon %in% gsub('_', ' ', f.subs),]
        # f.nms <- gsub(' ', '_', gts_sub$taxon[gts_sub$native_distribution == 'United States'])
        
      ## subset the species list to only those on GTS list within US, going to adm2 (county-level)
          to.adm2 <- unique(gsub(' ', '_', gts_sub$taxon[gts_sub$native_distribution == 'United States']))
      ## subset the species list to only those on GTS list to centroid at state level (adm1)
          # to.adm1 <- gsub(' ', '_', gts_sub$taxon[gts_sub$native_distribution == 'United States'])
      ## subset the species list to only those on GTS list outside US, going to adm0 (country-level)
          # to.adm0 <- unique(gsub(' ', '_', gts_sub$taxon[gts_sub$native_distribution != 'United States']))
          to.adm0 <- unique(gsub(' ', '_', gts_sub$taxon[!gts_sub$taxon %in% gsub('_', ' ', to.adm2)]))
          
    # if(gts_sub$native_distribution == 'United States'){


      ## create new folder for revised points, if not already existing
      out.fld.nm <- 'spp_edited_points'
      if(dir.exists(file.path(data_in, out.fld.nm))) print('directory already created') else
        dir.create(file.path(data_in, out.fld.nm), recursive=TRUE)
  
      ## to county/adm2 level
      ## United States goes down to county centroids
      cat("Starting ", "United States ", "taxa (", length(to.adm2), " total)", ".\n\n", sep="")
i <- 1
for (i in 1:length(to.adm2)){
      f.nms <- to.adm2
      f.nm <- f.nms[i]
        cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
      
  ## bring in records (load from *.RData file or bring in from text file)
    eo.df  <- read.csv(file.path(data_in, 'insitu_occurrence_points/raw_split_by_sp', paste0(f.nm, '.csv')))

    ## extract administrative area (0,1,2) based upon inforamtion provided in record for country/state/county.
    proj4string4poly <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
    eo.spdf <- SpatialPointsDataFrame(eo.df[,c('decimalLongitude', 'decimalLatitude')], eo.df,
                                      proj4string = CRS(proj4string4poly))
    eo.0 <- point.in.poly(eo.spdf, world.poly, sp=TRUE)
    eo.1 <- point.in.poly(eo.spdf, us1.poly, sp=TRUE)
    eo.2 <- point.in.poly(eo.spdf, us2.poly, sp=TRUE)
    # eo.1@data <- eo.1@data %>% select(-FIPS)
    # eo.2@data <- eo.2@data %>% select(-FIPS)
    
    ## compare the calculated area to the provided area and flag those that do not match
        ##assign the native country (ISO0), state (ISO1), and county (ISO2) to the SPDF or dataset

    gts_sub <- gts_all[gts_all$taxon %in% eo.2@data$taxon_name,]
    # eo.2@data <- eo.2@data %>% select(-FIPS)
    eo.post <- left_join(eo.2@data, gts_list[,c(2,4)], by=c('taxon_name_acc' = 'taxon'))

      eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% gts_sub$native_distribution, TRUE, FALSE)))
      # eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% strsplit(native_distribution, split = '; '), TRUE, FALSE)))

      # write_xlsx(eo.post, path=file.path(data_in, "match_work.xlsx"))

## remove records where country doesn't match and then output into folder (as *.csv)
  eo.out <- eo.post %>% filter(ISO1_match==TRUE) %>% select(names(eo.df))

  write.csv(eo.out, file.path(data_in, out.fld.nm, paste0(f.nm, '.csv')), row.names=FALSE)


  eo.spdf <- SpatialPointsDataFrame(eo.out[,c('decimalLongitude', 'decimalLatitude')], eo.out,
                                    proj4string = CRS(proj4string4poly))
  # save(eo.spdf, world.poly, us0.poly, us1.poly, us2.poly, taxon_list, gts_list, gts_all, adm0, adm1, adm2, file=file.path(data_in, 'gis_data', 'IMLS_GIS_data.RData'))
  # plot(eo.spdf)
  
    ## make a distance matrix
    d.mat <- distm(eo.spdf, adm2.spdf)
      row.names(d.mat) <- eo.spdf$X.1
      colnames(d.mat) <- adm2.spdf$UID
## if some values are within, then find which are within distance and flag them
      ## set field for flag
      eo.spdf$occ_flag <- ''

  ## check for any that fit a specific requirement (distance away)
      if(isTRUE(any(d.mat[d.mat < d.rm]))){
        
            ## set the values greater than the distance to NA
            cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
                d.mat[upper.tri(d.mat, diag=TRUE)] <- NA        
                d.mat[d.mat > d.rm] <- NA
                  df <- data.frame(d.mat)
          
                    # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
                    eo.spdf$occ_flag[eo.spdf$X.1 %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0('Check record for proximity to county/adm2 centroid (within ', d.rm, ' meters).')
          
      } else {
      cat("No points are within ", d.rm, " meters of a centroid for any administrative area.\n\n", sep="")
      }
      
      write.csv(eo.spdf@data, file.path(data_in, out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)
      
      cat("Ending ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
      
        }


################################################################################

  cat("Starting ", "rest of world ", "taxa (", length(to.adm0), " total)", ".\n\n", sep="")
  # i <- 14
      for (i in 1:length(to.adm0)){
        f.nms <- to.adm0
        f.nm <- f.nms[i]
            cat("Starting ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
       
      ## bring in records (load from *.RData file or bring in from text file)
      eo.df  <- read.csv(file.path(data_in, 'insitu_occurrence_points/raw_split_by_sp', paste0(f.nm, '.csv')))
      eo.df  <- eo.df %>% select(-FIPS)
      ## extract administrative area (0,1,2) based upon inforamtion provided in record for country/state/county.
      proj4string4poly <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'
      eo.spdf <- SpatialPointsDataFrame(eo.df[,c('decimalLongitude', 'decimalLatitude')], eo.df,
                                        proj4string = CRS(proj4string4poly))
      eo.0 <- point.in.poly(eo.spdf, world.poly, sp=TRUE)
      ## compare the calculated area to the provided area and flag those that do not match
      ##assign the native country (ISO0), state (ISO1), and county (ISO2) to the SPDF or dataset
      
      gts_sub <- gts_all[gts_all$taxon %in% eo.0@data$taxon_name,]
      
      eo.post <- left_join(eo.0@data, gts_list[,c(2,4)], by=c('taxon_name_acc' = 'taxon'))

      eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% gts_sub$native_distribution, TRUE, FALSE)))
      # eo.post <- eo.post %>% mutate(ISO1_match=(ifelse(NAME_0 %in% strsplit(native_distribution, split = '; '), TRUE, FALSE)))
      
      # write_xlsx(eo.post, path=file.path(data_in, "match_work.xlsx"))
      
      ## remove records where country doesn't match and then output into folder (as *.csv)
      eo.out <- eo.post %>% filter(ISO1_match==TRUE) %>% select(names(eo.df))
      
      write.csv(eo.out, file.path(data_in, out.fld.nm, paste0(f.nm, '.csv')), row.names=FALSE)
      
      
      eo.spdf <- SpatialPointsDataFrame(eo.out[,c('decimalLongitude', 'decimalLatitude')], eo.out,
                                        proj4string = CRS(proj4string4poly))

      ## make a distance matrix
      d.mat <- distm(eo.spdf, adm2.spdf)
      row.names(d.mat) <- eo.spdf$X.1
      colnames(d.mat) <- adm2.spdf$UID
      ## if some values are within, then find which are within distance and flag them
      ## set field for flag
      eo.spdf$occ_flag <- ''
      
      ## check for any that fit a specific requirement (distance away)
      if(isTRUE(any(d.mat[d.mat < d.rm]))){
        
        ## set the values greater than the distance to NA
        cat("One or more of the element occurrence points are within", d.rm, "m of a centroid for an administrative area. \n Please check to determine which points are close and whether they should be removed.\n\n")
        d.mat[upper.tri(d.mat, diag=TRUE)] <- NA        
        d.mat[d.mat > d.rm] <- NA
        df <- data.frame(d.mat)
        
        # row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])
        eo.spdf$occ_flag[eo.spdf$X.1 %in% row.names(df[data.frame(which(df <= d.rm, arr.ind=TRUE))$row,])] <- paste0('Check record for proximity to county/adm2 centroid (within ', d.rm, ' meters).')
        
      } else {
        cat("No points are within ", d.rm, " meters of a centroid for any administrative area.\n\n", sep="")
      }
      
      write.csv(eo.spdf@data, file.path(data_in, out.fld.nm, paste0(f.nm, ".csv")), row.names=FALSE)
      
      cat("Ending ", f.nm, ", ", i, " of ", length(f.nms), ".\n\n", sep="")
      
      }

      ################################################################################
      #   ## STILL WORKING ON THIS SECTION ## DON'T KNOW THAT THIS IS NECESSARY. WE COULD LOOK FOR DUPLICATES WITHIN EACH RASTER CELL
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
      #   row.names(d.mat) <- eo.spdf2$X.1
      #   colnames(d.mat) <- eo.spdf2$X.1
      #   d.mat[upper.tri(d.mat, diag=TRUE)] <- NA
      #   d.mat[d.mat > d.rm] <- NA
      #   # d.mat[d.mat < d.rm] <- TRUE
      #   d.mat <- as.data.frame(d.mat)
      #   
      #   
      #   
      #   # # names(d.mat) <- eo.spdf2$X.1
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
      