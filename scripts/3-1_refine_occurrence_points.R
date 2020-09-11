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
  "data.table", "textclean", "CoordinateCleaner", "countrycode", "usmap",
  "RColorBrewer", "leaflet")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

################################################################################
# Set working directory
################################################################################

source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
#source("scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################

#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))


################################################################################
################################################################################
# 1. Read in data
################################################################################

# bring in data (load from saved .RData file)
load(file.path(main_dir, "inputs", "gis_data", "admin_shapefiles.RData"))

# create new folder for revised points, if not already existing
out.fld.nm <- "spp_edited_points"
if(dir.exists(file.path(main_dir, "outputs", out.fld.nm)))
  print("directory already created") else dir.create(file.path(main_dir,
    "outputs", out.fld.nm), recursive=TRUE)

################################################################################
# 2. Iterate through species files and flag suspect points
################################################################################

# list of species files to iterate
all.spp.files <- list.files(path=file.path(main_dir, "outputs", "working",
  "raw_split_by_sp"), ignore.case=FALSE, full.names=FALSE, recursive=TRUE)
#all.spp.files <- all.spp.files[1:5]
spp_list <- file_path_sans_ext(all.spp.files)

# start a table to add summary of results for each species
summary_tbl <- data.frame(species_name_acc = "start", total_pts = "start",
  .cen = "start", .urb = "start", .inst = "start", .con = "start",
  .outl = "start", .gtsnative = "start", .rlnative = "start",
  .rl_introduced = "start", stringsAsFactors=F)

  # header/column name order and selection
  c.nms <- c("species_name_acc", "taxon_name", "scientificName",
    "taxonIdentificationNotes", "database", "all_source_databases", "year",
    "basisOfRecord", "establishmentMeans","decimalLatitude", "decimalLongitude",
    "coordinateUncertaintyInMeters", "geolocationNotes", "localityDescription",
    "county", "stateProvince", "countryCode_standard",
    "datasetName", "publisher", "nativeDatabaseID", "references",
    "informationWithheld", "issue", "taxon_name_full", "list", "UID",
    "country.name", "country.iso_a2", "country.iso_a3", "country.continent",
    ".cen",".urb",".inst",".con",".outl",".gtsnative",".rlnative",
    ".rlintroduced")

# iterate through each species file to flag suspect points
cat("Starting ", "target ", "taxa (", length(spp_list), " total)", ".\n\n",
  sep="")

for (i in 1:length(spp_list)){
  f.nm <- spp_list[i]

  # bring in records
  eo.df <- read.csv(file.path(main_dir, "outputs", "working", "raw_split_by_sp",
    paste0(f.nm, ".csv")))

  if(nrow(eo.df)<2){
    cat("Skipping ", f.nm, ", ", i, " of ", length(spp_list), ", because less than 2 rows.\n\n", sep="")
  } else {

    # create SpatialPointsDataFrame for species
    proj4string4poly <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    eo.spdf <- SpatialPointsDataFrame(eo.df[,c("decimalLongitude",
      "decimalLatitude")], eo.df, proj4string = CRS(proj4string4poly))
    ## add country polygon data to each point based on lat-long location
    eo.post <- point.in.poly(eo.spdf, adm0.poly, sp=TRUE)@data

    ## CHECK POINT LOCATION AGAINST "ACCEPTED" COUNTRY DISTRUBUTION
    ## GlobalTreeSearch
    # species native country distribution list from GTS
    s.nd.gts.l <- unique(unlist(strsplit(native_dist$gts_native_dist_iso2c[
      native_dist$species_name_acc==gsub("_"," ", f.nm)], "; ")))
    if(!is.na(s.nd.gts.l)){
    ## flag records where GTS country doesn't match record's coordinate location
    eo.post <- eo.post %>% mutate(.gtsnative=(ifelse(
      country.iso_a2 %in% s.nd.gts.l, TRUE, FALSE)))
    } else {
      eo.post$.gtsnative <- NA
    }
    ## IUCN Red List native
    # species native country distribution list from RL
    s.nd.rln.l <- unique(unlist(strsplit(native_dist$rl_native_dist_iso2c[
      native_dist$species_name_acc==gsub("_"," ", f.nm)], "; ")))
    if(!is.na(s.nd.rln.l)){
    ## flag records where RL country doesn't match record's coordinate location
    eo.post <- eo.post %>% mutate(.rlnative=(ifelse(
      country.iso_a2 %in% s.nd.rln.l, TRUE, FALSE)))
    } else {
      eo.post$.rlnative <- NA
    }
    ## IUCN Red List introduced
    # species introduced country distribution list from RL
    s.nd.rli.l <- unique(unlist(strsplit(native_dist$rl_introduced_dist_iso2c[
      native_dist$species_name_acc==gsub("_"," ", f.nm)], "; ")))
    if(!is.na(s.nd.rli.l)){
    ## flag records where RL introduced country does match record's coord location
    eo.post <- eo.post %>% mutate(.rlintroduced=(ifelse(
      country.iso_a2 %in% s.nd.rli.l, FALSE, TRUE)))
    } else {
      eo.post$.rlintroduced <- NA
    }

    ## SERIES OF VETTED TESTS FROM CoordinateCleaner PACKAGE
    # Geographic Cleaning of Coordinates from Biologic Collections
    # https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13152
    #   Cleaning geographic coordinates by multiple empirical tests to flag
    #     potentially erroneous coordinates, addressing issues common in
    #     biological collection databases.
    ## tests included:
    # cc_cen -> Identify Coordinates in Vicinity of Country and Province Centroids
    # cc_inst -> Identify Records in the Vicinity of Biodiversity Institutions
    # cc_urb -> Identify Records Inside Urban Areas
    ## other test not included but could add:
    # cc_iucn -> Identify Records Outside Natural Ranges
    eo.post2 <- clean_coordinates(eo.post,
      lon = "decimalLongitude",
      lat = "decimalLatitude",
      species = "species_name_acc",
      centroids_rad = 500, # radius around capital coords (meters); default=1000
      inst_rad = 100, # radius around biodiversity institutions coord (meters)
      tests = c("centroids","institutions","urban")#"outliers","countries"
    )
    # for some reason the "sea" flag isn't working in the above function...
    #    adding here separately
    # actually, found it flags a lot on islands, etc. might skip for now
    #   flag_sea <- cc_sea(eo.post,lon = "decimalLongitude",lat = "decimalLatitude",
    #      value = "flagged")
    #   eo.post2$.sea <- flag_sea
    # for some reason the outlier section won't work when part of
    #   "clean_coordinates" function above so adding it here
    eo.post2 <- as.data.frame(eo.post2)
    flag_outl <- cc_outl(eo.post2,
        lon = "decimalLongitude",
        lat = "decimalLatitude",species = "species_name_acc",
        method = "quantile", mltpl = 5, value = "flagged")
    eo.post2$.outl <- flag_outl
    # check if given country matches lat-long country (CoordinateCleaner
    #   has something like this but also flags when NA? Didn't love that)
    eo.post2 <- eo.post2 %>% mutate(.con=(ifelse(
      (as.character(country.iso_a3) == as.character(countryCode_standard) &
      !is.na(country.iso_a3) & !is.na(countryCode_standard)) |
      is.na(country.iso_a3) | is.na(countryCode_standard), TRUE, FALSE)))

    # set column order and remove a few unnecessary columns
    eo.post3 <- eo.post2 %>% dplyr::select(all_of(c.nms))

    # check
    #tail(eo.post3)
    #str(eo.post3)

    # add to summary table
    summary_add <- data.frame(
      species_name_acc = spp_list[i],
      total_pts = nrow(eo.post3),
      .cen = sum(!eo.post3$.cen),
      .urb = sum(!eo.post3$.urb),
      .inst = sum(!eo.post3$.inst),
      .con = sum(!eo.post3$.con),
      .outl = sum(!eo.post3$.outl),
      .gtsnative = sum(!eo.post3$.gtsnative),
      .rlnative = sum(!eo.post3$.rlnative),
      .rlintroduced = sum(!eo.post3$.rlintroduced),
      stringsAsFactors=F)
    summary_tbl[i,] <- summary_add

    # WRITE NEW FILE
    write.csv(eo.post3, file.path(main_dir, "outputs", out.fld.nm,
      paste0(f.nm, ".csv")), row.names=FALSE)

    cat("Ending ", f.nm, ", ", i, " of ", length(spp_list), ".\n\n", sep="")
    #rm(f.nm, s.nd.gts, s.nd.gts.l, s.nd.rln.l, s.nd.rli.l)
  }
}

# write summary table
write.csv(summary, file.path(main_dir,"outputs",
  paste0("flag_summary_by_sp_", Sys.Date(), ".csv")),row.names = F)
