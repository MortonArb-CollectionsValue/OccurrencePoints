
## This workflow is meant for downloading and cleaning occurrence point data for target species. The following gives an overview of each script in the workflow.

## 0-1_set_workingdirectory.R

 Sets the working environment based on the computer on which you're working.

 >INPUTS: User needs to update file paths based on specific computer and setup

## 1-0_get_taxonomic_info.R

 Takes a list of taxa and uses the 'taxize' package to pull taxonomic information from multiple databases. The output can either be used directly in the following scripts, or can be reviewed and revised manually (recommended). Information pulled includes:

 Acceptance status and authors from:
 - Tropicos
 - Integrated Taxonomic Information Service (ITIS)
 - Kew’s Plants of the World (POW)
 - The Plant List (TPL)

 Synonyms from:
 - Tropicos
 - ITIS
 - POW

 >INPUTS:
 > -- List of target taxa (target_taxa.csv, or create list by hand in script)
 >
 >OUTPUTS:
 > * List of target taxa with acceptance, authors, and synonyms (target_taxa_with_syn.csv)
 >
 >NOTE: The functions in this script ('taxize' package) are slow and require manual input while running; therefore if your list is long, you may need to find synonyms a different way.

## 2-0_get_raw_occurrence_points.R

 Provides manual instructions and code chunks for downloading and standardizing occurrence points from a variety of online databases. Data from all sources can be pulled, or specific sources can be chosen individually.

 Global databases (though all likely have U.S. bias?) include:
 - Global Biodiversity Information Facility (GBIF)
 - Integrated Digitized Biocollections (iDigBio)
 - U.S. Herbarium Consortia (SERNEC, SEINet, etc.)
 - Botanical Information and Ecology Network (BIEN)

 National databases include:
 - Forest Inventory and Analysis (FIA) Program, USDA Forest Service
 - Biodiversity Information Serving Our Nation (BISON), USGS

 >INPUTS:
 > - List of target taxa and synonyms (target_taxa_with_syn.csv)
 > - FIA metadata tables (FIA_AppendixF_TreeSpeciesCodes_2016.csv; US_state_county_FIPS_codes.csv)
 >
 >OUTPUTS: Raw occurrence records for target taxa or genera (depending on how the database’s download works), one CSV for each database (e.g., gbif.csv)
 >
 >NOTE: Not all data from these sources are reliable and many have duplicates from one or more datasets. The aim of this script is to get all easily-downloadable, public occurrence data, which can then be sorted and vetted for the user's specific purposes.

## 2-1_compile_exsitu_data.R

 ! STILL IN DEVELOPMENT !

 This script takes a folder of CSV files representing ex situ accessions data from different institutions, combines them into one dataset, and standardizes some important fields.

 >INPUTS:
 > - Folder of CSV files whose column names have be standardized by hand using the "standardizing_accessions_data_fields" template (https://docs.google.com/spreadsheets/d/1QLxxWu-bUIRcrjHiaWeSz9n1ms4EQB3yQ8P8PBBx3Zk/edit?usp=sharing)
 > - List of target taxa and synonyms (target_taxa_with_syn.csv)
 >
 >OUTPUTS: Ex situ accessions data compiled into one CSV, with some fields standardized: provenance type, number of individuals, latitude and longitude, collection/acquisition year (want to add some others eventually, like germplasm type)

## 3-0_compile_raw_occurrence_points.R

 Compiles raw occurrence point data previously downloaded.

 Steps include:
 - Stack all data
 - Filter by target taxa
 - Standardize some key columns (year, basisOfRecord, establishmentMeans)
 - Check validity of latitude and longitude (can be plotted; not both equal to zero; not further than 0.01 decimal degree from land)
 - Separate out points with locality description only (no valid lat-long), which can later be geolocated manually, as desired
 - Standardize country code column to contain valid ISO3 values, for later analysis
 - Remove duplicates based on species name and lat-long rounded to 3 digits after decimal
 - Write a separate CSV of lat-long points for each target species

 >INPUTS: Raw occurrence point data from 2-0_get_raw_occurrence_points.R and (optionally) 2-1_compile_exsitu_data.R; list of target taxa and synonyms (target_taxa_with_syn.csv)
 >
 >OUTPUTS: CSV of occurrence points for each target species (e.g., Quercus_lobata.csv); summary table with one row for each target species and two columns: 1) number of points with valid a lat-long 2) number of points with locality description only (occurrence_point_count_per_sp.csv)

## 3-1_prepare_gis_data.R

 Add GlobalTreeSearch and IUCN Red List country-level distribution data to target taxa list and prep country (adm0), state/province (adm1), and county (adm2) polygons for later use.

 >INPUTS:
  - List of target taxa with synonyms (target_taxa_with_syn.csv)
  - GlobalTreeSearch country-level distribution data for each target species, downloaded from https://tools.bgci.org/global_tree_search.php

 OUTPUTS:
  - List of target taxa native country distribution from GTS and IUCN RL added (target_taxa_with_syn_and_dist.csv); RL also has some introduced country distribution data that is added
  - adm0.poly (countries shapefile)
  - adm1.poly (state-level shapefile)
  - adm2.poly (county-level shapefile)

## 4-0_refine_raw_occurrence_points.R

 ! STILL IN DEVELOPMENT !

 OVERVIEW: Keep points only in species "accepted" range, based on:
  - GlobalTreeSearch countries of distribution
  - IUCN Red List countries and states of distribution
  - BISON counties of distribution (U.S. only)

 ?? Fix neg/pos longitude error in ex situ data ??

## 5-0_plot_occurrence_raw_all.R

 ! STILL IN DEVELOPMENT !

 OVERVIEW: Create occurrence point map for each species, for exploring

## X-0_Run_Point_Data.R

 ! STILL IN DEVELOPMENT !

 OVERVIEW: Runs the whole workflow from start to finish



## file structure

 ! STILL IN DEVELOPMENT !

  **Folders/files that must be manually created/downloaded are bolded**

  - **occurrence_points**
    - **inputs**
      - compiled_occurrence
        - bien.csv
        - bison.csv
        - fia.csv
        - gbif.csv
        - idigbio.csv
        - sernec.csv
        - exsitu.csv
      - **fia_tables**
        - **FIA_AppendixF_TreeSpeciesCodes_2016.csv**
        - **US_state_county_FIPS_codes.csv**
        - PLOT.csv
      - **gis_data**
        - datum_walkover.txt
        - gadm.xlsx
        - geo_work0.xlsx
        - geo_work1.xlsx
        - geo_work2.xlsx
        - global_admin_areas.xlsx
        - IMLS_GIS_data.RData
        - imls_global_admin_areas.xlsx
        - usa
          - **USA_adm**
            - **info**
              - **arc.dir**
            - **USA_adm0.dbf**
            - **USA_adm0.prj**
            - **USA_adm0.sbn**
            - **USA_adm0.sbx**
            - **USA_adm0.shp**
            - **USA_adm0.shx**
            - **USA_adm1.dbf**
            - **USA_adm1.prj**
            - **USA_adm1.sbn**
            - **USA_adm1.sbx**
            - **USA_adm1.shp**
            - **USA_adm1.shx**
            - **USA_adm2.dbf**
            - **USA_adm2.prj**
            - **USA_adm2.sbn**
            - **USA_adm2.sbx**
            - **USA_adm2.shp**
            - **USA_adm2.shx**
            - **USA_readme.txt**
          -USA_Counties_continental2.dbf
          -USA_Counties_continental2.prj
          -USA_Counties_continental2.sbn
          -USA_Counties_continental2.sbx
          -USA_Counties_continental2.shp
          -USA_Counties_continental2.shx
        - world
          -TM_WORLD_BORDERS-0.3
            -Readme.txt
            -TM_WORLD_BORDERS-0.3.dbf
            -TM_WORLD_BORDERS-0.3.prj
            -TM_WORLD_BORDERS-0.3.shp
            -TM_WORLD_BORDERS-0.3.shx
      - **IMLS_headers.xlsx**
      - **known_distribution**
        - **exsitu_standard_column_names**
      - raw_occurrence
        - gbif_raw
        - idigbio_raw
        - sernec_raw
        - bien_raw
        - fia_raw
        - bison_raw
      - **taxa_list**
        - **target_taxa.csv**
        - target_taxa_with_syn.csv
    - outputs
      - working
        - occurrence_point_count_per_species_<<YYYY-MM-DD>>.csv
        - records_to_examine
          - no_taxon_match_<<YYYY-MM-DD>>.csv
          - need_geolocation_<<YYYY-MM-DD>>.csv
          - not_on_land_<<YYYY-MM-DD>>.csv
          - water_points_<<YYYY-MM-DD>>.csv
        - split_by_sp
          - <<taxon>>.csv
        - interactive_maps_split_by_sp
          - <<taxon>>_leaflet_map_files
            - htmlwidgets-1.5.1
            - jquery-1.12.4
            - leaflet-1.3.1
            - leaflet-binding-2.0.3
            - leaflet-providers-1.9.0
            - leaflet-providers-plugin-2.0.3
            - leafletfix-1.0.0
            - Proj4Leaflet-1.0.1
              - proj4-compressed.js
              - proj4leaflet.js
            - rstudio_leaflet-1.3.1
              - images
                - 1px.png
              - rstudio_leaflet.css
          - <<taxon>>_leaflet_map.html
        - basic_maps_split_by_sp
          - <<taxon>>_raw.png
      - final
        - split_by_sp_final


## standard columns

 ! NEEDS TO BE UPDATED !

  - species_name_acc : accepted species name (from target_taxa_with_syn.csv)
  - taxon_name      
  - scientificName
  - taxonIdentificationNotes : concatenated columns regarding taxon ID, separated by "|", including whichever are available: "identificationRemarks", "identificationVerificationStatus", "identifiedBy", "taxonRemarks"

  - database : "GBIF", "iDigBio", "US_Herbaria", "BIEN", "FIA"  
  - year : 1500 to 2020; none = NA
  - basisOfRecord : "FOSSIL_SPECIMEN", "HUMAN_OBSERVATION", "LITERATURE",   "LIVING_SPECIMEN", "MACHINE_OBSERVATION", "MATERIAL_SAMPLE", "OBSERVATION", "PRESERVED_SPECIMEN", "UNKNOWN"
  - establishmentMeans : "DEAD", "INTRODUCED", "INVASIVE", "MANAGED", "NATIVE", "UNKNOWN"

  - decimalLatitude             
  - decimalLongitude
  - coordinateUncertaintyInMeters
  - geolocationNotes : information about how the record was geolocated, including whichever are available: "georeferencedBy", "georeferenceProtocol", "georeferenceRemarks", "georeferenceSources", "georeferenceVerificationStatus"
  - localityDescription : concatenated locality columns, separated by "|", including: "locality", "verbatiumLocality", "county", "municipality", "higherGeography", "stateProvince", "country", "countryCode"
  - locationNotes : other locality information, including whichever are available: "associatedTaxa", "eventRemarks", "fieldNotes", "habitat", "locationRemarks", "occurrenceRemarks", "occurrenceStatus"

  - datasetName : datasetName(gbif), institutionCode(idigbio,sernec), dataset(bien)
  - publisher : publisher(gbif), datasource(bien)  
  - nativeDatabaseID : gbifID(gbif), uuid(idigbio), id(sernec), record_number(bien), plot key(fia; "INVYR", "UNITCD", "COUNTYCD", "PLOT", "STATECD")
  - references : references(gbif,sernec), occurrenceid(idigbio)

  - source_databases : all databases with duplicate record, based on species_name_acc, lat_round, long_round, and establishmentMeans
  - informationWithheld : gbif and sernec only
  - issue : gbif only

  - taxon_name_full : true taxon name when record was matched to target taxa by species name
  - list : distinguishes between records matched directly to target taxa versus those matched to synonyms provided; "desiderata" or "synonym"
  - lat_round : latitude rounded to 3 decimal places
  - long_round : longitude rounded to 3 decimal places
  - FIPS : code for US county with centroid matching to 2 places after decimal


 -coords_error : Whether the coordinates can be used.
 -occ_flag : Coordinates flagged for one of a few reasons:
                Given coordinates are in water
                Check record for proximity to country/adm0 centroid (within 1000 meters).
                Check record for proximity to state/adm1 centroid (within 1000 meters).
                Check record for proximity to county/adm2 centroid (within 1000 meters).

 -adm0_match1 : Global Tree Search listed country of distribution does not match the coordinates provided by the dataset
 -adm0_match2 : listed country (adm0) of distribution does not match the coordinates provided by the dataset
 -adm0_match3 : Global Tree Search listed country of distribution does not match the country (adm0) provided by the dataset
 -adm1_match1 : (don't currently have) Global Tree Search listed state/province (adm1) of distribution does not match the coordinates provided by the dataset
 -adm1_match2 : listed state/province (adm1) of distribution does not match the coordinates provided by the dataset
 -adm1_match3 : (don't currently have) Global Tree Search listed county/parish (adm2) of distribution does not match the state/province (adm1) provided by the dataset
 -adm2_match1 : (don't currently have) Global Tree Search listed county/parish (adm2) of distribution does not match the coordinates provided by the dataset
 -adm2_match2 : listed state/province (adm1) of distribution does not match the coordinates provided by the dataset
 -adm2_match3 : (don't currently have) Global Tree Search listed county/parish (adm2) of distribution does not match the county/parish (adm2) provided by the dataset


### Still to come ###
IMLS_headers.xlsx : This file is used to help standardiuze column/field names. However, it is not currently in use.
  -
