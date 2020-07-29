
##  0-1_set_workingdirectory.R

 OVERVIEW: Sets the working environment based on the computer on which you're working. A new "else if" section needs to be manually added for each computer you're running on.

##  0-2_load_IMLS_functions.R

 OVERVIEW: Loads all of the functions to use for the project

## 1-0_get_taxonomic_info.R

 OVERVIEW: Takes a list of taxa and uses the taxize package to pull taxonomic information from multiple databases. The output can either be used directly in next script (2-0_get_raw_occurrence_points.R), or can be reviewed and revised manually (recommended). Information pulled includes:

    * Acceptance status and authors from:
      - Tropicos,
      - Integrated Taxonomic Information Service (ITIS)
      - Kew’s Plants of the World (POW)
      - The Plant List (TPL)

    * Synonyms from:
      - Tropicos
      - ITIS
      - POW

 INPUT: List of target taxa (target_taxa.csv)

 OUTPUT: List of target taxa with acceptance, authors, and synonyms (target_taxa_with_syn.csv)

## 2-0_get_raw_occurrence_points.R

 OVERVIEW: Provides manual instructions and code chunks for downloading and standardizing wild occurrence points from a variety of online databases. Data from all sources can be pulled, or specific sources can be chosen individually. Sources include:

    * Global databases (though all likely have U.S. bias?):
      - Global Biodiversity Information Facility (GBIF)
      - Integrated Digitized Biocollections (iDigBio)
      - U.S. Herbarium Consortia (SERNEC, SEINet, etc.)
      - Botanical Information and Ecology Network (BIEN)

    * National databases:
      - Forest Inventory and Analysis (FIA) Program, USDA Forest Service
      - Biodiversity Information Serving Our Nation (BISON), USGS

 NOTE: Not all data from these sources are reliable. The aim of this script is to get all easily-downloadable occurrence data, which can then be sorted and vetted for the user's specific purposes.

 INPUTS: List of target taxa and synonyms (target_taxa_with_syn.csv); FIA metadata tables (FIA_AppendixF_TreeSpeciesCodes_2016.csv, US_state_county_FIPS_codes.csv)

 OUTPUTS: Raw occurrence records for target taxa or genera (depending on how the database’s download works); one CSV for each database

## 2-1_compile_exsitu_data.R

 ! STILL IN DEVELOPMENT !

 OVERVIEW: This script takes a folder of CSV files representing ex situ accessions data from different institutions, combines them into one dataset, and standardizes some important fields.

 INPUTS: Folder of CSV files whose column names have be standardized by hand using the "standardizing_accessions_data_fields" template (https://docs.google.com/spreadsheets/d/1QLxxWu-bUIRcrjHiaWeSz9n1ms4EQB3yQ8P8PBBx3Zk/edit?usp=sharing); list of target taxa and synonyms (target_taxa_with_syn.csv) created through 1-0_get_taxonomic_info.R

 OUTPUTS: Ex situ accessions data compiled into one CSV, with some fields standardized: provenance type, number of individuals, latitude and longitude, collection/acquisition year (want to add some others eventually, like germplasm type)

## 3-0_compile_raw_occurrence_points.R

 ! STILL IN DEVELOPMENT ! ##

 OVERVIEW: Compiles raw occurrence point data
  - Filter by target taxa list
  - Standardize some key columns (year, basisOfRecord, establishmentMeans)
  - Separate out points with locality description only (no lat-long) and points in water
  - Remove duplicates based on species name and lat-long rounded to 3 digits after decimal
  - Write a CSV of lat-long points for each target species

 INPUTS: Raw occurrence point data from 2-0_get_raw_occurrence_points.R and 2-1_compile_exsitu_data.R; list of target taxa and synonyms

 OUTPUTS: CSV of occurrence points for each species; also a table of the number of lat-long, locality description only, and water points for each target species (occurrence_point_count_per_species.csv)

## 4-0_refine_raw_occurrence_points.R

 ! STILL IN DEVELOPMENT ! ##

 OVERVIEW: Keep points only in species "accepted" range, based on:
  - GlobalTreeSearch countries of distribution
  - IUCN Red List countries and states of distribution
  - BISON counties of distribution (U.S. only)

 ?? Fix neg/pos longitude error in ex situ data ??

## 5-0_plot_occurrence_raw_all.R

 ! STILL IN DEVELOPMENT ! ##

 OVERVIEW: Create occurrence point map for each species, for exploring

## X-0_Run_Point_Data.R

 ! STILL IN DEVELOPMENT ! ##

 OVERVIEW: Runs the whole workflow from start to finish

################################################################################

# file structure

  **Folders are bolded**

  Folders/files that must be manually created/downloaded are marked as such

  - **occurrence_points** (manually created)
    - ! **inputs**
    - **taxa_list** (manually created)
    - target_taxa_with_syn.csv (manually created)
    - occurrence_point_count_per_species.csv
    - **raw_occurrence_point_data** (manually created)
      - **gbif_read_in**
        - occurrence.txt
        - ...
      - **fia_read_in** (manually created)
        - AK_TREE.csv (manually downloaded)
        - ...
      - **sernec_read_in** (manually created)
        - occurrences.csv (manually downloaded)
        - ...
      - bien_raw.csv
      - fia_raw.csv
      - gbif_raw.csv
      - idigbio_raw.csv
      - sernec_raw.csv
    - **FIA_tables** (manually created)
      - FIA_AppendixF_TreeSpeciesCodes_2016.csv (manually downloaded)
      - PLOT.csv (manually downloaded)
      - US_state_county_FIPS_codes.csv (manually downloaded)
    - **raw_split_by_sp**
      - Malus_angustifolia.csv
      - ...


# standard columns

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
