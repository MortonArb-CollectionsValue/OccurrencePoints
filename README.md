# Code for analyzing spatial data associated with the IMLS project

################################################################################
 1_get_taxonomic_info.R
################################################################################

 OVERVIEW: Takes a list of taxa and uses the taxize package to pull taxonomic
           information from multiple databases. The output can then be used to
           create a final list of target taxa and synonyms by hand.

           Data pulled includes:

             - Acceptance and authors from Tropicos, Integrated Taxonomic
                Information Service (ITIS), and The Plant List (TPL)
             - Authors from International Plant Names Index (IPNI) and
                Taxonomic Name Resolution Service (TNRS)
             - Synonyms from Tropicos and ITIS

 INPUTS: List of target taxa

 OUTPUTS: List of target taxa with acceptance, authors, and synonyms

################################################################################
 2_get_raw_occurrence_points.R
################################################################################

 OVERVIEW: Provides manual instructions and code chunks for downloading and
           standardizing wild occurrence points from:

           * Global databases (though all likely have U.S. bias?):

             - Global Biodiversity Information Facility (GBIF)
             - Integrated Digitized Biocollections (iDigBio)
             - U.S. Herbarium Consortia (SERNEC, SEINet, etc.)
             - Botanical Information and Ecology Network (BIEN)

           * National databases:

             - Forest Inventory and Analysis (FIA) Program, USDA Forest Service

 INPUTS: List of target taxa, including:
          - synonyms & their accepted names (target_taxa_with_syn.csv),
          - FIA codes & sp. names (FIA_AppendixF_TreeSpeciesCodes_2016.csv),
          - county and state codes (US_state_county_FIPS_codes.csv),
          - FIA plot data (PLOT.csv)

 OUTPUTS: Raw occurrence records for target taxa or genera (depending on how
          the database’s download works); one CSV for each database

################################################################################
 3_compile_raw_occurrence_points.R
################################################################################

 OVERVIEW: Compiles raw occurrence point data downloaded in
           2_get_raw_occurrence_points.R:

             - stack all data
             - filter by target taxa
             - standardize key columns (localityDescription, year,
               basisOfRecord, establishmentMeans)
             - check validity of lat and long (can be plotted); if invalid,
               switch and check again; set to NA if still invalid
             - create subsets for localityDescription only and lat-long points
             - remove duplicates by species_name_acc, lat_round, long_round,
               and establishmentMeans
             - mark rows with lat-long matching U.S. county centroids to 2
               places after the decimal
             - split into separate CSVs by species_name_acc

 INPUTS: raw datasets from 2_get_raw_occurrence_points.R; list of target taxa

 OUTPUTS: "occurrence_point_count_per_species.csv"; CSV of points for each species

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

# file structure

  **Folders are bolded**

  Folders/files that were manually created/downloaded are marked as such

  - **insitu_occurrence_points** (manually created)
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
