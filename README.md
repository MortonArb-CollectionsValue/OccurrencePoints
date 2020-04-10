# Code for analyzing spatial data associated with the IMLS project

################################################################################
 1_get_taxonomic_info.R
################################################################################

 OVERVIEW: Takes a list of taxa and uses the taxize package to pull taxonomic
           information from multiple databases. The output can then be used to
           create a final list of target taxa and synonyms by hand. Data
           pulled includes:
             - Acceptance and authors from Tropicos, Integrated Taxonomic
                Information Service (ITIS), and The Plant List (TPL)
             - Authors from International Plant Names Index (IPNI) and
                Taxonomic Name Resolution Service (TNRS)
             - Synonyms from Tropicos and ITIS
 INPUTS: List of target taxa
 OUTPUTS: List of target taxa with acceptance, authors, and synonyms

################################################################################
# 2_get_raw_occurrence_points.R
################################################################################

# OVERVIEW: Provides manual instructions and code chunks for downloading and
#           standardizing wild occurrence points from:
#           * Global databases (though all likely have U.S. bias?):
#             - Global Biodiversity Information Facility (GBIF)
#             - Integrated Digitized Biocollections (iDigBio)
#             - U.S. Herbarium Consortia (SERNEC, SEINet, etc.)
#             - Botanical Information and Ecology Network (BIEN)
#           * National databases:
#             - Forest Inventory and Analysis (FIA) Program, USDA Forest Service
# INPUTS: - List of target taxa, including synonyms and their accepted names
#             (target_taxa_with_syn.csv)
#         - FIA codes and species names (FIA_AppendixF_TreeSpeciesCodes_2016.csv)
#         - county and state codes (US_state_county_FIPS_codes.csv)
# OUTPUTS: Raw occurrence records for target taxa or genera (depending on how
#          the database’s download works), one CSV for each database

################################################################################
# 3_compile_raw_occurrence_points.R
################################################################################

# OVERVIEW: Compiles raw occurrence point data downloaded in previous script
#             -
# INPUTS:
# OUTPUTS:

# columns
  # taxon_name      
  # scientificName
  # taxonIdentificationNotes : concatenated columns regarding taxon ID,
  #    separated by "|", including:
  #    "identificationRemarks","identificationVerificationStatus",
  #    "identifiedBy","taxonRemarks"
  # decimalLatitude             
  # decimalLongitude
  # coordinateUncertaintyInMeters
  # basisOfRecord : "FOSSIL_SPECIMEN","HUMAN_OBSERVATION","LITERATURE"     
  #    "LIVING_SPECIMEN","MACHINE_OBSERVATION","MATERIAL_SAMPLE"
  #    "OBSERVATION","PRESERVED_SPECIMEN","UNKNOWN"
  # year
  # nativeDatabaseID : gbifID, uuid(idigbio), id(sernec), record_number(bien),
  #    plot key(fia; "INVYR","UNITCD","COUNTYCD","PLOT","STATECD")
  # references
  # localityDescription : concatenated locality columns, separated by "|",
  #    including:
  #    "locality","verbatiumLocality","county","municipality",
  #    "stateProvince","higherGeography","country","countryCode"
  # locationNotes : other locality information, including
  #    "associatedTaxa","eventRemarks","fieldNotes","habitat",
  #    "locationRemarks","occurrenceRemarks","occurrenceStatus"
  # geolocationNotes : any information about how the record was geolocated,
  #    including "georeferencedBy","georeferencedDate","georeferenceProtocol",
  #    "georeferenceRemarks","georeferenceSources",
  #    "georeferenceVerificationStatus",
  # datasetName
  # ?publisher
  # establishmentMeans : "DEAD","INTRODUCED","INVASIVE","MANAGED","NATIVE","UNKNOWN"
  # informationWithheld
  # issue
  # database                 
  # species_name

> percent.filled(gbif_raw)
[1] "taxon_name: 100%"
[1] "scientificName: 100%"
[1] "taxonIdentificationNotes: 46.6%"
[1] "decimalLatitude: 89.6%"
[1] "decimalLongitude: 89.6%"
[1] "coordinateUncertaintyInMeters: 66.8%"
[1] "basisOfRecord: 100%"
[1] "year: 77.4%"
[1] "nativeDatabaseID: 100%"
[1] "references: 8.4%"
[1] "locality: 35.7%"
[1] "verbatimLocality: 4.7%"
[1] "county: 30.6%"
[1] "municipality: 23.8%"
[1] "stateProvince: 26.7%"
[1] "higherGeography: 2.6%"
[1] "countryCode: 97.8%"
[1] "locationNotes: 54.3%"
[1] "geolocationNotes: 3.4%"
[1] "datasetName: 100%"
[1] "publisher: 100%"
[1] "establishmentMeans: 0.6%"
[1] "informationWithheld: 6.7%"
[1] "issue: 73.3%"
[1] "database: 100%"
[1] "species_name: 100%"

> percent.filled(idigbio_raw)
[1] "taxon_name: 100%"
[1] "scientificName: 100%"
[1] "decimalLongitude: 32.2%"
[1] "decimalLatitude: 32.2%"
[1] "coordinateUncertaintyInMeters: 17.5%"
[1] "basisOfRecord: 100%"
[1] "year: 58.6%"
[1] "nativeDatabaseID: 100%"
[1] "references: 100%"
[1] "locality: 71.7%"
[1] "verbatimLocality: 3.4%"
[1] "county: 79.8%"
[1] "municipality: 6.3%"
[1] "stateProvince: 95%"
[1] "country: 98.7%"
[1] "countryCode: 98.1%"
[1] "datasetName: 49.8%"
[1] "database: 100%"
[1] "species_name: 100%"

> percent.filled(sernec_raw)
[1] "taxon_name: 100%"
[1] "scientificName: 100%"
[1] "taxonIdentificationNotes: 26.6%"
[1] "coordinateUncertaintyInMeters: 12.7%"
[1] "decimalLatitude: 27.3%"
[1] "decimalLongitude: 27.3%"
[1] "basisOfRecord: 100%"
[1] "year: 65.3%"
[1] "nativeDatabaseID: 100%"
[1] "references: 100%"
[1] "locality: 64.6%"
[1] "county: 77.9%"
[1] "municipality: 5.4%"
[1] "stateProvince: 91.8%"
[1] "country: 94%"
[1] "locationNotes: 45.2%"
[1] "geolocationNotes: 14.2%"
[1] "datasetName: 100%"
[1] "establishmentMeans: 0.6%"
[1] "informationWithheld: 2%"
[1] "database: 100%"
[1] "species_name: 100%"

> percent.filled(bien_raw)
[1] "taxon_name: 100%"
[1] "family: 100%"
[1] "genus: 100%"
[1] "scientificName: 100%"
[1] "taxonIdentificationNotes: 100%"
[1] "decimalLongitude: 99.9%"
[1] "decimalLatitude: 99.9%"
[1] "basisOfRecord: 100%"
[1] "year: 99.4%"
[1] "nativeDatabaseID: 94%"
[1] "locality: 7.3%"
[1] "county: 97%"
[1] "state_province: 98.9%"
[1] "country: 100%"
[1] "datasetName: 100%"
[1] "publisher: 100%"
[1] "database: 100%"
[1] "species_name: 100%"
[1] "establishmentMeans: 100%"

> percent.filled(fia_raw)
[1] "taxon_name: 100%"
[1] "decimalLongitude: 98.7%"
[1] "decimalLatitude: 98.7%"
[1] "year: 100%"
[1] "nativeDatabaseID: 100%"
[1] "database: 100%"
[1] "species_name: 100%"
[1] "establishmentMeans: 100%"
[1] "basisOfRecord: 100%"
