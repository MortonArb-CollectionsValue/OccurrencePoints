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

 INPUTS: List of target taxa, including synonyms and their accepted names
             (target_taxa_with_syn.csv), FIA codes and species names (FIA_AppendixF_TreeSpeciesCodes_2016.csv), county and state codes (US_state_county_FIPS_codes.csv)

 OUTPUTS: Raw occurrence records for target taxa or genera (depending on how
          the database’s download works), one CSV for each database

################################################################################
 3_compile_raw_occurrence_points.R
################################################################################

 OVERVIEW: Compiles raw occurrence point data downloaded in previous script
             -
 INPUTS:
 OUTPUTS:


# standard columns
  - taxon_name      
  - scientificName
  - taxonIdentificationNotes : concatenated columns regarding taxon ID,
      separated by "|", including:
      "identificationRemarks","identificationVerificationStatus",
      "identifiedBy","taxonRemarks"
  - decimalLatitude             
  -  decimalLongitude
  -  coordinateUncertaintyInMeters
  -  basisOfRecord : "FOSSIL_SPECIMEN","HUMAN_OBSERVATION","LITERATURE"     
      "LIVING_SPECIMEN","MACHINE_OBSERVATION","MATERIAL_SAMPLE"
      "OBSERVATION","PRESERVED_SPECIMEN","UNKNOWN"
  -  year
  -  nativeDatabaseID : gbifID, uuid(idigbio), id(sernec), record_number(bien),
      plot key(fia; "INVYR","UNITCD","COUNTYCD","PLOT","STATECD")
  -  references
  -  localityDescription : concatenated locality columns, separated by "|",
      including:
      "locality","verbatiumLocality","county","municipality",
      "stateProvince","higherGeography","country","countryCode"
  -  locationNotes : other locality information, including
      "associatedTaxa","eventRemarks","fieldNotes","habitat",
      "locationRemarks","occurrenceRemarks","occurrenceStatus"
  -  geolocationNotes : any information about how the record was geolocated,
      including "georeferencedBy","georeferencedDate","georeferenceProtocol",
      "georeferenceRemarks","georeferenceSources",
      "georeferenceVerificationStatus",
  -  datasetName
  -  publisher
  -  establishmentMeans : "DEAD","INTRODUCED","INVASIVE","MANAGED","NATIVE","UNKNOWN"
  -  informationWithheld
  -  issue
  -  database                 
  -  species_name
