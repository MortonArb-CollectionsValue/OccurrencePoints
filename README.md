
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

 >INPUT: List of target taxa (target_taxa.csv; or create list by hand in script)
 >
 >OUTPUT: List of target taxa with acceptance, authors, and synonyms (target_taxa_with_syn.csv); see ["Taxonomic Output" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing) for table metadata
 >
 >NOTE: The functions in this script ('taxize' package) are slow and require manual input while running; therefore if your list is long, you may need to find synonyms a different way.

## 1-1_prepare_gis_data.R

 Adds GlobalTreeSearch (GTS) and IUCN Red List (RL) country-level distribution data to target taxa list and preps country (adm0), state/province (adm1), and county (adm2) polygons for later use.

 >INPUTS:<br>
 > ~ List of target taxa with synonyms (target_taxa_with_syn.csv)<br>
 > ~ [GlobalTreeSearch](https://tools.bgci.org/global_tree_search.php) country-level distribution data for target genera (globaltreesearch_country_distribution.csv)
 >
 >OUTPUTS:<br>
 > -- List of target taxa with native country distribution from GTS and IUCN RL added (target_taxa_with_native_dist.csv); RL also has some introduced country distribution data that is added<br>
 > -- RData file with country and state polygon data from 'rnaturalearthhires' package and manually downloaded U.S. county polygon data (admin_shapefiles.RData)

## 2-0_get_raw_occurrence_points.R

 Provides manual instructions and code chunks for downloading and standardizing occurrence points from a variety of online databases. Data from all sources can be pulled, or specific sources can be chosen individually.

 Global databases include:
 - Global Biodiversity Information Facility (GBIF) [auto download with 'rgbif']
 - Integrated Digitized Biocollections (iDigBio) [auto download with 'ridigbio', or manual download to make sure all fields are captured]
 - U.S. Herbarium Consortia (SERNEC, SEINet, etc.) [manual download]
 - Botanical Information and Ecology Network (BIEN) [auto download with 'BIEN']

 National databases include:
 - Forest Inventory and Analysis (FIA) Program, USDA Forest Service [auto download with raw files pulled from internet]
 - Biodiversity Information Serving Our Nation (BISON), USGS [auto download with 'rbison']

 >INPUTS:<br>
 > -- List of target taxa and synonyms (target_taxa_with_syn.csv)<br>
 > -- FIA metadata tables (FIA_AppendixF_TreeSpeciesCodes_2016.csv; US_state_county_FIPS_codes.csv)
 >
 >OUTPUT: Raw occurrence records for target taxa or genera (depending on how the database’s download works), one CSV for each database (e.g., gbif.csv); see ["Renaming Columns" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing) for schema used to rename columns and standardize data
 >
 >NOTES:<br>
 > -- Not all data from these sources are reliable and many have duplicates from one or more datasets. The aim of this script is to get all easily-downloadable, public occurrence data, which can then be sorted and vetted for the user's specific purposes.<br>
 > -- You can add other occurrence point data (e.g., expert comment, NatureServe, floras, USDA PLANTS, BONAP, IUCN Red List, private sources, etc.) by standardizing column names and formatting to match the schema in the ["Renaming Columns" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing), then save as CSV and place in "inputs/compiled_occurrence" folder.

## 2-1_compile_exsitu_data.R

 ! STILL IN DEVELOPMENT !

 Takes a folder of CSV files representing ex situ accessions data from different institutions, combines them into one dataset, and standardizes some important fields.

 >INPUTS:<br>
 > -- Folder of CSV files whose column names have be standardized by hand using the ["standardizing_accessions_data_fields"](https://docs.google.com/spreadsheets/d/1QLxxWu-bUIRcrjHiaWeSz9n1ms4EQB3yQ8P8PBBx3Zk/edit?usp=sharing) template <br>
 > -- List of target taxa and synonyms (target_taxa_with_syn.csv)
 >
 >OUTPUT: Ex situ accessions data compiled into one CSV (exsitu.csv), with some fields standardized: provenance type, number of individuals, latitude and longitude, collection/acquisition year (want to add some others eventually, like germplasm type)

  ?? Fix neg/pos longitude error ??

## 3-0_compile_raw_occurrence_points.R

 Compiles raw occurrence point data previously downloaded. Steps include:
 - Stack all data
 - Filter by target taxa
 - Standardize some key columns (year, basisOfRecord, establishmentMeans)
 - Check validity of latitude and longitude (can be plotted; not both equal to zero; not further than 0.01 decimal degree from land)
 - Separate out points with locality description only (no valid lat-long), which can later be geolocated manually, as desired
 - Standardize country code column to contain valid ISO3 values, for later analysis
 - Remove duplicates based on species name and lat-long rounded to 3 digits after decimal
 - Write a separate CSV of lat-long points for each target species

 >INPUTS:<br>
 > -- Raw occurrence point data from 2-0_get_raw_occurrence_points.R and (optionally) 2-1_compile_exsitu_data.R<br>
 > -- List of target taxa and synonyms (target_taxa_with_syn.csv)
 >
 >OUTPUTS:<br>
 > -- CSV of all occurrence points without lat-long but with locality description (need_geolocation.csv)<br>
 > -- CSV of occurrence points for each target species (e.g., Quercus_lobata.csv) in outputs/working/raw_split_by_sp<br>
 > -- Summary table with one row for each target species, listing number of points with valid a lat-long and number of points with locality description only (occurrence_point_count_per_sp.csv)

## 3-1_refine_occurrence_points.R

 Flags suspect points by adding a column for each type of flag, where FALSE = flagged. Most of the flagging is done through the 'CoordinateCleaner' package, which was created for "geographic cleaning of coordinates from biologic collections." Flag columns include:
 - .cen -> Flag records within 500m of country and province centroids
 - .urb -> Flag records inside urban areas (based on rnaturalearth ne_50m_urban_areas shapefile)
 - .inst -> Flag records within 100m of biodiversity institutions
 - .con -> Flag records outside their reported country
 - .outl -> Flag geographic outliers in species distribution (based on quantile method, with multiplier of the interquartile range = 5; must have at least 7 records to be tested)
 - .gtsnative -> Flag records in countries outside the species native range as reported in GlobalTreeSearch
 - .rlnative -> Flag records in countries outside the species native range as reported in the IUCN Red List
 - .rlintroduced -> Flag records in countries reported in the IUCN Red List as part of the species "introduced" range

 >INPUTS:<br>
 > -- Raw occurrence point data from 2-0_get_raw_occurrence_points.R and (optionally) 2-1_compile_exsitu_data.R<br>
 >
 >OUTPUTS:<br>
 > -- CSV of occurrence points for each target species (e.g., Quercus_lobata.csv) in outputs/working/raw_split_by_sp; see ["Occurrence Output" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing) for full output metadata<br>
 > --

## 5-0_plot_occurrence_raw_all.R

 ! STILL IN DEVELOPMENT !

 Creates occurrence point map for each species, for exploring

## X-0_Run_Point_Data.R

 ! STILL IN DEVELOPMENT !

 Runs the whole workflow from start to finish

# file structure

  **Folders/files that must be manually created or downloaded are bolded**

  - **occurrence_points**
    - **inputs**
      - **taxa_list**
        - **target_taxa.csv**
        - target_taxa_with_syn.csv
      - **known_distribution**
        - **globaltreesearch_country_distribution.csv**
        - target_taxa_with_native_dist.csv
      - **gis_data**
        - **USA_adm**
          - **USA_adm2.shp**, and associated files
        - geo_work0.xlsx
        - geo_work1.xlsx
        - geo_work2.xlsx
        - admin_shapefiles.RData
      - **fia_tables**
        - **FIA_AppendixF_TreeSpeciesCodes_2016.csv**
        - **US_state_county_FIPS_codes.csv**
        - PLOT.csv
      - raw_occurrence
        - gbif_raw
        - idigbio_raw
        - sernec_raw
          - **[[zipped download for each target genus]]**
        - bien_raw
        - fia_raw
        - bison_raw
        - exsitu_standard_column_names
      - compiled_occurrence
        - bien.csv
        - bison.csv
        - fia.csv
        - gbif.csv
        - idigbio.csv
        - sernec.csv
        - exsitu.csv
    - outputs
      - need_geolocation_[[YYYY-MM-DD]].csv
      - occurrence_point_count_per_species_[[YYYY-MM-DD]].csv
      - raw_split_by_sp
        - [[genus_species]].csv
      - flag_summary_by_sp_[[YYYY-MM-DD]].csv
      - spp_edited_points
        - [[genus_species}}.csv
      - spp_interactive_maps
        - [[genus_species]]_leaflet_map.html
      - spp_basic_maps
        - [[genus_species]]_raw.png
