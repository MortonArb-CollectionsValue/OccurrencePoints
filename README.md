
## This workflow is meant for downloading and cleaning occurrence point data for target species. The following outline gives an overview of each script.
### Final output is a folder of "spp_edited_points" with CSV of occurrence points for each target species; a variety of standardized columns are included for sorting and filtering, based on specific project goals (see 3-1 below for details). An interactive map (HTML) for each target species is located in "spp_interactive_maps" folder, and can be used for visualizing standard columns and flags.

## 0-1_set_workingdirectory.R

 Sets the working environment based on the computer on which you're working.

 >INPUTS: User needs to update file paths based on specific computer and setup

## 1-0_get_taxonomic_info.R

 Takes a list of taxa and uses the 'taxize' package to pull taxonomic information from multiple databases. The output can either be used directly in the following scripts, or can be reviewed and revised manually based on expert knowledge of the taxa (not all synonyms pulled are agreeable, depending on your taxonomic viewpoint). Taxonomic information can also be added manually to the final list, for taxa with no match in the databases searched. Information pulled includes:

 Acceptance status and authors from:
 - Tropicos
 - Integrated Taxonomic Information Service (ITIS)
 - Kew’s Plants of the World (POW)
 - The Plant List (TPL)

 Synonyms from:
 - Tropicos
 - ITIS
 - POW

 >INPUTS:<br>
 > ~ List of target taxa (target_taxa.csv with one column containing target taxa tames; or create list by hand in script)
 > ~ Must manually add list of target families (line 80 in script)
 >
 >OUTPUTS:<br>
 > ~ List of target taxa with acceptance, authors, and synonyms (target_taxa_with_syn.csv); synonyms are added as additional rows; see ["Taxonomic Output" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing) for table metadata
 >
 >NOTE: The functions in this script ('taxize' package) are slow and require manual input while running; therefore if your list is more than a few hundred, you may need to find synonyms a different way.

## 1-1_prepare_gis_data.R

 Adds GlobalTreeSearch (GTS) and IUCN Red List (RL) country-level distribution data to target taxa list and preps country (adm0), state/province (adm1), and county (adm2) polygons for later use.

 >INPUTS:<br>
 > ~ List of target taxa with synonyms (target_taxa_with_syn.csv)<br>
 > ~ [GlobalTreeSearch](https://tools.bgci.org/global_tree_search.php) country-level distribution data for target genera (globaltreesearch_country_distribution.csv); must download data for each genus then compile manually<br>
 >
 >OUTPUTS:<br>
 > ~ List of target taxa with native country distribution from GTS and IUCN RL added (target_taxa_with_native_dist.csv); RL also has some introduced country distribution data that is added<br>
 > ~ RData file with country and state polygon data from 'rnaturalearthhires' package and U.S. county polygon data from census.gov (admin_shapefiles.RData)

## 2-0_get_raw_occurrence_points.R

 Provides manual instructions and code chunks for downloading and standardizing occurrence points from a variety of online databases. Standardization includes column selection and header names, taxon name format, and values in priority columns (database, basisOfRecord, establishmentMeans, year). Data from all sources can be pulled, or specific sources can be chosen individually.

 Global databases include:
 - Global Biodiversity Information Facility (GBIF) [auto download with 'rgbif']
 - Integrated Digitized Biocollections (iDigBio) [auto download with 'ridigbio', or manual download to make sure all fields are captured]
 - U.S. Herbarium Consortia (SERNEC, SEINet, etc.) [manual download; instructions provided in script]
 - Botanical Information and Ecology Network (BIEN) [auto download with 'BIEN']

 National databases include:
 - Forest Inventory and Analysis (FIA) Program, USDA Forest Service [auto download with raw files pulled from web]
 - Biodiversity Information Serving Our Nation (BISON), USGS [auto download with 'rbison']

 >INPUTS:<br>
 > ~ List of target taxa and synonyms (target_taxa_with_syn.csv); all target names (included synonyms) are searched<br>
 > ~ FIA metadata tables (FIA_AppendixF_TreeSpeciesCodes_2016.csv; US_state_county_FIPS_codes.csv); available in repository's "data" folder
 >
 >OUTPUTS:<br>
 > ~ Raw occurrence records for target taxa or target genera, depending on how the data were downloaded (e.g., SERNEC data are downloaded for each target genus while GBIF query is by taxon name), one CSV for each database (e.g., gbif.csv); see ["Renaming Columns" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing) for schema used to rename columns and standardize data
 >
 >NOTES:<br>
 > ~ Not all data from these sources can be interpreted in the same way (e.g., vouchers or observations of wild individuals, records from botanical gardens, urban/suburban plantings, etc.), sometimes records have conflicting information or have not used the column's standards correctly (e.g., wild locality fields are filled with data referencing the museum the specimen is held in, etc.), and many have duplicates from one or more datasets. The aim of this script is to get all easily-downloadable, public occurrence data, which can then be sorted and vetted for the user's specific purposes.<br>
 > ~ You can add other occurrence point data (e.g., expert comment, NatureServe, floras, USDA PLANTS, BONAP, IUCN Red List, private sources, etc.) by standardizing column names and formatting to match the schema in the ["Renaming Columns" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing), then save as CSV and place in "inputs/compiled_occurrence" folder.

## 2-1_compile_exsitu_data.R

 ! STILL IN DEVELOPMENT !

 Takes a folder of CSV files representing ex situ accessions data from different institutions, combines them into one dataset, and standardizes some important fields.

 >INPUTS:<br>
 > ~ List of target taxa and synonyms (target_taxa_with_syn.csv)
 > ~ Folder of CSV files whose column names have be standardized by hand using the ["standardizing_accessions_data_fields"](https://docs.google.com/spreadsheets/d/1QLxxWu-bUIRcrjHiaWeSz9n1ms4EQB3yQ8P8PBBx3Zk/edit?usp=sharing) template<br>
 >
 >OUTPUTS:<br>
 > ~ Ex situ accessions data compiled into one CSV (exsitu.csv), with some fields standardized: provenance type, number of individuals (zero if dead), latitude and longitude of wild collecting location, collection/acquisition year (want to add some others eventually, like germplasm type). Depending on the institutions submitting data and your target species, most records will not have wild lat-long data; these data are often only available if the institution has a focus on wild collecting or conservation.

## 3-0_compile_raw_occurrence_points.R

 Compiles raw occurrence point data previously downloaded. Note that this script and all following work on a species level, not by taxon (original taxon name is retained for reference but all grouping is by accepted species name). Steps include:
 - Stack all occurrence point data from 2-0, and, if utilizing, ex situ data from 2-1
 - Filter by target taxa (necessary because some databases were downloaded by genus) and match to list of target taxa and synonyms (adds accepted species name column)
 - Standardize some key columns (year, basisOfRecord, establishmentMeans)
 - Check validity of latitude and longitude (can be plotted; not both equal to zero; not further than 0.01 decimal degree from land)
 - Separate out points with locality description only (no valid lat-long), which can later be geolocated manually, as desired
 - Standardize country code column to contain valid ISO3 values, for later analysis
 - Remove duplicates based on species name and lat-long rounded to 3 digits after decimal (approx. 100-160m of uncertainty depending on location; e.g., ~138m uncertainty at The Morton Arboretum)
 - Write a separate CSV of lat-long points for each target species

 >INPUTS:<br>
 > ~ Raw occurrence point data from 2-0_get_raw_occurrence_points.R and (optionally) 2-1_compile_exsitu_data.R<br>
 > ~ List of target taxa and synonyms (target_taxa_with_syn.csv)
 >
 >OUTPUTS:<br>
 > ~ CSV of all occurrence points without lat-long but with locality description (need_geolocation.csv)<br>
 > ~ spp_raw_points folder with CSV of occurrence points for each target species (e.g., Quercus_lobata.csv)<br>
 > ~ Summary table with one row for each target species, listing number of points with a valid lat-long and number of points with locality description only (occurrence_point_count_per_sp.csv)

## 3-1_refine_occurrence_points.R

 Flags suspect points by adding a column for each type of flag, where FALSE = flagged. Most of the flagging is done through the 'CoordinateCleaner' package, which was created for "geographic cleaning of coordinates from biologic collections," but GTS and RL flags are original. Flag columns include:
 - **.cen**: Flag records within 500m of country and province centroids (default is 1000m, but found it was flagging good points in small states; can test and find best value for your work)
 - **.urb**: Flag records inside urban areas (based on rnaturalearth ne_50m_urban_areas shapefile)
 - **.inst**: Flag records within 100m of biodiversity institutions
 - **.con**: Flag records outside their reported country
 - **.outl**: Flag geographic outliers in species distribution (based on quantile method, with multiplier of the interquartile range = 5; must have at least 7 records to be tested)
 - **.gtsnative**: Flag records in countries outside the species native range as reported in GlobalTreeSearch (note that sometimes points on islands or near water are flagged because slight coarseness of global polygons)
 - **.rlnative**: Flag records in countries outside the species native range as reported in the IUCN Red List (note that sometimes points on islands or near water are flagged because slight coarseness of global polygons)
 - **.rlintroduced**: Flag records in countries reported in the IUCN Red List as part of the species "introduced" range

 Other important columns for sorting and filtering include:
 - **species_name_acc**: Your accepted species name, based on target taxa list and synonyms
 - **database**: Record's source database (FIA; GBIF; US_Herbaria; iDigBio; BISON; BIEN; Ex_situ)
 - **all_source_databases**: List of all databases containing the record before duplicates were removed (dups removed based on species name and lat-long rounded to 3 digits after decimal) (e.g., "FIA,GBIF,US_Herbaria")
 - **year**: Year of collection or observation  
 - **basisOfRecord**: Type of record/how it was recorded (PRESERVED_SPECIMEN; MATERIAL_SAMPLE; OBSERVATION; HUMAN_OBSERVATION; MACHINE_OBSERVATION; LITERATURE; FOSSIL_SPECIMEN; LIVING_SPECIMEN; UNKNOWN)
 - **establishmentMeans**: Nativity/management status (NATIVE; UNKNOWN; INTRODUCED; MANAGED; CUT; INVASIVE; DEAD)
 - **coordinateUncertaintyInMeters**

 >INPUTS:<br>
 > ~ Compiled occurrence points from 3-0_compile_occurrence_points.R
 >
 >OUTPUTS:<br>
 > ~ spp_edited_points folder with CSV of occurrence points for each target species (e.g., Quercus_lobata.csv); see ["Occurrence Output" tab](https://docs.google.com/spreadsheets/d/1dllfDXaZBLvB1AsrY1wDS-sPceKAdOY681bqUbfoQAs/edit?usp=sharing) for full output metadata<br>
 > ~ Summary table with one row for each target species, listing number of points and number of flagged records in each flag column (flag_summary_by_sp.csv)


## 4-0_plot_occurrence_raw_all.R

 Creates interactive (HTML) occurrence point map for each target species, for exploring. Includes toggles that show points flagged in 3-1_refine_occurrence_points.R<br>
 Also creates two fixed basic (PNG) maps for each target species: one with all valid occurrence points (output from 3-0_compile_occurrence_points.R) and another with all flagged points removed (output from 3-1_refine_occurrence_points.R).

 >INPUTS:<br>
 > ~ Occurrence points from 3-1_refine_occurrence_points.R (spp_edited_points)<br>
 >
 > OUTPUTS:<br>
 > ~ spp_interactive_maps folder with HTML map for each target species (e.g., Quercus_lobata_leafet_map.html), which can be downloaded and opened in your browser for exploring<br>
 > ~ spp_basic_maps folder with PNG maps for each target species, one with all valid points (e.g., Quercus_lobata_raw.png) and one with unflagged points only (e.g., Quercus_lobata_filtered.png)

## X-0_Run_Point_Data.R

 ! STILL IN DEVELOPMENT !

 Runs the whole workflow from start to finish

# file structure

##**Folders/files that must be manually created or downloaded are bolded; unbolded folders/files are created or downloaded within the scripts**

  - **occurrence_points**
    - **inputs**
      - **taxa_list**
        - **target_taxa.csv**
        - target_taxa_with_syn.csv
      - **known_distribution**
        - **globaltreesearch_country_distribution.csv**
        - target_taxa_with_native_dist.csv
      - **fia_tables**
        - **FIA_AppendixF_TreeSpeciesCodes_2016.csv**
        - **US_state_county_FIPS_codes.csv**
        - PLOT.csv
      - gis_data
        - USA_adm
          - cb_2018_us_county_5m.shp and associated files
        - geo_work0.xlsx
        - geo_work1.xlsx
        - geo_work2.xlsx
        - admin_shapefiles.RData      
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
      - spp_raw_points
        - [[genus_species]].csv
      - summary_of_flagged_points_[[YYYY-MM-DD]].csv
      - spp_edited_points
        - [[genus_species]].csv
      - spp_interactive_maps
        - [[genus_species]]_leaflet_map.html
      - spp_basic_maps
        - [[genus_species]]_raw.png
