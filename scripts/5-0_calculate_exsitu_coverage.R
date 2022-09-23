################################################################################

## 5-0_calculate_exsitu_coverage.R
### Authors: Emily Beckman Bruns
### Date: September 23, 2022

### DESCRIPTION:
  # This script creates circular buffers around in situ points (wild occurrence
	#		records) and ex situ points (wild locations where seeds were collected for
	#		cultivation in botanic gardens or arboreta) to calculate the geographic
	#		(area within buffers) and ecological (number of ecoregions within buffers)
	#		diversity conserved in ex situ living collections.

### DATA IN:
	## Global country boundaries
	#		UIA World Countries Boundaries, UNIGIS Geospatial Education Resources, via
	#			ArcGIS Hub Shapefile
	#			https://hub.arcgis.com/datasets/252471276c9941729543be8789e06e12_0
	## In situ occurrence points (latitude and longitude in decimal degrees)
	# 	Can use the output from 3-1_refine_occurrence_points.R
	# 		https://github.com/MortonArb-CollectionsValue/OccurrencePoints/tree/master/scripts
	# 		Each file has data for only one species and is named "Genus_species.csv"
	# 		You can read in data for mult. species in one file but need to edit the
	#			code to split after reading in
	## Ex situ wild localities (latitude and longitude in decimal degrees)
	# 	Can use the output from 3-1_refine_occurrence_points.R, which has a
	#			"database" column that has "Ex_situ" to distinguish the ex situ records
	#			from the rest of the in situ records
	## Global Ecoregions
	# 	Terrestrial Ecoregions of the World, via WWF (Olson et al., 2001)
	#			https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
	## U.S. EPA Level III and Level IV Ecoregions
	#		Level III (us_eco_l3) -> https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip
	#		Level IV (us_eco_l4)-> https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4_state_boundaries.zip

### DATA OUT:
  ## Table of geographic and ecological coverage based on three buffer sizes
	#		(10km, 50km, 100km) and three ecoregion levels (US III, US IV, global)

################################################################################
# Load libraries
################################################################################

rm(list=ls())
my.packages <- c("ggplot2", "maps", "leaflet", "RColorBrewer", "dplyr")
#install.packages(my.packages) #Turn on to install current versions
lapply(my.packages, require, character.only=TRUE)
  rm(my.packages)

################################################################################
# Set working directory
################################################################################

# either set manually:
#main_dir <- "/Volumes/GoogleDrive/My Drive/Conservation Consortia/R Training/occurrence_points"
main_dir <- "/Volumes/GoogleDrive-103729429307302508433/Shared drives/Global Tree Conservation Program/4. GTCP_Projects/Gap Analyses/Mesoamerican Oak Gap Analysis/3. In situ/occurrence_points"
#script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"

# or use 0-1_set_workingdirectory.R script:
#source("./Documents/GitHub/OccurrencePoints/scripts/0-1_set_workingdirectory.R")
#source("scripts/0-1_set_workingdirectory.R")

################################################################################
# Load functions
################################################################################
#source(file.path(script_dir,"0-2_load_IMLS_functions.R"))


################################################################################
################################################################################
# Use leaflet package to create interactive maps to explore (html)
################################################################################
