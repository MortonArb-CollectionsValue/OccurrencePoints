################################################################################

### Authors: Shannon Still & Emily Beckman ### Date: 05/21/2020

### DESCRIPTION:
# This script sets the working environment for the computer on which you are
#   working.

################################################################################
# Set the working environment depending on the computer on which working
################################################################################

# Use this to check your "nodename", which you should paste into the if
#   statement below
# Sys.info()[4]

## For Shannon Still:
if (Sys.info()[4] == "Still-MB-Pro-15.local") {
  # set other directories or relative paths
  imls.meta <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/metadata"
  imls.raw <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/raw_data"
  imls.output <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs"
  imls.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data"
  imls.exsitu <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Ex situ survey/standard_column_names"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(imls.local, "gbif.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))
  
  ## For Emily Beckman:
} else if (Sys.info()[4] == "Africa.local") {
  # set the absolute path
  #setwd("/Users/aesculus/Box/Research/Active_Projects/IMLS MFA/IMLS_CollectionsValue")
  #  print(getwd())
  # set other directories or relative paths
  imls.meta <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/metadata"
  imls.raw <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/raw_data"
  imls.output <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs"
  imls.local <- "./Desktop"
  imls.exsitu <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Ex situ survey/standard_column_names"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path("/Users/aesculus/Desktop/gbif.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))
  ## For Shannon workstation: 
}  
if (Sys.info()[4] == "CAES-SSTILL") {
  # set other directories or relative paths
  data.local <- "C:\Users\aesculus\Box\Research\Active_Projects\IMLS_MortonArb\MortonArb-CollectionsValue"
  imls.local <- file.path(data.local, "OccurrencePoints\local_data")
  imls.meta <- "G:\Shared drives\IMLS MFA\occurrence_points\metadata"
  imls.raw <- "G:\Shared drives\IMLS MFA\occurrence_points\raw_data"
  imls.output <- "G:\Shared drives\IMLS MFA\occurrence_points\outputs"
  imls.exsitu <- "G:\Shared drives\IMLS MFA\Ex situ survey/standard_column_names"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(imls.local, "gbif.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))
  
} else {
  
  # default, which sets the working driectory as the folder from which you opened
  #   the scripts/project
  setwd(getwd())
}
