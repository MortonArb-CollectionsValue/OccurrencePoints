################################################################################

### Authors: Emily Beckman & Shannon Still ### Date: 05/21/2020

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
    # set the absolute path
      #setwd("/Users/aesculus/Box/Research/Active_Projects/IMLS MFA/IMLS_CollectionsValue")
      #  print(getwd())
    # set other directories or relative paths
    imls.meta <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/metadata"
    imls.raw <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/raw_data"
    imls.output <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs"
    imls.local <- "/Users/aesculus/Box/Research/Active_Projects/IMLS MFA/trial_data_folder"
    # set location for login information (e.g., for GBIF)
    log_loc <- file.path("/Users/aesculus/Desktop/gbif.txt")
    # prints computer name, to let you know you're in the right spot
    print(paste("Working from the lovely",Sys.info()[4]))

## For Emily Beckman:
  } else if (Sys.info()[4] == "Africa.local") {
    # set the absolute path
      #setwd("/Users/aesculus/Box/Research/Active_Projects/IMLS MFA/IMLS_CollectionsValue")
      #  print(getwd())
    # set other directories or relative paths
    imls.meta <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/metadata"
    imls.raw <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/raw_data"
    imls.output <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points/outputs"
    imls.local <- "/Desktop"
    # set location for login information (e.g., for GBIF)
    log_loc <- file.path("/Users/aesculus/Desktop/gbif.txt")
    # prints computer name, to let you know you're in the right spot
    print(paste("Working from the lovely",Sys.info()[4]))
	} else {

# default, which sets the working driectory as the folder from which you opened
#   the scripts/project
  setwd(getwd())
}
