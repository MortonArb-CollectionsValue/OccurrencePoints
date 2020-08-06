################################################################################

## 0-1_set_workingdirectory.R
### Authors: Shannon Still & Emily Beckman ### Date: 05/21/2020

### DESCRIPTION:
# This script sets the working environment for the computer on which you are
#   working.

################################################################################
# Set working environment depending on your computer
################################################################################

# Use this to check your "nodename"
# Sys.info()[4]

## For Shannon Still:
if (Sys.info()[4] == "Still-MB-Pro-15.local") {
  # set main working directory
  main_dir <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points"
  # set location of scripts
  script_dir <- "scripts"
  # OPTIONAL: set local working directory, for trialing locally before saving
  #   to main working directory
  local_dir <- "/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data"

  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(local_dir, "gbif.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## For Emily Beckman:
} else if (Sys.info()[4] == "Africa.local") {
  # set main working directory
  main_dir <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/occurrence_points"
  # set location of scripts
  script_dir <- "./Documents/GitHub/OccurrencePoints/scripts"
  # OPTIONAL: set local working directory, for trialing locally before saving
  #   to main working directory
  local_dir <- "./Desktop"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(local_dir, "IMLS_passwords.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## For Shannon workstation:
} else if (Sys.info()[4] == "CAES-SSTILL") {
  # set main working directory
  main_dir <- "G:/Shared drives/IMLS MFA/occurrence_points"
  # set location of scripts
  script_dir <- "scripts"
  # OPTIONAL: set local working directory, for trialing locally before saving
  #   to main working directory
  local_dir <- "C:/Users/aesculus/Box/Research/Active_Projects/IMLS_MortonArb/local_data"
  # set location for login information (e.g., for GBIF)
  log_loc <- file.path(local_dir, "gbif.txt")
  # prints computer name, to let you know you're in the right spot
  print(paste("Working from the lovely", Sys.info()[4]))

## can add as many additional "else if" sections as needed to cover other
#   workstations

} else {
  # default, which sets the working driectory as the folder from which you
  #   opened the scripts/project
  setwd(getwd())
}
