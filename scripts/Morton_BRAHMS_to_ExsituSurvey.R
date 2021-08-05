# load packages
library(plyr)
library(data.table)
library(readxl)
library(tidyverse)

# set working directory
main_dir <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/Ex_situ_survey"

##
### ACCESSIONS DATA TABLE (has most data we are looking for; i.e., provenance)
##

# read in all Excel accessions workbooks in data folder
acc_list <- list.files(file.path(main_dir,
  "MortonBRAHMS_IMLSgenera_Dec2020_Accessions"),
  pattern=".xlsx",full.names=TRUE)
# read in each excel file in path list to create list of dataframes
acc_dfs <- lapply(acc_list,read_xlsx,col_types="text")
# stack all dataframes
brahms_acc_raw <- data.frame()
  for(file in seq_along(acc_dfs)){
    brahms_acc_raw <- rbind(brahms_acc_raw, acc_dfs[[file]]) }
# glance at data
str(brahms_acc_raw)

# create infra_rank and infra_name columns
subsp <- brahms_acc_raw[which(!is.na(brahms_acc_raw$Subspecies)),]
  subsp$infra_rank <- "subsp."
  subsp$infra_name <- subsp$Subspecies
var <- brahms_acc_raw[which(!is.na(brahms_acc_raw$Variety)),]
  var$infra_rank <- "var."
  var$infra_name <- var$Variety
f <- brahms_acc_raw[which(!is.na(brahms_acc_raw$Forma)),]
  f$infra_rank <- "f."
  f$infra_name <- f$Forma
none <- brahms_acc_raw[which(
  is.na(brahms_acc_raw$Subspecies) &
  is.na(brahms_acc_raw$Variety) &
  is.na(brahms_acc_raw$Forma)),]
  none$infra_rank <- NA
  none$infra_name <- NA
one <- rbind(subsp,var)
two <- rbind(one,f)
brahms_acc_raw2 <- rbind(two,none)

# create locality and habitat colums
brahms_acc_raw2 <- tidyr::unite(brahms_acc_raw2,"locality", c("LocalityName","LocalityNotes"),
  sep="; ",remove=T,na.rm=T)
  brahms_acc_raw2$locality[which(brahms_acc_raw2$locality == "")] <- NA
brahms_acc_raw2 <- tidyr::unite(brahms_acc_raw2,"habitat", c("HabitatText","DescriptionText"),
  sep="; ",remove=T,na.rm=T)
  brahms_acc_raw2$habitat[which(brahms_acc_raw2$habitat == "")] <- NA

# filter to include only "active" (alive) records, rename columns, and remove unused columns
brahms_acc <- brahms_acc_raw2 %>%
  filter(AccessionStatus == "A") %>%
  dplyr::rename(acc_num = Accession,
                prov_type = ProvenanceType,
                taxon_full_name = CalcFullName,
                genus = GenusName,
                hybrid = SpeciesHybrid,
                species = SpeciesName,
                trade_name = Trademark,
                cultivar = Cultivar,
                notes = OriginNote,
                country = CountryName,
                state = MajorAdminName,
                county = MinorAdminName,
                coll_name = Collectors,
                coll_num = FieldNumber,
                coll_year = CollectionYear,
                orig_lat = Latitude,
                orig_long = Longitude,
                coord_precision = LLResolution,
                lin_num = SourceReference,
                rec_as = SuppliedAs,
                orig_source = LivingAccessionDonorCode,
                assoc_sp = Comments) %>%
  dplyr::select(acc_num,prov_type,taxon_full_name,genus,hybrid,species,infra_rank,infra_name,
                trade_name,cultivar,notes,country,state,county,coll_name,coll_num,
                coll_year,orig_lat,orig_long,coord_precision,lin_num,rec_as,orig_source,
                assoc_sp,locality,habitat
                #,AccessionStatus
              ) %>%
  arrange(taxon_full_name)
str(brahms_acc) #1396 ; 2947

##
### PLANTS DATA TABLE (has number of individuals and garden location; i.e., if in propagation)
##

# read in all Excel plants workbooks in data folder
plt_list <- list.files(file.path(main_dir,
  "MortonBRAHMS_IMLSgenera_Dec2020_Plants"),
  pattern=".xlsx",full.names=TRUE)
# read in each excel file in path list to create list of dataframes
plt_dfs <- lapply(plt_list,read_xlsx,col_types="text")
# stack all dataframes
brahms_plt_raw <- data.frame()
  for(file in seq_along(plt_dfs)){
    brahms_plt_raw <- rbind(brahms_plt_raw, plt_dfs[[file]]) }
# glance at data
str(brahms_plt_raw)

# filter to include only "active" (alive) records and those in collections (not natural areas)
brahms_plt <- brahms_plt_raw %>%
  filter(LivingStatus == "A") %>%
  filter(!grepl("^Morton ",GardenLocalityName))
# make number of plants numeric
brahms_plt$CalcRemainingPlantQuantity <- as.numeric(brahms_plt$CalcRemainingPlantQuantity)
# assign everything not in propagation as "collections"
brahms_plt[which(brahms_plt$GardenLocalityName == "Greenhouse"),]$GardenLocalityName <- "Propagation"
brahms_plt[which(brahms_plt$GardenLocalityName != "Propagation"),]$GardenLocalityName <- "Collections"

# combine duplicates
brahms_plt <- brahms_plt %>%
  dplyr::rename(acc_num = Accession,
                num_indiv = CalcRemainingPlantQuantity,
                garden_loc = GardenLocalityName) %>%
  group_by(acc_num,garden_loc) %>%
  dplyr::mutate(num_indiv = sum(num_indiv)) %>%
  ungroup() %>%
  distinct(acc_num,garden_loc,.keep_all=T) %>%
  dplyr::select(acc_num,num_indiv,garden_loc) %>%
  arrange(acc_num)
head(as.data.frame(brahms_plt))
  # check for remaining duplicate acc_num because in collections and propagation
brahms_plt[which(duplicated(brahms_plt$acc_num) | duplicated(brahms_plt$acc_num,fromLast=T)),]
nrow(brahms_plt) #1309

#ta <- brahms_acc_raw2[which(brahms_acc_raw2$AccessionStatus == "I"),]$Accession
#tp <- brahms_plt_raw[which(brahms_plt_raw$LivingStatus == "I"),]$Accession
#!(ta %in% tp)
#!(tp %in% ta)

##
### COMBINE TABLES
##

# join accessions to plants table
all_data <- left_join(brahms_acc,brahms_plt)
head(as.data.frame(all_data))
tail(as.data.frame(all_data))
nrow(all_data) #1397
  # looks like there are accessions that are "active" in the accessions table but
  #   "inactive" in the plants table
head(as.data.frame(all_data[which(is.na(all_data$garden_loc)),]))

# remove accessions with no match in plant table (in natural areas)
all_data2 <- all_data %>% filter(!is.na(garden_loc))
# genus_species column
all_data2$genus_species <- paste(all_data2$genus,all_data2$species)
all_data2$genus_species[which(grepl(" NA",all_data2$genus_species))] <- NA
all_data2[which(all_data2$hybrid == "×"),]$genus_species <- NA
str(all_data2); nrow(all_data2) #1306

# make number of plants 1 if NA
as.data.frame(all_data2[which(is.na(all_data2$num_indiv)),])
all_data2$num_indiv[which(is.na(all_data2$num_indiv))] <- 1
# remove records where no plants are alive (not sure why these are active still?)
as.data.frame(all_data2[which(all_data2$num_indiv == 0),])
all_data3 <- all_data2 %>% filter(num_indiv != 0)
nrow(all_data3) #1303

# write file
write.csv(all_data2,
  file.path(main_dir,"MortonBRAHMS_IMLSgenera_Dec2020_COMBINED.csv"),row.names = F)
