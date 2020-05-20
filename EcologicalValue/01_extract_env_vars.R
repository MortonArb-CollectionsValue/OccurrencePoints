# Script to get extract & store environmental variables for occurence poitns
# 1. Load species points
# 2. Extract raw values and perform calculations
#    2.a. Climate (Daymet) -- 
#    2.b. Soils
#    2.c. Landcover
# 3. Store values by species as csv?




library(sp); library(ggplot2); library(maps)

# ---------------------------------------
# Set up file paths etc.
# ---------------------------------------
path.imls <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"
path.pts <- file.path(path.imls, "insitu_occurrence_points/raw_split_by_sp")
path.figs <- file.path(path.imls, "Environmental Niche Value", "figures")
path.data <- file.path(path.imls, "Environmental Niche Value", "data")

if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)
if(!dir.exists(path.figs)) dir.create(path.data, recursive=T)
if(!dir.exists(file.path(path.figs, "maps"))) dir.create(file.path(path.figs, "maps"), recursive=T)

arb.lat=41.812739
arb.lon=-88.072749


map.world <- map_data("world")
map.world2 <- map_data("world2")
map.us <- map_data("state")

# ggplot() +
#   coord_equal() +
#   geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
#   scale_x_continuous(expand=c(0,0)) +
#   scale_y_continuous(expand=c(0,0))
# ---------------------------------------



# ---------------------------------------
#
# ---------------------------------------
spp.test <- c("Quercus georgiana", "Quercus imbricaria", "Quercus arkansana", "Quercus falcata", "Quercus stellata", "Quercus acutissima")

spp.all <- dir(path.pts, ".csv")

for(i in 1:length(spp.test)){
  fnow <- grep(sub(" ", "_", spp.test[i]), spp.all)
  
  dat.now <- read.csv(file.path(path.pts, spp.all[fnow]))
  summary(dat.now)
  
  # we have some VERY odd locations... FIA and BIEN seem to be the biggest offenders 
  #  FIA should only be in the northern hemisphere (US), so assume everything negative is backwards 
  #  BIEN is a bit different... BIEN looks like it has a LOT of redundancy, so lets just get rid of it
  #  Not feeling like "preserved specimens" are actually located to places
  #  Also highly questioning the reliability of iNaturalist observations -- I think they're REALLY skewing Q. acutissima
  dat.now <- dat.now[!is.na(dat.now$decimalLatitude),]
  if(nrow(dat.now)==0) next
  
  dat.now <- dat.now[dat.now$decimalLatitude!=0 & dat.now$decimalLongitude!=0,] # get rid of things with 0,0 location
  dat.now <- dat.now[!dat.now$database %in% c("BIEN"),] # Lets just get rid of BIEN -- it looks like lots of overlap
  dat.now <- dat.now[!dat.now$basisOfRecord %in% c("PRESERVED_SPECIMEN"),] # This seems odd too and probably not an actual occurrence point
  dat.now <- dat.now[!dat.now$publisher %in% ("iNaturalist.org"),]
  rows.bad <- which((dat.now$database=="FIA" & dat.now$decimalLatitude < 0) ) # 
  dat.now[rows.bad, c("decimalLongitude", "decimalLatitude")] <- dat.now[rows.bad, c("decimalLatitude", "decimalLongitude")]
  dat.now[dat.now$decimalLatitude<0,c("decimalLongitude", "decimalLatitude", "database")]
  
  png(file.path(path.figs, "maps", paste0(sub(" ", "_", spp.test[i]), "_adj.png")), height=6, width=10, units="in", res=180)
  print(
  ggplot() +
    coord_equal() +
    ggtitle(spp.test[i]) +
    geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
    geom_point(data=dat.now[,], aes(x=decimalLongitude, y=decimalLatitude), color="red", size=2) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    theme_minimal()
  )
  dev.off()
  
  
  
  
}
# ---------------------------------------
