# Script to quickly map occurrence points for all species



library(ggplot2); library(maps)

# ---------------------------------------
# Set up file paths etc.
# ---------------------------------------
path.imls <- "/Volumes/GoogleDrive/Shared drives/IMLS MFA/"
path.pts <- file.path(path.imls, "insitu_occurrence_points/raw_split_by_sp")
path.figs <- file.path(path.pts, "../maps_raw_split_by_sp")

if(!dir.exists(path.figs)) dir.create(path.figs, recursive=T)

map.world <- map_data("world")
# ---------------------------------------



# ---------------------------------------
#
# ---------------------------------------
# spp.test <- c("Quercus georgiana", "Quercus imbricaria", "Quercus arkansana", "Quercus falcata", "Quercus stellata", "Quercus acutissima")

spp.all <- dir(path.pts, ".csv")

for(i in 1:length(spp.all)){
  spp.now <- strsplit(spp.all[i], "[.]")[[1]][1]
  # spp.now <- gsub(spp.n)
  
  dat.now <- read.csv(file.path(path.pts, spp.all[i]))
  dat.now$decimalLatitude <- as.numeric(dat.now$decimalLatitude)
  dat.now$decimalLongitude <- as.numeric(dat.now$decimalLongitude)
  # summary(dat.now[dat.now$decimalLatitude<0,])
  summary(dat.now)
  
  if(nrow(dat.now[!is.na(dat.now$decimalLatitude),])==0) next
  
  png(file.path(path.figs, paste0(spp.now, "_raw.png")), height=6, width=10, units="in", res=180)
  print(
    ggplot() +
      coord_equal() +
      ggtitle(sub("_", " ", spp.now)) +
      geom_path(data=map.world, aes(x=long, y=lat, group=group)) +
      geom_point(data=dat.now, aes(x=decimalLongitude, y=decimalLatitude), color="red", size=2) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      theme_minimal()
  )
  dev.off()
}
# ---------------------------------------
