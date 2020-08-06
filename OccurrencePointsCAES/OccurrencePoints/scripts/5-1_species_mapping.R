library(stringr)
library(rgdal)
library(raster)

https://esp.cr.usgs.gov/data/little/querdoug.zip

gen1 <- Oak_Collection_Species$Genus[2] #get genus name from table
spec1 <- Oak_Collection_Species$Species[2] #get species name from table

gen2 <- tolower(unlist(strsplit(gen1, ""))[1:4]) #split genus letters and lower case
spec2 <- tolower(unlist(strsplit(spec1, ""))[1:4]) #split species letters and lower case

gen3 <- paste(gen2, collapse = "") #convert characters back to string
spec3 <- paste(spec2, collapse = "") #convert characters back to string

genspec <- str_c(gen3, spec3) #combine genus and species strings

webadd <- "https://esp.cr.usgs.gov/data/little/" #create string for beginning of web address
zip <- ".zip" #create string for end of web address
webadd2 <- str_c(webadd, genspec, zip) #combine all strings to make complete web address

wd1 <- getwd() #get working directory
wd2 <- str_c(wd1, genspec, sep = "/") #format working directory to include filename 
wd3 <- str_c(wd2, zip) #add .zip extension
download.file(webadd2, wd3) #download zip file from web

zipped <- str_c(genspec, zip) #create name of zipped file
unzip(zipped) #extract shapefiles from zipped file

shapename <- paste0(genspec, ".shp") #create name of shapefile
shapefile1 <- readOGR(dsn = shape_name) #import shapefile
crs(shapefile1) <- "+init=epsg:4267" #set reference?

loc <- get_map(location = c(-130, 20, -65, 50), maptype = "toner-background") #get US map
us_map <- ggmap(loc) + geom_polygon(data = shapefile1, colour = "blue", fill = "blue") #not working yet

