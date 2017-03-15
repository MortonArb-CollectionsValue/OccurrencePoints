https://esp.cr.usgs.gov/data/little/querdoug.zip

gen1 <- Oak_Collection_Species$Genus[1] #get genus name from table
spec1 <- Oak_Collection_Species$Species[1] #get species name from table

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
curl_download(webadd2, wd2) #download file to working directory with filename
