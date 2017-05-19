library(dplyr)
library(stringr)

#Read from CSV file (STACT Data)
stact <- read.csv("STACT.txt")

#define city to run extract for
city <- 'TX'

#Extract data for city 
ExtractData <- filter (stact, str_detect(STACTData, city))
#ExtractData <- stact

#Extract data for NAICS code 541410
Extract541410 <- filter (ExtractData, str_detect(STACTData, '541410'))

#tmp2 <- as.data.frame(sapply(ExtractData, function(x) gsub("\\t", " ", x)))

#Replace comma with blank as this will interfere with saving the details as CSV format
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub(",", " ", x)))
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("\\t541410", "0000000000541410", x)))

#comma separate NAICS code
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("541410", ",541410,", x)))

#Test comma separate city & state
#tmp2 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*)(\\t)(\\D+)(\\t)(TX)(.*)", "\\1,\\3,\\5,\\6,", x)))
#tmp3 <- as.data.frame(sapply(tmp2, function(x) gsub("(.*)(\\t)(\\D+)(\\t)(TX)(.*)", "\\1,\\3,\\5,\\6,", x)))

#comma separate city names
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*)(\\t)(\\D+)(\\t)(TX)(.*)", "\\1,\\3,\\5,\\6,", x)))
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*)(\\t)(\\D+)(\\t)(TX)(.*)", "\\1,\\3,\\5,\\6,", x)))


#Extract Tax Payer Number
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub('^(.{11})(.*)$', '\\1,\\2', x)))

#Extract Outlet Number
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub('^(.{17})(.*)$', '\\1,\\2', x)))


#Remove extra tabs
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("\\t\\t", "", x)))
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("\\t", " ", x)))

#Add comma separator to split the Company Name and Address (assumption: address starts with a number)
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) sub("^(.{18})(.*?)(PO BOX|\\d+)(.*)", "\\1\\2,\\3\\4", x)))

#Split indicator flag and date fields appearing at the end of each record, after NAICS code
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*541410,.*)(\\D{1})(\\d{8})(\\d{8})(.*)", "\\1\\2,\\3,\\4\\5", x)))

#Split zip code, county code and phone number that comes after State
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*TX,)(\\d{5})(\\d{3})(\\d{10}|\\s|\\t)(.*)", "\\1\\2,\\3,\\4\\5", x)))

#Repeat the split as the zip code info appears twice
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*TX,)(\\d{5})(\\d{3})(\\d{10}|\\s|\\t)(.*)", "\\1\\2,\\3,\\4,\\5", x)))
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*TX,)(\\d{5})(\\d{3})(\\d{10}|\\s|\\t)(.*)", "\\1\\2,\\3,\\4,\\5", x)))
#Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*TX,.*)(\\d{5})(\\d{3})(\\d{10}|\\s|\\t)(.*)", "\\1\\2,\\3,\\4\\5", x)))
#Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*TX,.*)(\\d{5})(\\d{3})(\\d{10}|\\s|\\t)(.*)", "\\1\\2,\\3,\\4\\5", x)))

#Split indicator flag and date fields appearing at the end of each record, after NAICS code
#Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub("(.*541410,.*)(\\D{1})(\\d{8})(\\d{8})(.*)", "\\1\\2,\\3,\\4\\5", x)))

#Remove double quote so that the file can be created as a well-written csv file
Extract541410 <- as.data.frame(sapply(Extract541410, function(x) gsub('"', '', x)))

#write to file and add column header
write.table(Extract541410, "tmpfile.csv", sep=",", quote=FALSE)
tmpfile <- read.csv("tmpfile.csv", header = F)
colnames(tmpfile) <- c("Row", "Taxpayer Number","Outlet Number","Taxpayer Name","Taxpayer Address","Taxpayer City","Taxpayer State","Taxpayer Zipcode","Taxpayer CountyCode","Taxpayer PhoneNumber","OrgType_OrganizationName","Outlet City","Outlet State","Outlet Zipcode","Outlet CountyCode","Outlet PhoneNumber","NAICS Code","Indicator","Permit Issue Date","First Sales Date")

#Split the org details field to capture OrgType, OutletName and OutletAddress
tmpfile$OrgType <- substr(tmpfile$OrgType_OrganizationName, 1, 2)
tmpfile$OrgNameAddr <- substr(tmpfile$OrgType_OrganizationName, 3, length(tmpfile$OrgType_OrganizationName)-2)
tmpfile$OrgType_OrganizationName <-NULL

#Extract the Organization Name & Address
tmpfile$OutletName <- sapply(tmpfile$OrgNameAddr, function(x) gsub("(.*?)(\\d+)(.*)", "\\1", x))
tmpfile$OutletAddress <- sapply(tmpfile$OrgNameAddr, function(x) gsub("(.*?)(\\d+)(.*)", "\\2\\3", x))
tmpfile$OrgNameAddr <-NULL

#Define the column header
colnames(tmpfile) <- c("Row", "Taxpayer Number","Outlet Number","Taxpayer Name","Taxpayer Address","Taxpayer City","Taxpayer State","Taxpayer Zipcode","Taxpayer CountyCode","Taxpayer PhoneNumber","Outlet City","Outlet State","Outlet Zipcode","Outlet CountyCode","Outlet PhoneNumber","NAICS Code","Indicator","Permit Issue Date","First Sales Date","OrgType", "Outlet Name", "Outlet Address")

write.csv(tmpfile, paste(city, "541410_Data.csv"))
