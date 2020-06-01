# remove all variables to clear the workspace 
rm(list=ls())

# Install packages (run lines individually to ensure smooth installation)
# check for updates. If there are any for the below packages, update by clicking the refresh button
install.packages(c("boot", "class", "foreign", "lattice", "MASS", "nlme", "nnet", "survival"))
install.packages("plyr")
install.packages("data.table")
install.packages("sqldf")
install.packages("anytime")

# load packages
library(plyr)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(tidyverse)
library(data.table)
library(sqldf)
library(anytime)

# the 10 local authorities (and their codes) in the dataset below make up greater manchester
la <- read_excel("raw data used/local authorities.xlsx")

# Import the time IDs
time <- read_excel("raw data used/DimTime v2.xlsx")

# Import the provider IDs
provider <- read_excel("raw data used/Provider Type.xlsx")


###########################################################################
###########################################################################
# *****childcare****
# Import dataset, specify range and give appropriate name 
childcare <- read_excel("raw data used/Education_Childcare_dataset_as_at_31_March_2018_new (version 1).xlsx",sheet = "Childcare_providers", col_types = c("text","text", "text", "text", "date", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","numeric", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text"))

#filter dataset to only include the local authorities we are interested in
x <- childcare %>%
  filter(`Local Authority`%in% la$`Local authority`)

# Introduce a column for the Authority code by duplicating the local authority column
x$`Authority code` <- x$`Local Authority`

# filter columns to show only the required columns (remove region and others)
x2 <- x[c(2,5,6,7,13,21,40)] 
# reorder the columns 
x2 <- x2[c(1,2,3,4,7,5,6)]

# rename to work on new variable 
as1 <- x2

# check the types of data contained in the columns we are concerned about
counts <- table(as1$`Local Authority`, useNA ="ifany")
view(counts)
countsla <- table(as1$`Authority code`, useNA ="ifany")
view(countsla)

# assigning the correct Authority codes to the correct local authorities
as1$`Authority code`[as1$`Authority code` %in% "Bolton"] <- "E08000001"
as1$`Authority code`[as1$`Authority code` %in% "Bury"] <- "E08000002"
as1$`Authority code`[as1$`Authority code` %in% "Manchester"] <- "E08000003"
as1$`Authority code`[as1$`Authority code` %in% "Oldham"] <- "E08000004"
as1$`Authority code`[as1$`Authority code` %in% "Rochdale"] <- "E08000005"
as1$`Authority code`[as1$`Authority code` %in% "Salford"] <- "E08000006"
as1$`Authority code`[as1$`Authority code` %in% "Stockport"] <- "E08000007"
as1$`Authority code`[as1$`Authority code` %in% "Tameside"] <- "E08000008"
as1$`Authority code`[as1$`Authority code` %in% "Trafford"] <- "E08000009"
as1$`Authority code`[as1$`Authority code` %in% "Wigan"] <- "E08000010"


# correct the dates to remove the time 
x3 <- separate(as1,'Registration date', c('date', 'time'), sep = ' ')

# filter out the time column and then correct the order(appearance of the date column)
#  !!!!! ALWAYS RUN THE FOLLOWING TWO LINES TOGETHER PLEASE!!!!!
x4 <- x3[c(1,2,4:8)]
x4$date <- format(as.Date(x4$date), "%d/%b/%Y")
#  !!!!! ALWAYS RUN THE ABOVE TWO LINES TOGETHER PLEASE!!!!!


# Read the missing cells into the counts object along with any missing values: we have 7 nulls
# If we subtract the 68 nulls from x4's length of 3867, we should have 3799 after filtering
counts <- table(x4$`Registered places`, useNA ="ifany")
view(counts)

# Assign name "NA" to the missing values within the counts object 
names(counts)[is.na(names(counts))] <- "NA"
# Display barplot to see nulls 
barplot(counts, main="Registered places", xlab='Counts', ylab=('Registered places'), horiz=TRUE)


# remove the nulls
test <- x4 %>% filter(`Registered places`!='NULL')
# view(test)

# rename your cleaned data 
test2 <- test

# check the column names
names(test2)

# rename some columns for consistency
names(test2)[2] <- "Registration date"
names(test2)[6] <- "Authority name"
test2$`Reg date` <- test2$`Registration date`
test2 <- test2[c(1,8,2,3,4,5,6,7)]
# view(test2)

#Separate the date into day month and year columns
datesep <- separate(test2,'Registration date', c('reg day','reg month', 'reg year'), sep = '/')
# view(datesep)

# join the column for the TimeID from the time dataset
cleanedcare <- sqldf("SELECT time.TimeID, datesep.`Reg date`, datesep.`Provider URN`, datesep.`Provider type`,
datesep.`Provider name`, datesep.`Authority code`, datesep.`Authority name`, datesep.`Registered places`
FROM datesep
JOIN time
ON datesep.`reg month`=time.MonthName AND datesep.`reg year`=time.Year")
# view(cleanedcare)

# join the column for the ProviderID from the provider dataset
cleanedcare <- sqldf("SELECT cleanedcare.TimeID, cleanedcare.`Reg date`, cleanedcare.`Provider URN`, provider.`ProviderID`, cleanedcare.`Provider type`,
cleanedcare.`Provider name`, cleanedcare.`Authority code`, cleanedcare.`Authority name`, cleanedcare.`Registered places`
FROM cleanedcare
JOIN provider
ON cleanedcare.`Provider type`=provider.`Provider type`")
# view(cleanedcare)


# make sure the data types are correct. It seems sql changes everything to character
cleanedcare$TimeID <- as.numeric(cleanedcare$TimeID)
cleanedcare$`Reg date` <- anydate(cleanedcare$`Reg date`)
cleanedcare$`Provider URN` <- as.character(cleanedcare$`Provider URN`)
cleanedcare$ProviderID<- as.numeric(cleanedcare$ProviderID)
cleanedcare$`Provider type` <- as.character(cleanedcare$`Provider type`)
cleanedcare$`Provider name` <- as.character(cleanedcare$`Provider name`)
cleanedcare$`Authority code` <- as.character(cleanedcare$`Authority code`)
cleanedcare$`Authority name` <- as.character(cleanedcare$`Authority name`)
cleanedcare$`Registered places` <- as.numeric(cleanedcare$`Registered places`)

#check data type for specific column
class(cleanedcare$`Reg date`)

#check data summary
summary(cleanedcare)

#view cleaned dataset
view(cleanedcare)

# ############# checks (uncomment this section and run separately) ##################
# # data summary and first few values in each column
# str(cleanedcare)
# # checking Non numeric values
# 'isitnumeric?'<- unlist(lapply(cleanedcare, is.numeric))
# view(`isitnumeric?`)
# # checking Non character values
# 'isitcharacter?'<- unlist(lapply(cleanedcare, is.character))
# view(`isitcharacter?`)
# #checking for nulls in the object
# is.null(cleanedcare)
# complete.cases(cleanedcare)
# #dataset summary
# summary(cleanedcare)
# ############# checks (uncomment this section and run separately) ##################

# save cleaned dataset as a csv file in working directory
write.csv(cleanedcare, "clean/cleanedcare.csv", row.names=F)

######################################################################################################################################################
# Import Total live births by month and area of usual residence of Mother, 
# England and Wales, September 2014 to August 2019
births1419 <- read_excel("raw data used/live births/Merged births.xlsx")

#view what you imported
# view(births1419)

#create variable to work with
ex1 <- births1419

# change the format of the names of the columns 
names(ex1)[3:62] <-  format(as.Date(as.numeric(names(ex1)[3:62]),
                                  origin = "1899-12-30"), "%d/%b/%Y")

#filter dataset to only include the local authorities we are interested in
ex1 <- ex1 %>%
  filter(`Authority name`%in% la$`Local authority`)

#create column for dates and gather dates into this
ex2 <- gather (ex1, Date, 'new births', -1,-2)

#Duplicate the date column and call this 'Reg date'
ex2$`Reg date` <- ex2$Date

# separate the date into day month and year columns
sepn <- separate(ex2,'Date', c('reg day','reg month', 'reg year'), sep = '/')


# Join the column for the TimeID from the time dataset
cleanbirths <- sqldf("SELECT time.TimeID, sepn.`Reg date`, sepn.`Authority code`, sepn.`Authority name`, sepn.`new births`
FROM sepn
JOIN time
ON sepn.`reg month`=time.MonthName AND sepn.`reg year`=time.Year")
view(cleanbirths)

# make sure the data types are correct. It seems sql changes everything to character
cleanbirths$TimeID <- as.numeric(cleanbirths$TimeID)
cleanbirths$`Reg date` <- anydate(cleanbirths$`Reg date`)
cleanbirths$`Authority code` <- as.character(cleanbirths$`Authority code`)
cleanbirths$`Authority name` <- as.character(cleanbirths$`Authority name`)
cleanbirths$`new births`<- as.numeric(cleanbirths$`new births`)

#check data type for specific column
class(cleanbirths$`Reg date`)

#view summary
summary(cleanbirths)

#view cleaned dataset
view(cleanbirths)


# ############# checks (uncomment this section and run separately) ##################
# # data summary and first few values in each column
# str(cleanbirths)
# # checking Non numeric values
# 'isitnumeric?'<- unlist(lapply(cleanbirths, is.numeric))
# view(`isitnumeric?`)
# # checking Non character values
# 'isitcharacter?'<- unlist(lapply(cleanbirths, is.character))
# view(`isitcharacter?`)
# #checking for nulls in the object
# is.null(cleanbirths)
# complete.cases(cleanbirths)
# #dataset summary
# summary(cleanbirths)
# ############# checks (uncomment this section and run separately) ##################

#convert dataset to csv file and save in working directory
write.csv(cleanbirths, "clean/cleanbirths.csv", row.names=F)


######################################################################################################################################################

