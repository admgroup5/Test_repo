# remove all variables to clear the workspace 
# rm(list=ls())
# Install packages (run lines individually to ensure smooth installation)
# check for updates. If there are any for the below packages, update by clicking the refresh button
install.packages(c("boot", "class", "foreign", "lattice", "MASS", "nlme", "nnet", "survival"))
install.packages("plyr")
install.packages("data.table")
install.packages("sqldf")

library(plyr)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(tidyverse)
library(data.table)
library(sqldf)

# filter data to only show the local authorities we are interested in (the 10 below make up greater manchester)
la <- read_excel("raw data used/local authorities.xlsx")

# Import the time IDs
time <- read_excel("raw data used/DimTime v2.xlsx")

###########################################################################
###########################################################################
# *****childcare****
# Import dataset, specify range and give appropriate name 
childcare <- read_excel("raw data used/Education_Childcare_dataset_as_at_31_March_2018_new (version 1).xlsx",sheet = "Childcare_providers", col_types = c("text","text", "text", "text", "date", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","numeric", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text"))

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

# introduce the Authority codes
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

counts <- table(as1$`Local Authority`, useNA ="ifany")
view(counts)
countsla <- table(as1$`Authority code`, useNA ="ifany")
view(countsla)

# E08000001	Bolton
# E08000002	Bury
# E08000003	Manchester
# E08000004	Oldham
# E08000005	Rochdale
# E08000006	Salford
# E08000007	Stockport
# E08000008	Tameside
# E08000009	Trafford
# E08000010	Wigan



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
# Display barplot 
barplot(counts, main="Registered places", xlab='Counts', ylab=('Registered places'), horiz=TRUE)


# remove the nulls
test <- x4 %>% filter(`Registered places`!='NULL')
view(test)

# rename your cleaned data 
test2 <- test

# check the column names
names(test2)

# rename some columns
names(test2)[2] <- "Registration date"
names(test2)[6] <- "Authority name"
test2$`Reg date` <- test2$`Registration date`
test2 <- test2[c(1,8,2,3,4,5,6,7)]
view(test2)


datesep <- separate(test2,'Registration date', c('reg day','reg month', 'reg year'), sep = '/')
view(datesep)

# Add a column for the TimeID
cleanedcare <- sqldf("SELECT time.TimeID, datesep.`Reg date`, datesep.`Provider URN`, datesep.`Provider type`,
datesep.`Provider name`, datesep.`Authority code`, datesep.`Authority name`, datesep.`Registered places`
FROM datesep
JOIN time
ON datesep.`reg month`=time.MonthName AND datesep.`reg year`=time.Year")
view(cleanedcare)


# Run the following section by section to view how many nulls are present 
# Showed no nulls in provider URN
counts <- table(cleanedcare$`Provider URN`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(cleanedcare$`Reg date`, useNA ="ifany")
view(counts)

# no nulls in provider type
counts <- table(cleanedcare$`Provider type`, useNA ="ifany")
view(counts)

# no nulls in provider name 
counts <- table(cleanedcare$`Provider name`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(cleanedcare$`Authority code`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(cleanedcare$`Authority name`, useNA ="ifany")
view(counts)

# no nulls in registered places
counts <- table(cleanedcare$`Registered places`, useNA ="ifany")
view(counts)

############# checks ##################
complete.cases(cleanedcare)
str(cleanedcare)
# checking Non numeric values
'isitnumeric?'<- unlist(lapply(cleanedcare, is.numeric))
view(`isitnumeric?`)
# checking Non character values
'isitcharacter?'<- unlist(lapply(cleanedcare, is.character))
view(`isitcharacter?`)
#checking for null in the object
is.null(cleanedcare)
#checking missing values
summary(cleanedcare)
############# checks ##################

# save cleaned dataset as a csv file
write.csv(cleanedcare, "clean/cleanedcare.csv", row.names=F)

######################################################################################################################################################
# Import Total live births by month and area of usual residence of Mother, 
# England and Wales, September 2014 to August 2019
births1419 <- read_excel("raw data used/live births/Merged births.xlsx")
view(births1419)
ex1 <- births1419

# change the format of the names of the columns 
names(ex1)[3:62] <-  format(as.Date(as.numeric(names(ex1)[3:62]),
                                  origin = "1899-12-30"), "%d/%b/%Y")

ex1 <- ex1 %>%
  filter(`Authority name`%in% la$`Local authority`)

ex2 <- gather (ex1, Date, 'new births', -1,-2)
view(ex2)

ex2 <- separate(ex2,'Date', c('reg day','reg month', 'reg year'), sep = '/')
view(ex2)

# Add a column for the TimeID
cleanbirths <- sqldf("SELECT time.TimeID, ex2.`Authority code`, ex2.`Authority name`, ex2.`new births`
FROM ex2
JOIN time
ON ex2.`reg month`=time.MonthName AND ex2.`reg year`=time.Year")
view(cleanbirths)


counts <- table(ex2$`Authority code`, useNA ="ifany")
counts <- table(ex2$`Authority name`, useNA ="ifany")
counts <- table(ex2$`Date`, useNA ="ifany")
counts <- table(z$`TimeID`, useNA ="ifany")
view(counts)

############# checks ##################
complete.cases(cleanbirths)
str(cleanbirths)
# checking Non numeric values
'isitnumeric?'<- unlist(lapply(cleanbirths, is.numeric))
view(`isitnumeric?`)
# checking Non character values
'isitcharacter?'<- unlist(lapply(cleanbirths, is.character))
view(`isitcharacter?`)
#checking for null in the object
is.null(cleanbirths)
#checking missing values
summary(cleanbirths)
############# checks ##################

write.csv(cleanbirths, "clean/notyetreadybirths.csv", row.names=F)


######################################################################################################################################################

