# remove all variables to clear the workspace 
# rm(list=ls())
# Install packages (run lines individually to ensure smooth installation)
# check for updates. If there are any for the below packages, update by clicking the refresh button
library(dplyr)
install.packages(c("boot", "class", "foreign", "lattice", "MASS", "nlme", "nnet", "survival"))
library(readxl)
library(readr)
library(tidyr)
library(tidyverse)

###########################################################################
###########################################################################
# *****childcare****
# Import dataset, specify range and give appropriate name 
childcare <- read_excel("raw data used/Education_Childcare_dataset_as_at_31_March_2018_new (version 1).xlsx",sheet = "Childcare_providers", col_types = c("text","text", "text", "text", "date", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","numeric", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text"))
time <- read_excel("raw data used/DimTime.xlsx")


# filter data to only show the local authorities we are interested in (the 10 below make up greater manchester)
la <- read_excel("raw data used/local authorities.xlsx")

x <- childcare %>%
  filter(`Local Authority`%in% la$`Local authority`)

# Duplicate the local authority column
x$la2 <- x$`Local Authority`

# filter columns to show only the required columns (remove region and others)
x2 <- x[c(2,5,6,7,13,21,40)] 
# reorder the columns 
x2 <- x2[c(1,2,3,4,7,5,6)]

# rename to work on new variable 
as1 <- x2
counts <- table(as1$`Local Authority`, useNA ="ifany")
view(counts)
countsla <- table(as1$la2, useNA ="ifany")
view(countsla)

as1$la2[as1$la2 %in% "Bolton"] <- "E08000001"
as1$la2[as1$la2 %in% "Bury"] <- "E08000002"
as1$la2[as1$la2 %in% "Manchester"] <- "E08000003"
as1$la2[as1$la2 %in% "Oldham"] <- "E08000004"
as1$la2[as1$la2 %in% "Rochdale"] <- "E08000005"
as1$la2[as1$la2 %in% "Salford"] <- "E08000006"
as1$la2[as1$la2 %in% "Stockport"] <- "E08000007"
as1$la2[as1$la2 %in% "Tameside"] <- "E08000008"
as1$la2[as1$la2 %in% "Trafford"] <- "E08000009"
as1$la2[as1$la2 %in% "Wigan"] <- "E08000010"

counts <- table(as1$`Local Authority`, useNA ="ifany")
view(counts)
countsla <- table(as1$la2, useNA ="ifany")
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
x3 <- separate(x2,'Registration date', c('date', 'time'), sep = ' ')

# filter out the time column
x4 <- x3[c(1,2,4:8)]

# correct the order(appearance of the date column)
x4$date <- format(as.Date(x4$date), "%d/%m/%Y")

# Read the missing cells into the counts object along with any missing values: we have 7 nulls
# If we subtract the 68 nulls from x4's length of 3867, we should have 361 after filtering
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
cleanedcare <- test

# check the column names
names(cleanedcare)

# rename some columns
names(cleanedcare)[2] <- "Registration date"
names(cleanedcare)[5] <- "Local authority"

# Run the following section by section to view how many nulls are present 
# Showed no nulls in provider URN
counts <- table(cleanedcare$`Provider URN`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(cleanedcare$`Registration date`, useNA ="ifany")
view(counts)

# no nulls in provider type
counts <- table(cleanedcare$`Provider type`, useNA ="ifany")
view(counts)

# no nulls in provider name 
counts <- table(cleanedcare$`Provider name`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(cleanedcare$`Local authority`, useNA ="ifany")
view(counts)

# no nulls in region
counts <- table(cleanedcare$Region, useNA ="ifany")
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

###########################################################################
###########################################################################
# *****Population****
# Import dataset
population <- read_csv("raw data used/Population 2.0.csv")

# create new version to clean
pop2 <- population

# rename some columns
names(pop2)[1] <- "Local authority"
names(pop2)[2] <- "Local authority code"

# change the order of the columns 
pop2 <- pop2[c(2,1,3,4,5,6,7,8,9,10)]

# remove unnecessary columns 
pop3 <- pop2[c(1,2,3,5,6,7,8,9,10)]

# create a key(to be called'year') for the years and a new column 
# for the values (called 'count')as the dataset is unneccesarily wide
pop4 <- gather (pop3, year, 'count', -'Local authority code', -'Local authority', -'Age')


# Introduce month column with empty values
pop4[,"month"] <- NA

# change the order so month is next to the year 
pop5 <- pop4[c(1,2,3,4,6,5)]

# change the data type to character
pop5$month <- as.character(pop5$month)

# replace the values in the month column with the value we want ("NA")
tst <- c()
value <- c('NA')
tst[1:360]<- unique(value)[1]

pop5$month <- tst

# your clean data
cleanpop <- pop5

############# checks ##################
complete.cases(cleanpop)
str(cleanpop)
# checking Non numeric values
'isitnumeric?'<- unlist(lapply(cleanpop, is.numeric))
view(`isitnumeric?`)
# checking Non character values
'isitcharacter?'<- unlist(lapply(cleanpop, is.character))
view(`isitcharacter?`)
#checking for null in the object
is.null(cleanpop)
#checking missing values
summary(cleanpop)
############# checks ##################

# save cleaned dataset as a csv file
write.csv(cleanpop, "clean/cleanpop.csv", row.names=F)

###########################################################################
###########################################################################
# *****housing****
# Import dataset
housing <- read_csv("raw data used/housing.csv")

# rename some columns
names(housing)[4] <- "Local authority"

# filter out unwanted local authorities
y <- housing %>%
  filter(`Local authority` %in% tst2)

# check counts
counts <- table(y$`Local authority`, useNA ="ifany")
view(counts)

# create a key(to be called'date') for the dates and a new column 
# for the number of houses called 'new houses' as the dataset is unneccesarily wide
houses <- gather (y, Date, 'new houses', -'Region/Country code', -'Region/Country name', -'Local authority code', -'Local authority')


############# checks ##################
complete.cases(houses)
str(houses)
# checking Non numeric values
'isitnumeric?'<- unlist(lapply(houses, is.numeric))
view(`isitnumeric?`)
# checking Non character values
'isitcharacter?'<- unlist(lapply(houses, is.character))
view(`isitcharacter?`)
#checking for null in the object
is.null(houses)
#checking missing values
summary(houses)
############# checks ##################

# since its all clean, we can rename 
cleanhouse <- houses

# save cleaned dataset as a csv file
write.csv(cleanhouse, "clean/cleanhouse.csv", row.names=F)

