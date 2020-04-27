###########################################################################

# *******This is old code that could not be used due to granularity************

###########################################################################
# remove all variables to clear the workspace 
# rm(list=ls())
# Install packages (run lines individually to ensure smooth installation)
# check for updates. If there are any for the below packages, update by clicking the refresh button
install.packages(c("boot", "class", "foreign", "lattice", "MASS", "nlme", "nnet", "survival"))
install.packages("plyr")
library(plyr)
library(dplyr)
library(readxl)
library(readr)
library(tidyr)
library(tidyverse)

# filter data to only show the local authorities we are interested in (the 10 below make up greater manchester)
la <- read_excel("raw data used/local authorities.xlsx")

# Import the time IDs
time <- read_excel("raw data used/DimTime.xlsx")


###########################################################################
###########################################################################
# *****housing****
# Import dataset
housing <- read_csv("raw data used/housing.csv")

# rename some columns
names(housing)[4] <- "Local authority"

# filter out unwanted local authorities and the unwanted columns 
y <- housing %>%
  filter(`Local authority` %in% la$`Local authority`)
y <- y[3:100]
view(y)

# check counts
counts <- table(y$`Local authority`, useNA ="ifany")
view(counts)

# create a key(to be called'date') for the dates and a new column 
# for the number of houses called 'new houses' as the dataset is unneccesarily wide
houses <- gather (y, Date, 'new houses', -'Local authority code', -'Local authority')
view(houses)

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
write.csv(cleanhouse, "clean/old clean bad granularity/cleanhouse.csv", row.names=F)

###########################################################################
###########################################################################
# *****Population****
# Import dataset
population <- read_csv("raw data used/Population 2.0.csv")

# create new version to clean
pop2 <- population
view(pop2)

# rename some columns
names(pop2)[1] <- "Local authority"
names(pop2)[2] <- "Local authority code"

# change the order of the columns then remove unnecessary columns 
pop2 <- pop2[c(2,1,3,4,5,6,7,8,9,10)]
pop3 <- pop2[c(1,2,3,5,6,7,8,9,10)]

# create a key(to be called'year') for the years and a new column 
# for the values (called 'count')as the dataset is unneccesarily wide
pop4 <- gather (pop3, year, 'count', -'Local authority code', -'Local authority', -'Age')


# # Introduce month column with empty values
# pop4[,"month"] <- NA

# # change the order so month is next to the year 
# pop5 <- pop4[c(1,2,3,6,4,5)]
# 
# # change the data type to character
# pop5$month <- as.character(pop5$month)

# replace the values in the month column with the value we want ("NA")
# tst <- c()
# value <- c('NA')
# tst[1:360]<- unique(value)[1]
# 
# pop5$month <- tst

# your clean data
cleanpop <- pop4

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
write.csv(cleanpop, "clean/old clean bad granularity/cleanpop.csv", row.names=F)

