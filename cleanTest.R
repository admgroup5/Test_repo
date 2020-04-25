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

# Introduce a column for the local authority code by duplicating the local authority column
x$`Local authority code` <- x$`Local Authority`

# filter columns to show only the required columns (remove region and others)
x2 <- x[c(2,5,6,7,13,21,40)] 
# reorder the columns 
x2 <- x2[c(1,2,3,4,7,5,6)]

# rename to work on new variable 
as1 <- x2

# check the types of data contained in the columns we are concerned about
counts <- table(as1$`Local Authority`, useNA ="ifany")
view(counts)
countsla <- table(as1$`Local authority code`, useNA ="ifany")
view(countsla)

# introduce the local authority codes
as1$`Local authority code`[as1$`Local authority code` %in% "Bolton"] <- "E08000001"
as1$`Local authority code`[as1$`Local authority code` %in% "Bury"] <- "E08000002"
as1$`Local authority code`[as1$`Local authority code` %in% "Manchester"] <- "E08000003"
as1$`Local authority code`[as1$`Local authority code` %in% "Oldham"] <- "E08000004"
as1$`Local authority code`[as1$`Local authority code` %in% "Rochdale"] <- "E08000005"
as1$`Local authority code`[as1$`Local authority code` %in% "Salford"] <- "E08000006"
as1$`Local authority code`[as1$`Local authority code` %in% "Stockport"] <- "E08000007"
as1$`Local authority code`[as1$`Local authority code` %in% "Tameside"] <- "E08000008"
as1$`Local authority code`[as1$`Local authority code` %in% "Trafford"] <- "E08000009"
as1$`Local authority code`[as1$`Local authority code` %in% "Wigan"] <- "E08000010"

counts <- table(as1$`Local Authority`, useNA ="ifany")
view(counts)
countsla <- table(as1$`Local authority code`, useNA ="ifany")
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
x4$date <- format(as.Date(x4$date), "%d/%m/%Y")
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
cleanedcare <- test

# check the column names
names(cleanedcare)

# rename some columns
names(cleanedcare)[2] <- "Registration date"
names(cleanedcare)[6] <- "Local authority"

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
counts <- table(cleanedcare$`Local authority code`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(cleanedcare$`Local authority`, useNA ="ifany")
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
write.csv(cleanhouse, "clean/cleanhouse.csv", row.names=F)

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
write.csv(cleanpop, "clean/cleanpop.csv", row.names=F)

######################################################################################################################################################
# Import Total live births by month and area of usual residence of Mother, 
# England and Wales, September 2014 to August 2019
births1419 <- read_excel("raw data used/live births/Merged births.xlsx")
view(births1419)
ex1 <- births1419


names(ex1)[3:62] <-  format(as.Date(as.numeric(names(ex1)[3:62]),
                                  origin = "1899-12-30"), "%d/%m/%Y")



ex2 <- gather (ex1, Date, 'new births', -1,-2)
view(ex2)

counts <- table(ex2$`Authority code`, useNA ="ifany")
counts <- table(ex2$`Authority name`, useNA ="ifany")
counts <- table(ex2$`Date`, useNA ="ifany")
view(counts)

write.csv(ex2, "clean/notyetreadybirths.csv", row.names=F)


######################################################################################################################################################

# lon<-live20142015 %>% gather(Key="DateofBirth", Value = "NumberofBirth", c(-1,-2))
rlang::last_error()
# datefig<- live20142015[5,]
# view(datefig)
# to convert to date type
# I tried working on these but could ot get what I expected, I will check it again