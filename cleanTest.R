# Install packages
library(dplyr)
install.packages(c("boot", "class", "foreign", "lattice", "MASS", "nlme", "nnet", "survival"))
library(readxl)
library(readr)
###########################################################################
###########################################################################
# remove specific variables 
# rm(population, childcare, childcare2, housing)
# or removel all variables to clear the workspace 
# rm(list=ls())

###########################################################################
###########################################################################

# Import dataset for ADMP, specify range and give appropriate name 
childcare <- read_excel("~/Dropbox/ADM Group Assessment/The Development/Education_Childcare_dataset_as_at_31_March_2018_new (version 1).xlsx",sheet = "Childcare_providers", col_types = c("text","text", "text", "text", "date", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","numeric", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text"))

# filter data to only show the local authorities we are interested in (the 10 below make up greater manchester)
x <- childcare %>% filter(`Local Authority`==c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Salford', 'Tameside', 'Rochdale', 'Stockport', 'Trafford', 'Wigan'))

# filter columns to show only the required columns
x2 <- x[c(2,5,6,7,13,16,21)] 

# correct the dates to remove the time 
x3 <- separate(x2,'Registration date', c('date', 'time'), sep = ' ')

# filter out the time column
x4 <- x3[c(1,2,4:8)]

# correct the order(appearance of the date column)
x4$date <- format(as.Date(x4$date), "%d/%m/%Y")

# remove the nulls
test <- x4 %>% filter(`Registered places`!='NULL')
view(test)

# rename your cleaned data 
cleanedcare <- test

# check the column names
names(cleanedcare)

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
counts <- table(cleanedcare$`Local Authority`, useNA ="ifany")
view(counts)

# 7 nulls in registered places
counts <- table(cleanedcare$Region, useNA ="ifany")
view(counts)

############# checks ##################
complete.cases(cleanedcare)
str(cleanedcare)
NonNum<- unlist(lapply(cleanedcare, is.numeric))
# checking Non character values
NonChar<- unlist(lapply(cleanedcare, is.character))
#checking for null in the object
is.null(cleanedcare)
#checking missing values
summary(cleanedcare)
############# checks ##################

# rename some columns
names(cleanedcare)[2] <- "Registration date"

# save cleaned dataset as a csv file
write.csv(cleanedcare, "cleanedcare.csv")

###########################################################################
###########################################################################

# *****Population****

population <- read_csv("~/Dropbox/ADM Group Assessment/The Development/Structed Data/Population 2.0.csv")

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
# for the values as the dataset is unneccesarily wide
pop4 <- gather (pop3, year, 'count', -'Local authority code', -'Local authority', -'Age')


# Introduce month column with empty values
pop4[,"month"] <- NA

# change the order so month is next to the year 
pop5 <- pop4[c(1,2,3,4,6,5)]

# change the data type to character
pop5$month <- as.character(pop5$month)

# Assign name "NA" to the empty values(all values) within the month object 
names(pop5)[is.na(names(pop5))] <- "NA"

# your clean data
cleanpop <- pop5

# save cleaned dataset as a csv file
write.csv(cleanpop, "cleanpop.csv")


###########################################################################
###########################################################################

# *****housing****

housing <- read_csv("~/Dropbox/ADM Group Assessment/The Development/Structed Data/housing.csv")