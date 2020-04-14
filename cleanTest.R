# Install packages
library(dplyr)
install.packages(c("boot", "class", "foreign", "lattice", "MASS", "nlme", "nnet", "survival"))
library(readxl)
library(readr)
###########################################################################
###########################################################################
# remove variables 
rm(population, childcare, childcare2, housing)

###########################################################################
###########################################################################

# Import dataset for ADMP, specify range and give appropriate name 
population <- read_csv("~/Dropbox/ADM Group Assessment/The Development/Structed Data/Population 2.0.csv")
#childcare <- read_excel("~/Dropbox/ADM Group Assessment/The Development/Education_Childcare_dataset_as_at_31_March_2018_new (version 1).xlsx", sheet = "Childcare_providers")
childcare <- read_excel("~/Dropbox/ADM Group Assessment/The Development/Education_Childcare_dataset_as_at_31_March_2018_new (version 1).xlsx",sheet = "Childcare_providers", col_types = c("text","text", "text", "text", "date", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","numeric", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text", "text","text", "text", "text", "text"))
childcare2 <- read_csv("~/Dropbox/ADM Group Assessment/The Development/Structed Data/facilities.csv")
housing <- read_csv("~/Dropbox/ADM Group Assessment/The Development/Structed Data/housing.csv")

###########################################################################
###########################################################################

# 1) rename specific columns 
# 1.5) rearrange the order of the columns appearing 
# 2) convert Excel numeric date data to correct date format in R
# 
# 3) to quickly find and remove data that has wrong dates
# 
# 4) to backup and store any rows that will be updated or removed.
# 
# 5) to identify, edit and / or remove null or missing data.
# 
# 6) to identify and remove garbage data.
# 
# 7) then we will make some quick and easy insights in the end that you will find interesting on this dataset.
# 

###########################################################################
###########################################################################


x <- childcare %>% filter(`Local Authority`==c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Salford', 'Tameside', 'Rochdale', 'Stockport', 'Trafford', 'Wigan'))
x2 <- x[c(2:7,13,14,21,22)]
x3 <- separate(x2,'Registration date', c('date', 'time'), sep = ' ')
x4 <- x3[c(1:4,6:11)]
x4$date <- format(as.Date(x4$date), "%d/%m/%Y")

# check the column names
names(x4)

# Run the following section by section to view how many nulls are present 
# Showed no nulls in provider URN
counts <- table(x4$`Provider URN`, useNA ="ifany")

# 267 nulls in registered person URN
counts <- table(x4$`Registered Person URN`, useNA ="ifany")
view(counts)

# 267 nulls in registered person name
counts <- table(x4$`Registered Person Name`, useNA ="ifany")

# no nulls here
counts <- table(x4$date, useNA ="ifany")
view(counts)

# no nulls in provider type
counts <- table(x4$`Provider type`, useNA ="ifany")
view(counts)

# no nulls in provider name 
counts <- table(x4$`Provider name`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(x4$`Local Authority`, useNA ="ifany")
view(counts)

# no nulls here 
counts <- table(x4$`Parliamentary constituency`, useNA ="ifany")
view(counts)

# 7 nulls in registered places
counts <- table(x4$`Registered places`, useNA ="ifany")
view(counts)

# no nulls here
counts <- table(x4$`Registered places including estimates`, useNA ="ifany")
view(counts)

# remove the nulls and view 
test <- x4 %>% filter(`Registered Person URN`!='NULL', `Registered places`!='NULL')
view(test)

# remove the nulls and view separate thing
test2 <- x4 %>% filter(`Registered places`!='NULL')
test3 <- test2[c(1,4:10)]
view(test3)

# rename your cleaned data 
cleanedcare2 <- test3

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

# save cleaned dataset as a csv file
write.csv(cleanedcare, "cleanedcare.csv")
write.csv(cleanedcare2, "cleanedcare2.csv")



