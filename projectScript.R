# gives info on the packages and version of R installed
sessionInfo()
# instal package arules and include it's library
library(arules)
# importing childcare data
childcare<- read.csv("childcare.csv")
View(childcare)
install.packages("RODBC")
# installing readr
install.packages("readr")
library(readr)
#dropping rows, We insatll ggplot2
install.packages("ggplot2")
nrow(childcare)
childcare.10<- childcare[10, ]
View(childcare.10)
ncol(childcare)
#extracting the needed columns and viewing them
caredata <- childcare[,c(2,5,13,14,16)]
View(caredata)
#Need to count 
install.packages("dplyr")
library(dplyr)
# to count all values found including null
count(caredata,'provider.URN')
population <- read_excel("C:/Users/user/School_Project/population.xlsx",sheet = "Dataset")
View(population)
View(population[-c(1), ])
#taking Care of null values
#first We assign name "NA" to the missing values within the caredata
names(caredata)[is.na(names(caredata))]<-"NA"
#checking if caredata has any non-numeric values
NonNum<- unlist(lapply(caredata, is.numeric))
caredata[,NonNum]
# checking Non character values
NonChar<- unlist(lapply(caredata, is.character))
caredata[,NonChar]
#checking for null in the object
is.null(caredata)
#checking missing values
summary(caredata)
#dispalying missing values in caredata
install.packages("dplyr")
library(dplyr)
caredata %>% filter(is.na(Provider.URN))
# to manipulate date 
install.packages("lubridate")
library(lubridate)
caredata %>% filter(is.na(Registration.date))
df[df == "NULL"] <- NA
df[["Registration.date"]] <- as.numeric(df[[""]])
#checking Datatype
#exp <- separate(cleanhouse,'Date', c('month', 'year'), sep = '-')
View(caredata)
str(caredata)
caredata$Registration.date <-as.Date(caredata$Registration.date, origin = "1899-12-30")
#checking how complete the values are
complete.cases(caredata)
#creating dataframe for population of aged 0-5
popdata <- population[-c(1),-c(2)]
View(popdata)
#checking how complete the values are
complete.cases(popdata)
#checking Datatype
str(popdata)
#Checking number of rows
nrow(popdata)
#Checking number of Columns
ncol(popdata)
# counting all values including null
install.packages("dplyr")
library(dplyr)
count(popdata,"Age")
#working on Housing
library(readxl)
#We don't need reg Housing
RegHousing <- read_excel("C:/Users/user/School_Project/housing.xlsx",sheet = "1a")
View(RegHousing)
#We don't need the commentted Housing data 
#Localhousing <- read_excel("C:/Users/user/School_Project/housing.xlsx",sheet = "2a")
#Countyhousing <- read_excel("C:/Users/user/School_Project/housing.xlsx",sheet = "3a")
Authhousing <- read_excel("C:/Users/user/School_Project/housing.xlsx", sheet = "4a")
AHousing <- Authhousing[-c(1:5),-c(3:59,99:103)]
View(AHousing)
#View(Countyhousing)
#CHousing <- Countyhousing[-c(1:4),-c(3:61,99:105)]
#View(CHousing)
#View(RHousing)
#We are using AHousing for Housing
#performing Count on AHousing
install.packages("dplyr")
library(dplyr)
count(AHousing,"Code")
#checking for datatype
str(AHousing)
#checking the completeness of dataframe
complete.cases(AHousing)
summary(AHousing)
View(AHousing)
#working on 2015 and 2016
#Gathering the data to have date  and values arranged properly
install.packages("tidyver")
library(tidyverse)
Clive2<- livebirthsbymonthsexandladsept2015toaug2016ew
view(Clive2)
CGlive2<- Clive2 %>% gather(key = "Month",value = "Births", c(-1,-2))
view(CGlive2)
#Checking the data type of month
class(CGlive2$Month)
#To convert the character to date type
?strptimese
library(readxl)
# Importing the dates table
Dates <- read_excel("raw data used/live births/Dates.xlsx",col_types = c("date", "date", "date", "date", "date"))
View(Dates)
#Now replacing the date values in BirthDate
?replace
CGlive2$Month <- replace(CGlive2$Month,CGlive2$Month == Dates)
BirthDate<- strptime(CGlive2$Month,"%dd%mm%yyyy")
view(BirthDate)
c<-Dates$`2014/2015`
replace(BirthDate, BirthDate, c)
?as.Date
class(Dates$`2014/2015`)