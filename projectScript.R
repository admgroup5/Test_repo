# gives info on the packages and version of R installed
sessionInfo()
# instal package arules and include it's library
library(arules)
# importing childcare data
childcare<- read.csv("childcare.csv")
#commiting
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
install.packages("tidyverse")
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
?as.Date
class(Dates$`2014/2015`)
library(readxl)
Dates <- read_excel("raw data used/live births/Dates.xlsx", 
                    col_types = c("date", "date", "date", 
                                  "date", "date", "date", "date", "date", 
                                  "date", "date", "date", "date"))
View(Dates)
Mlive =merge(Clive2,Dates)
View(Mlive)
?merge


?rbind
Dates <- read_excel("raw data used/live births/Dates.xlsx", 
                    col_types = c("numeric", "numeric", "date", 
                                  "date", "date", "date", "date", "date", 
                                  "date", "date", "date", "date", "date", 
                                  "date"))
View(Dates)

#coersing the two names to be the same
names(Clive2) <- names(Dat) 
trial<-rbind(Clive2,Dat)
View(trial)
trialb<-rbind(Dat$...5,Clive2)
View(trialb)
# now let's gather
Gt<- trialb %>% gather(key = "Month",value = "Births", c(-1,-2))
view(CGlive2)
view(Gt)
#converting the month column to date
Gt$Month<- as.Date(Gt$Month,"%dd%mm%yyyy")
str(Gt$Month)
view(Gt)
Bt<- trialb %>% gather(key = "Month",value = "Births", c(-1,-2))
view(Bt)
?strptime
write.csv(Bt, "trial.csv", row.names=T)
newtime<- strptime(Bt$Month,"%d%m%yyyy")
view(newtime)
newCt<- strptime(Cdate,"%d%m%yyyy")
view(Cdate)
Cdat <- as.Date(Cdate,format = "%m/%d/%y")
view(Cdat)
?cbind
# this did not work as expected gBirth<- Births20182019 %>% gather(key = "Authority Name",value = "Births", c(-2))
view(gBirth)
t1<-Transposed20142015
view(t5)
t2<-Transposed20152016
t3<-Transposed20162017
t4<-Transpose20172018
t5<- Transpose20182019
t6<- t4[,-2]
view(t6)
# need to filter to have same column numbers
#creating a vector of the needed values
you<- c('Bolton', 'Bury', 'Manchester', 'Oldham', 'Salford', 'Tameside', 'Rochdale', 'Stockport', 'Trafford', 'Wigan')
FT1<- T20142015[,]
names(t1) <- names(t2) <- names(t3) <-names(t6) <-names(t5)
tbound<- rbind(t1,t2,t3,t6,t5)