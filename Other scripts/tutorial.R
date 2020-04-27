# load and save your data
patients <- read_csv("OneDrive - Sheffield Hallam University/2nd semester/Advanced Data Management Project/data integration/csv files/patients.csv")
patfile <- read_csv("OneDrive - Sheffield Hallam University/2nd semester/Advanced Data Management Project/data integration/csv files/patfile.csv")
flights <- read_csv("OneDrive - Sheffield Hallam University/2nd semester/Advanced Data Management Project/data integration/csv files/flights.csv")
consfile <- read_csv("OneDrive - Sheffield Hallam University/2nd semester/Advanced Data Management Project/data integration/csv files/consfile.csv")

# View your loaded data
View(patients)
View(patfile)
View(flights)
View(consfile)

# Produce a Simple Frequency Count for Gender Variable
count(patients, 'GENDER')
# Produce a Simple Frequency Count for AE Variable
count(patients, 'AE')
# Produce a Simple Frequency Count for DX Variable
count(patients, 'DX')

# Simple Horizontal Bar Plot with Added Labels - Gender
# Read the missing cells into the counts object along with any missing values 
counts <- table(patients$GENDER, useNA ="ifany")

# Assign name "NA" to the missing values within the counts object 
names(counts)[is.na(names(counts))] <- "NA"
# Display barplot 
barplot(counts, main="Gender Distribution", xlab='Counts', ylab='Gender', horiz=TRUE)


# Simple Horizontal Bar Plot with Added Labels - AE
# Read the missing cells into the counts object along with any missing values 
counts <- table(patients$AE, useNA ="ifany")
# Assign name "NA" to the missing values within the counts object 
names(counts)[is.na(names(counts))] <- "NA"
# Display barplot 
barplot(counts, main="AE Distribution", xlab='Counts', ylab='Gender', horiz=TRUE)

# Simple Horizontal Bar Plot with Added Labels - DX
# Read the missing cells into the counts object along with any missing any values 
counts <- table(patients$DX, useNA ="ifany")
# Assign name "NA" to the missing values within the counts object 
names(counts)[is.na(names(counts))] <- "NA"
# Display barplot 
barplot(counts, main="DX Distribution", xlab='Counts', ylab='Gender', horiz=TRUE)
