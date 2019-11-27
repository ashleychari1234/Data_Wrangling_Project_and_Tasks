# Load in packages
library("RODBC")
library(sqldf)

# Setup connection to the database using ODBC strings (information removed for security)
myconn <- odbcConnect("dartmouth", "-----", "------")

# Question 1

# Part a: Convert BP alerts to BP status
# Load in bp dataset
bp_data <- read.csv("IC_BP_v2.csv")

# Look at the structure of the data
str(bp_data)

# Convert BP alerts to BP status
colnames(bp_data)[colnames(bp_data)=="BPAlerts"] <- "BPStatus"

# Print random 10 rows showing these changes
bp_data[sample(nrow(bp_data), 10),]

# Part b: Define Hypotension-1 & Normal as Controlled blood pressure Hypotension-2, Hypertension-1, 
# Hypertension-2 & Hypertension-3 as Uncontrolled blood pressure: 
# Controlled & Uncontrolled blood pressure as 1 or 0 (Dichotomous Outcomes) 

# Convert the column into a character first, so that I can change the label
bp_data$BPStatus <- as.character(bp_data$BPStatus)

# Define Hypotension-1 & Normal as Controlled Blood pressure
bp_data$BPStatus[bp_data$BPStatus == "Hypo1"] <- "Controlled Blood Pressure"
bp_data$BPStatus[bp_data$BPStatus == "Normal"] <- "Controlled Blood Pressure"
bp_data[sample(nrow(bp_data), 10),]

# Define Hypotension-2, Hypertension-1, Hypertension-2 & Hypertension-3 as Uncontrolled Blood pressure
bp_data$BPStatus[bp_data$BPStatus == "Hypo2"] <- "Uncontrolled Blood Pressure"
bp_data$BPStatus[bp_data$BPStatus == "HTN1"] <- "Uncontrolled Blood Pressure"
bp_data$BPStatus[bp_data$BPStatus == "HTN2"] <- "Uncontrolled Blood Pressure"
bp_data$BPStatus[bp_data$BPStatus == "HTN3"] <- "Uncontrolled Blood Pressure"

# Print random 10 rows to show these changes
bp_data[sample(nrow(bp_data), 10),]


# Create a new column "BPOutcome" for the Dichotomous values for the uncontrolled and controlled blood pressure
bp_data["BPOutcome"] <- NA
bp_data$BPOutcome[bp_data$BPStatus == "Controlled Blood Pressure"] <- 1
bp_data$BPOutcome[bp_data$BPStatus == "Uncontrolled Blood Pressure"] <- 0

# Print random 10 rows to show these changes
bp_data[sample(nrow(bp_data), 10),]

# Part c: Merge this table with demographics (SQL table) to obtain their enrollment dates

# Load in demographics table from sql
Demographics_table <- sqlQuery(myconn, "select * from Demographics")
str(Demographics_table)

# Merge demographics table with bp_data
bp_demographics_data <- sqldf("select A.*, B.* from bp_data A
                              inner join Demographics_table B
                              on A.ID=B.contactid")

# Print random 10 rows to show these changes
bp_demographics_data[sample(nrow(bp_demographics_data), 10),]

# Part d: Create a 12-week interval of averaged scores of each customer 
# Use tri_enrollmentcareemailsentdate as the starting date for the twelve week interval
# Use tri_enrollmentcareemailsentdate as the starting date for the twelve week interval
library(lubridate)

# look at data type of the date column
typeof(bp_demographics_data$tri_enrollmentcompletedate)

# Convert from integer into a date format
bp_demographics_data$tri_imaginecareenrollmentemailsentdate <- as.Date(bp_demographics_data$tri_imaginecareenrollmentemailsentdate, format="%m/%d/%y")
earliest_date <- min(na.omit(bp_demographics_data$tri_imaginecareenrollmentemailsentdate))

# Add 84 days (12 weeks) to the earliest date to get the end of the 12 week interval
twelve_weeks <- earliest_date + 84

# Convert Observed Time to a date format
bp_demographics_data$ObservedTime <- mdy(bp_demographics_data$ObservedTime)

# Get all of the observed dates that fall into the 12 week interval using tri_imaginecarenrollmentemailsentdate # as the start
library(dplyr)
bp_dem_12Weeks <- bp_demographics_data %>%
  select(ID, SystolicValue, Diastolicvalue, BPStatus, ObservedTime, ObservedTime, BPOutcome, tri_imaginecareenrollmentemailsentdate) %>%
  filter(ObservedTime >= earliest_date & ObservedTime <= twelve_weeks)


# Get all of the average scores for each customer within the 12-week interval
twelve_week_avgScores <- sqldf("select ID, avg(SystolicValue) as AverageSystolicBP, avg(DiastolicValue) as AverageDiastolicBP from bp_dem_12Weeks group by ID")

# Print random 10 rows to show these changes
twelve_week_avgScores[sample(nrow(twelve_week_avgScores), 10),]

# Part e: Compare the scores from baseline (first week) to follow-up scores (12 weeks)
# Get all of the observations per ID in one row
wide_bp_dem_12Weeks <- reshape(bp_dem_12Weeks, idvar=c("ID"), timevar = "ObservedTime", direction="wide")
wide_bp_dem_12Weeks

  
# Subset to only get the data from the baseline date
earliest_observed <- min(bp_dem_12Weeks$ObservedTime)
bp_dem_baseline <- bp_dem_12Weeks %>%
  select(ID, SystolicValue, Diastolicvalue, BPStatus, ObservedTime, ObservedTime, BPOutcome, tri_imaginecareenrollmentemailsentdate) %>%
  filter(ObservedTime == earliest_observed)

# Summarize the data
summary(bp_dem_baseline)

# Subset to only get the data from the last date in the 12 week period and summarize the data
latest_observed <- max(bp_dem_12Weeks$ObservedTime)
bp_dem_followup <- bp_dem_12Weeks %>%
  select(ID, SystolicValue, Diastolicvalue, BPStatus, ObservedTime, ObservedTime, BPOutcome, tri_imaginecareenrollmentemailsentdate) %>%
  filter(ObservedTime == latest_observed)

# Summarize the data
summary(bp_dem_followup)

# Part f: How many customers were brought from uncontrolled regime to controlled regime after 12 weeks of intervention?
# Look at the BPStatus as uncontrolled for the baseline
uncontrolled_baseline <- bp_dem_baseline[bp_dem_baseline$BPStatus=='Uncontrolled Blood Pressure',]
uncontrolled_IDs <- unique(uncontrolled_baseline$ID)

# Look at the BPStatus as controlled for the 12 weeks using the ID's from uncontrolled
bp_controlled <- bp_dem_followup[bp_dem_followup$ID %in% uncontrolled_IDs,]

# Question 3: Merge the tables Demographics, Conditions and TextMessages. 
# Obtain the final dataset such that we have 1 Row per ID by choosing on the latest date when the text was sent
# if sent on multiple days) Hint: You might want to use tidyr/dplyr packages

# Load in Conditions and TextMessages tables
Conditions_table <- sqlQuery(myconn, "select * from Conditions")
TextMessages_table <- sqlQuery(myconn, "select * from TextMessages")
#Conditions_table[Conditions_table$tri_patientid == "1FEA1762-05E0-E511-8122-C4346BB59854",]

# Want to go from wide table to long table for conditions table
library(dplyr)
library(tidyr)
# Order the ids
conditions_byID <- Conditions_table[order(Conditions_table$tri_patientid),]
conditions_byID["count"] <- NA

# Take out the rows with NULL tri_patientid
conditions_byID <- conditions_byID[conditions_byID$tri_patientid != "NULL",]

# Add the counts for each condition per ID
i <- 1
j <- 1
while (i < length(conditions_byID$tri_patientid)){
  ID_name <- conditions_byID$tri_patientid[i]
  nextID <- conditions_byID$tri_patientid[i+1]
  conditions_byID$count[i] <- j
  if(ID_name == nextID){
    i <- i + 1
    j <- j + 1
  }else{
    i <- i + 1
    j <- 1
  }
}

wide_conditions_byID <- conditions_byID %>% spread(count, tri_name)
#names(wide_conditions_byID) <- c("tri_patientid", "Condition_1", "Condition_2", "Condition_3", "Condition_4", #"Condition_5", "Condition_6")

# Look at text messages table and check if the TextSentDate is in date format
typeof(TextMessages_table$TextSentDate)

# convert TextSentDate to date format 
TextMessages_table$TextSentDate <- as.Date(TextMessages_table$TextSentDate, "%m/%d/%y")

# Get the latest TextSentDate per ID
max_sent_date <- aggregate(TextMessages_table$TextSentDate, by=list(TextMessages_table$tri_contactId), max)
names(max_sent_date) <- c("Id", "MaxDate")

# Merge this with the original TextMessages_table
TextMessages_final <- sqldf("select A.*, B.* from TextMessages_table A
                            inner join max_sent_date B on A.TextSentDate=B.MaxDate where A.tri_contactId=B.Id")

# Merge the demographics and wide_conditions_byID table
#Demographics_table <- sqlQuery(myconn, "select * from Demographics")
conditions_demographics <- sqldf("select A.*, B.* from Demographics_table A
                                 inner join wide_conditions_byID B on A.contactid=B.tri_patientid")

# Merge the conditions_demographics with the TextMessages_final table
dem_conditions_texts <- sqldf("Select A.*, B.* from conditions_demographics A 
                              inner join TextMessages_final B on A.contactid=B.tri_contactid")


# Print random 10 rows to show these changes
dem_conditions_texts[sample(nrow(dem_conditions_texts), 10),]
