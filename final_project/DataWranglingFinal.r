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

# Get all of the observed dates that fall into the 12 week interval
observed_aboveEarliest <- subset(bp_demographics_data, bp_demographics_data$ObservedTime >= earliest_date)
bp_dem_12Weeks <- subset(observed_aboveEarliest, observed_aboveEarliest$ObservedTime <= twelve_weeks)

# Get all of the average scores for each customer within the 12-week interval
twelve_week_avgScores <- sqldf("select ID, avg(SystolicValue) as AverageSystolicBP, avg(DiastolicValue) as AverageDiastolicBP from bp_dem_12Weeks group by ID")

# Print random 10 rows to show these changes
twelve_week_avgScores[sample(nrow(twelve_week_avgScores), 10),]

# Part e: Compare the scores from baseline (first week) to follow-up scores (12 weeks)


# Part f: How many customers were brought from uncontrolled regime to controlled regime after 12 weeks of intervention?


# Question 3: Merge the tables Demographics, Conditions and TextMessages. 
# Obtain the final dataset such that we have 1 Row per ID by choosing on the latest date when the text was sent
# if sent on multiple days) Hint: You might want to use tidyr/dplyr packages

