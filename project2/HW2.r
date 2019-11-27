# HW 2 for qss 30.09/qbs 181

#Load in packages
install.packages("sqldf")
install.packages("RODBC")
library("RODBC")
library(sqldf)

# Question 1: Create a new column “Enrollment group” in the table Phonecall

# Setup connection to the database using ODBC strings (info removed for security purposes)
myconn <- odbcConnect("------", "------", "--------")

# Load in table
Phonecall_table <- sqlQuery(myconn, "select * from Phonecall")

# Create a new column "Enrollment group in Phonecall_table
Phonecall_table["Enrollment group"] <- NA

# Check to make sure new column is added
str(Phonecall_table)

# Merge the PhoneCall_encounters table with the Phonecall table
Phonecall_encounters <- sqlQuery(myconn, "select * from Phonecall_Encounter")

Phonecalls <- sqlQuery(myconn, "select A.*, B.* from Phonecall A
                       inner join Phonecall_Encounter B
                       on A.tri_CustomerIDEntityReference=B.CustomerId")

# Add enrollment group column to this new table
Phonecalls["EnrollmentGroup"] <- NA
# Insert EnrollmentGroup=Clinical Alert where EncounterCode = 125060000
Phonecalls$EnrollmentGroup[Phonecalls$EncounterCode == "125060000"] <- "Clinical Alert"
# Insert EnrollmentGroup=Health Coaching where EncounterCode = 125060001
Phonecalls$EnrollmentGroup[Phonecalls$EncounterCode == "125060001"] <- "Health Coaching"
# Insert EnrollmentGroup=Technical Question where EncounterCode = 125060002
Phonecalls$EnrollmentGroup[Phonecalls$EncounterCode == "125060002"] <- "Technical Question"
# Insert EnrollmentGroup=Administrative where EncounterCode = 125060003
Phonecalls$EnrollmentGroup[Phonecalls$EncounterCode == "125060003"] <- "Administrative"
# Insert EnrollmentGroup=Other where EncounterCode = 125060004
Phonecalls$EnrollmentGroup[Phonecalls$EncounterCode == "125060004"] <- "Other"
# Insert EnrollmentGroup=Lack of engagement where EncounterCode = 125060005
Phonecalls$EnrollmentGroup[Phonecalls$EncounterCode == "125060005"] <- "Lack of engagement"

# Print random 10 rows to show these changes
Phonecalls[sample(nrow(Phonecalls), 10), ]

# Question 2: Obtain the # of records for each enrollment group
EnrollmentGroup_records <- sqldf("select count(EnrollmentGroup) as Number_records, EnrollmentGroup from Phonecalls group by EnrollmentGroup")

EnrollmentGroup_records

# Question 3: Merge the Phone call encounter table with Call duration table.
# Look at the phonecall_Duration table to find key
Phonecall_Duration <- sqlQuery(myconn, "select * from CallDuration")

# Inner join the encounter table with call duration table
PhoneCall_En_Dur <- sqlQuery(myconn, "select A.*, B.* from
                            Phonecall_encounter A inner join 
                            CallDuration B on A.CustomerId=B.tri_CustomerIDEntityReference")
     

# Print random 10 rows to show these changes                       
PhoneCall_En_Dur[sample(nrow(PhoneCall_En_Dur), 10), ]


# Question 4: Find out the # of records for different call outcomes and call type. Use 1-Inbound and 2-Outbound, for call types; use 1-No response,2-Left voice mail and 3 successful. Please also find the call duration for each of the enrollment groups 
# Create a new column in the table for Inbound/Outbound - "CallType_Label"
PhoneCall_En_Dur["CallType_Label"] <- NA
# Label call type with 1 as Inbound
PhoneCall_En_Dur$CallType_Label[PhoneCall_En_Dur$CallType == 1] <- "Inbound"
# Label call type with 2 as Outbound
PhoneCall_En_Dur$CallType_Label[PhoneCall_En_Dur$CallType == 2] <- "Outbound"

# Create a new column in the table for Call outcomes - "CallOutcome_Label"
PhoneCall_En_Dur["CallOutcome_Label"] <- NA
# Label call outcome with 1 as No response
PhoneCall_En_Dur$CallOutcome_Label[PhoneCall_En_Dur$CallOutcome == 1] <- "No response"
# Label call type with 2 as Left voice mail
PhoneCall_En_Dur$CallOutcome_Label[PhoneCall_En_Dur$CallOutcome== 2] <- "Left voice mail"
# Label call type with 3 as Successful
PhoneCall_En_Dur$CallOutcome_Label[PhoneCall_En_Dur$CallOutcome == 3] <- "Successful"

# Print random 10 rows to show these changes
PhoneCall_En_Dur[sample(nrow(PhoneCall_En_Dur),10),]

# Find number of records for different call types
Phonecall_types <- sqldf("select count(CallType) as Number_records, CallType_Label from PhoneCall_En_Dur group by CallType")

Phonecall_types

# Find number of records for different call outcomes
Phonecall_outcomes <- sqldf("select count(CallOutcome) as Number_records, CallOutcome_Label from PhoneCall_En_Dur group by CallOutcome")

Phonecall_outcomes

# Find call duration for each of the enrollment groups
avg_callDur <- sqldf("select avg(CallDuration) as AverageCallDuration, EnrollmentGroup from Phonecalls group by EnrollmentGroup")

avg_callDur

# Question 5: Merge the tables Demographics, Conditions and TextMessages. Find the # of texts/per week, by the type of sender. Draw a visual using ggplot to obtain # of texts and color it by the type of sender
# Merge the tables Demographics, Conditions, and TextMessages
dem_con_text <- sqlQuery(myconn, "select A.*, B.*, C.* from Demographics A
inner join Conditions B on A.contactid = B.tri_patientid
inner join TextMessages C on B.tri_patientid = C.tri_contactId")

# Print random 10 rows to show these changes
dem_con_text[sample(nrow(dem_con_text), 10), ]

# Find the number of texts/per week by the type of sender
unique(dem_con_text$SenderName) # view the different sender types: Clinician Customer System
typeof(dem_con_text$TextSentDate)
dem_con_text$TextSentDate <- as.Date(dem_con_text$TextSentDate, format="%m/%d/%y") # convert to date format

# Make new column week number
dem_con_text["WeekNumber"] <- NA
dem_con_text$WeekNumber <- strftime(dem_con_text$TextSentDate, "%V") # get the number of weeks

text_messages_bySender <- sqldf("select count(TextSentDate) as Texts_per_week, SenderName, WeekNumber from                                    dem_con_text
                                group by WeekNumber, SenderName")

# Print random 10 rows to show these changes
text_messages_bySender[sample(nrow(text_messages_bySender), 10), ]

library(ggplot2)

# Visualize the data 
ggplot(data= text_messages_bySender, aes(x=WeekNumber, y=Texts_per_week, color=SenderName, group=SenderName)) + geom_line(size=2) + theme_light()+ xlab("Week Number")
 + ylab("Number of Texts per Week") + ggtitle("Texts per week based on Sender Name")

# Quesiton 6: Obtain the count of texts based on the chronic condition over a period of time (say per week). Draw a visual using ggplot to obtain the counts
texts_by_condition <- as.data.frame(sqldf("select count(TextSentDate) as Texts_per_week, tri_name as ChronicCondition, WeekNumber from dem_con_text group by tri_name, WeekNumber"))

# Visualize the data
ggplot(data= texts_by_condition, aes(x=WeekNumber, y=Texts_per_week, color=ChronicCondition, group=ChronicCondition)) + geom_line(size=2) + theme_light() + xlab("Week Number") 
+ ylab("Number of Texts per Week") + ggtitle("Texts per week based on Chronic Condition")

