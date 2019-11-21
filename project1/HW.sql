/* Print random 10 rows from the table */
select top 10 * from [afrancisco].demographics order by newid();

/* Rename all the columns in the table */
exec sp_rename 'afrancisco.demographics.tri_age', 'Age', 'COLUMN';
exec sp_rename 'afrancisco.demographics.gendercode', 'gender', 'COLUMN';
exec sp_rename 'afrancisco.demographics.contactid', 'ID', 'COLUMN';
exec sp_rename 'afrancisco.demographics.address1_stateorprovince', 'State', 'COLUMN';
exec sp_rename 'afrancisco.demographics.tri_imaginecareenrollmentemailsentdate', 'EmailSentDate', 'COLUMN';

/* Create a new column “Enrollment Status”*/
alter table [afrancisco].demographics add Enrollment_status nvarchar(50);
/* Change codes */
update [afrancisco].demographics set Enrollment_status='Complete' where tri_imaginecareenrollmentstatus=167410011;
update [afrancisco].demographics set Enrollment_status='Email Sent' where tri_imaginecareenrollmentstatus=167410001;
update [afrancisco].demographics set Enrollment_status='Non responder' where tri_imaginecareenrollmentstatus=167410004;
update [afrancisco].demographics set Enrollment_status='Facilitated Enrollment' where tri_imaginecareenrollmentstatus=167410005;
update [afrancisco].demographics set Enrollment_status='Incomplete Enrollments' where tri_imaginecareenrollmentstatus=167410002;
update [afrancisco].demographics set Enrollment_status='Opted Out' where tri_imaginecareenrollmentstatus=167410003;
update [afrancisco].demographics set Enrollment_status='Unprocessed' where tri_imaginecareenrollmentstatus=167410000;
update [afrancisco].demographics set Enrollment_status='Second email sent' where tri_imaginecareenrollmentstatus=167410006;

/* create new column sex */
alter table [afrancisco].demographics add Sex nvarchar(50);
/* Change the codes */
update [afrancisco].demographics set Sex='female' where gender='2';
update [afrancisco].demographics set Sex='male' where gender='1';
update [afrancisco].demographics set Sex='other' where gender='167410000';
update [afrancisco].demographics set Sex='Unknown' where gender='NULL';

/* Create a new column “Age group” and create age groups with an interval of 25 yrs. for example 0-25 years as ‘0-25’, 26-50 as “26-50” */
select * from [afrancisco].demographics;
/* Find the max age to create intervals */
select max(Age) as max_age from [afrancisco].demographics;
/* Create a column Age_group */
alter table [afrancisco].demographics add Age_group nvarchar(50);
/* Create interval for 0-25 */
update [afrancisco].demographics set Age_group='0-25' where Age between 0 and 25;
/* Create interval for 26-50 */
update [afrancisco].demographics set Age_group='26-50' where Age between 26 and 50;
/* Create interval for 51-75 */
update [afrancisco].demographics set Age_group='51-75' where Age between 51 and 75;
/* Create interval for 76-100 */
update [afrancisco].demographics set Age_group='76-100' where Age between 76 and 100;
