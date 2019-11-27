/* Question 2: Merge the tables Demographics, Conditions and TextMessages. 
Obtain the final dataset such that we have 1 Row per ID by choosing on the latest date 
when the text was sent */

/* Copy in tables to edit */
select *
into [afrancisco].[conditions]
from [qbs181].dbo.Conditions
select *
into [afrancisco].[TextMessages]
from [qbs181].dbo.TextMessages

/* Get the distinct conditions */
select distinct(tri_name)
from [afrancisco].conditions

/* Collapse rows with the same id into 1 row without the tri_name */
/* One hot encode the conditions and create new table */
select tri_patientid as ID,
    count(case when tri_name='Activity Monitoring' then 1 end) as ActivityMonitoring,
    count(case when tri_name='Congestive Heart Failure' then 1 end) as CongestiveHeartFailure,
    count(case when tri_name='Hypertension' then 1 end) as Hypertension,
    count(case when tri_name='COPD' then 1 end) as COPD,
    count(case when tri_name='Diabetes' then 1 end) as Diabetes
into [afrancisco].onehotconditions
from [afrancisco].conditions
group by tri_patientid;

/* Create table of the latest text sent date */
select *
from [afrancisco].TextMessages;

select tri_contactId, max(TextSentDate) as latestSentDate
into [afrancisco].latestTextMessages
from [afrancisco].TextMessages
group by tri_contactId;

select *
from [afrancisco].latestTextMessages;

/* Merge the onehotconditions, the demographics table, and the latestTextMessages table and name as final table*/
select *
into [afrancisco].finalTable
from (
select A.*, B.ActivityMonitoring, B.CongestiveHeartFailure, B.COPD, B.Diabetes,
        B.Hypertension, C.latestSentDate
    from [afrancisco].demographics A
        inner join [afrancisco].onehotconditions B on A.ID=B.ID
        inner join [afrancisco].latestTextMessages C on B.ID=C.tri_contactId) as finalTable;

select *
from [afrancisco].finalTable;

/* Print random 10 rows to show these changes */
select top 10
    *
from [afrancisco].finalTable
order by newid();
