url <- "https://cran.r-project.org/src/contrib/Archive/rtimes/rtimes_0.5.0.tar.gz"
install.packages(url, repos=NULL, type='source')
library(rtimes)

# upload the medications csv to get a list of medications
setwd("/Users/frantennis/Desktop")
getwd()
demographics_medications <- read.csv("demographics_medications_ratings.csv")
demographics_medications

# Just take a look at the drugs_com dataset
drugs_com <- read.table(file= 'drugsComTrain_raw.tsv', sep='\t', header=TRUE, blank.lines.skip = FALSE)
wellbutrin_reviews <- drugs_com[drugs_com$drugName == "Wellbutrin", ]

# Get all the unique drug names from the dataset
drug_names <- unique(demographics_medications$RXDDRUG)
key <- "72TG8593IytiDBELXHszCj7NiOAL0hV9"


# Top 10 most common prescription medications
drugNameFreqs <- sort(table(demographics_medications$RXDDRUG))
top10drugs <- tail(names(drugNameFreqs), 10) 


# Get all of the articles between the year 2016-2019 for each of the top 10 drugs
article_search1 <- as_search(q=top10drugs[1], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search2 <- as_search(q=top10drugs[2], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search3 <- as_search(q=top10drugs[3], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search4 <- as_search(q=top10drugs[4], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search5 <- as_search(q=top10drugs[5], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search6 <- as_search(q=top10drugs[6], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search7 <- as_search(q=top10drugs[7], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search8 <- as_search(q=top10drugs[8], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search9 <- as_search(q=top10drugs[9], key=key, begin_date="2013-01-01", end_date="2018-01-01")
article_search10 <- as_search(q=top10drugs[10], key=key, begin_date="2013-01-01", end_date="2018-01-01")


HYDROCHLOROTHIAZIDE_dataset <- article_search1$data
METOPROLOL_dataset <- article_search2$data
ALBUTEROL_dataset <- article_search3$data
OMEPRAZOLE_dataset <- article_search4$data
AMLODIPINE_dataset <- article_search5$data
ATORVASTATIN_dataset <- article_search6$data
SIMVASTATIN_dataset <- article_search7$data
LEVOTHYROXINE_dataset <- article_search8$data
METFORMIN_dataset <- article_search9$data
LISINOPRIL_dataset <- article_search10$data

# Get all the abstracts for each drug
all_abstracts <- c(HYDROCHLOROTHIAZIDE_dataset$abstract, METOPROLOL_dataset$abstract, ALBUTEROL_dataset$abstract, 
                               OMEPRAZOLE_dataset$abstract, AMLODIPINE_dataset$abstract, ATORVASTATIN_dataset$abstract,
                               SIMVASTATIN_dataset$abstract, LEVOTHYROXINE_dataset$abstract, METFORMIN_dataset$abstract,
                               LISINOPRIL_dataset$abstract)

# Get all of the lead paragraphs for each drug
all_lead_paragraphs <- c(HYDROCHLOROTHIAZIDE_dataset$lead_paragraph, METOPROLOL_dataset$lead_paragraph, ALBUTEROL_dataset$lead_paragraph, 
                         OMEPRAZOLE_dataset$lead_paragraph, AMLODIPINE_dataset$lead_paragraph, ATORVASTATIN_dataset$lead_paragraph,
                         SIMVASTATIN_dataset$lead_paragraph, LEVOTHYROXINE_dataset$lead_paragraph, METFORMIN_dataset$lead_paragraph,
                         LISINOPRIL_dataset$lead_paragraph)

# Do the dates with the prescription data
METOPROLOL_dataset$pub_date
METOPROLOL_dataset$keywords
all_dates <- c(HYDROCHLOROTHIAZIDE_dataset$pub_date, METOPROLOL_dataset$pub_date, ALBUTEROL_dataset$pub_date, 
               OMEPRAZOLE_dataset$pub_date, AMLODIPINE_dataset$pub_date, ATORVASTATIN_dataset$pub_date,
               SIMVASTATIN_dataset$pub_date, LEVOTHYROXINE_dataset$pub_date, METFORMIN_dataset$pub_date,
               LISINOPRIL_dataset$pub_date)

# Create a data frame that combines all of the abstracts, lead paragraphs, dates
NYT_data <- data.frame("abstract" = all_abstracts, "lead_paragraph" = all_lead_paragraphs, "pub_date" = all_dates)

# Create a column that labels which drug the article is about
NYT_data["drug_name"] <- NA
NYT_data[NYT_data$abstract == HYDROCHLOROTHIAZIDE_dataset$abstract, ]$drug_name <- "HYDROCHLOROTHIAZIDE"
NYT_data[NYT_data$abstract == METOPROLOL_dataset$abstract, ]$drug_name <- "METOPROLOL"
NYT_data[NYT_data$abstract == ALBUTEROL_dataset$abstract, ]$drug_name <- "ALBUTEROL"
# Create a column with the number of articles written for each specific drug
length(METOPROLOL_dataset$abstract)
# Label METOPROLOL
i <- 1
while (i <= length(METOPROLOL_dataset$abstract)){
  print(METOPROLOL_dataset$abstract[i])
  NYT_data[NYT_data$abstract == METOPROLOL_dataset$abstract[i], ]$drug_name <- "METOPROLOL"
  i <- i + 1
}

# Label ALBUTEROL
i <- 1
while (i <= length(ALBUTEROL_dataset$abstract)){
  print(ALBUTEROL_dataset$abstract[i])
  NYT_data[NYT_data$abstract == ALBUTEROL_dataset$abstract[i], ]$drug_name <- "ALBUTEROL"
  i <- i + 1
}

# Label OMEPRAZOLE
i <- 1
while (i <= length(OMEPRAZOLE_dataset$abstract)){
  print(OMEPRAZOLE_dataset$abstract[i])
  NYT_data[NYT_data$abstract == OMEPRAZOLE_dataset$abstract[i], ]$drug_name <- "OMEPRAZOLE"
  i <- i + 1
}

# Label AMLODIPINE
i <- 1
while (i <= length(AMLODIPINE_dataset$abstract)){
  print(AMLODIPINE_dataset$abstract[i])
  NYT_data[NYT_data$abstract == AMLODIPINE_dataset$abstract[i], ]$drug_name <- "AMLODIPINE"
  i <- i + 1
}

# Label ATORVASTATIN
i <- 1
while (i <= length(ATORVASTATIN_dataset$abstract)){
  print(ATORVASTATIN_dataset$abstract[i])
  NYT_data[NYT_data$abstract == ATORVASTATIN_dataset$abstract[i], ]$drug_name <- "ATORVASTATIN"
  i <- i + 1
}

# Label SIMVASTATIN
i <- 1
while (i <= length(SIMVASTATIN_dataset$abstract)){
  print(SIMVASTATIN_dataset$abstract[i])
  NYT_data[NYT_data$abstract == SIMVASTATIN_dataset$abstract[i], ]$drug_name <- "SIMVASTATIN"
  i <- i + 1
}

# Label LEVOTHYROXINE
i <- 1
while (i <= length(LEVOTHYROXINE_dataset$abstract)){
  print(SIMVASTATIN_dataset$abstract[i])
  NYT_data[NYT_data$abstract == LEVOTHYROXINE_dataset$abstract[i], ]$drug_name <- "LEVOTHYROXINE"
  i <- i + 1
}

# Label METFORMIN
i <- 1
while (i <= length(METFORMIN_dataset$abstract)){
  print(METFORMIN_dataset$abstract[i])
  NYT_data[NYT_data$abstract == METFORMIN_dataset$abstract[i], ]$drug_name <- "METFORMIN"
  i <- i + 1
}

# Label LISINOPRIL
i <- 1
while (i <= length(LISINOPRIL_dataset$abstract)){
  print(LISINOPRIL_dataset$abstract[i])
  NYT_data[NYT_data$abstract == LISINOPRIL_dataset$abstract[i], ]$drug_name <- "LISINOPRIL"
  i <- i + 1
}

# Alter the dates so its of the standard R form
test_date <- as.POSIXlt(ALBUTEROL_dataset$pub_date)
NYT_data$pub_date <- as.POSIXlt(NYT_data$pub_date)
# new column for the year
NYT_data["Year"] <- NA
NYT_data$Year <- strftime(NYT_data$pub_date, "%Y") 

# Plot of the number of NYT articles per year grouped by Drug
library(ggplot2)
ggplot(data=NYT_data, aes(x=Year, fill=drug_name)) + geom_bar() +
  geom_text(aes(label=..count..),stat="count",position=position_stack(0.5)) + ggtitle("NYT articles per year by Drug")

ggplot(data=NYT_data, aes(x=Year, fill=drug_name)) + geom_bar(position="dodge") +
  geom_text(aes(label=..count..),stat="count",position=position_dodge(0.9), vjust=-0.2) + ggtitle("NYT articles per year by Drug")


# function to get all of the article subjects (from the keywords list)
get_subjects <- function(dataset_list){
  subject_list <- c()
  # Loop through datasets in the list
  for (dataset in dataset_list){
    # get the length of articles for the keywords
    print("im here 1")
    num_articles <- length(dataset$keywords)
    i <- 1
    while (i <= num_articles) {
      print("im here 2")
      dataset_keysubjects <-
        dataset$keywords[[i]][dataset$keywords[[i]]$name == "subject", ]$value
      subject_list <- append(dataset_keysubjects, subject_list)
      i <- i + 1
    }
  }
  return(subject_list)
}

# all datasets
all_datasets <- list(HYDROCHLOROTHIAZIDE_dataset, METOPROLOL_dataset, ALBUTEROL_dataset, 
                   OMEPRAZOLE_dataset, AMLODIPINE_dataset, ATORVASTATIN_dataset,
                   SIMVASTATIN_dataset, LEVOTHYROXINE_dataset, METFORMIN_dataset,
                   LISINOPRIL_dataset)

all_subjects <- get_subjects(all_datasets)

# Create a wordcloud for the words
library(wordcloud)
wordcloud(all_subjects, colors = c("Black", "Blue", "Red"))


library(sqldf)
as.data.frame(NYT_data)
#Need to coerce all of vars into chars before I can use sql
str(NYT_data)
NYT_data$abstract <- as.character(NYT_data$abstract)
NYT_data$lead_paragraph <- as.character(NYT_data$lead_paragraph)
NYT_data$pub_date <- as.character(NYT_data$pub_date)
str(NYT_data)


all_wordCounts <- c(HYDROCHLOROTHIAZIDE_dataset$word_count, METOPROLOL_dataset$word_count, ALBUTEROL_dataset$word_count, 
                    OMEPRAZOLE_dataset$word_count, AMLODIPINE_dataset$word_count, ATORVASTATIN_dataset$word_count,
                    SIMVASTATIN_dataset$word_count, LEVOTHYROXINE_dataset$word_count, METFORMIN_dataset$word_count,
                    LISINOPRIL_dataset$word_count)
NYT_data["word_count"] <- NA
NYT_data$word_count <- all_wordCounts
NYT_data$word_count <- as.character(NYT_data$word_count)
articles_byDrug <- sqldf("Select avg(word_count) as avg_word_count, drug_name from NYT_data group by drug_name")

# Bar plot of the average word counts by drug_name
ggplot(data=articles_byDrug, aes(x=drug_name, y=avg_word_count, color=drug_name)) + geom_bar(stat="identity", fill="white") 
+ coord_flip() + xlab("Average article word count") + ylab("Drug Name") + ggtitle("Average Article Word Count by Drug")
