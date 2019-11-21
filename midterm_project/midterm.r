# Load in packages
install.packages("Hmisc")
install.packages("foreign")
library(foreign)
library(Hmisc)
library(sqldf)

DIQ_data <- read.xport("DIQ_I (1).XPT")
str(DIQ_data)

# Inspect the data
# Look at mins/maxes for each column
mins <- apply(DIQ_data, 2, min)
maxes <- apply(DIQ_data, 2, max)

mins
maxes

# Verify that the counts of each code or value for various variables are correct as mentioned on website
# Var 1 --> DIQ010
# Create a new column, just for labeling
DIQ_data["DIQ010_val"] <- NA
DIQ_data$DIQ010_val[DIQ_data$DIQ010 == 1] <- "Yes"
DIQ_data$DIQ010_val[DIQ_data$DIQ010 == 2] <- "No"
DIQ_data$DIQ010_val[DIQ_data$DIQ010 == 3] <- "Borderline"
DIQ_data$DIQ010_val[DIQ_data$DIQ010 == 7] <- "Refused"
DIQ_data$DIQ010_val[DIQ_data$DIQ010 == 9] <- "Don't know"
DIQ010_count <- sqldf("select DIQ010, DIQ010_val as Value_Description, count(DIQ010) as Count from
                         DIQ_data group by DIQ010")
DIQ010_count

# Var 2 --> DID040
DIQ_data["DID040_val"] <- NA
DIQ_data$DID040_val[DIQ_data$DID040 >=2 && DIQ_data$DID040 <= 78] <- "Range of Values"
DIQ_data$DID040_val[DIQ_data$DID040 == 80] <- "80 years or older"
DIQ_data$DID040_val[DIQ_data$DID040 == 666] <- "Less than 1 year"
DIQ_data$DID040_val[DIQ_data$DID040 == 999] <- "Don't know"
DIQ_data$DID040_val[is.na(DIQ_data$DID040)] <- "Missing"
DID040_count <- sqldf("select DID040, DID040_val as Value_Description, count(DID040_val) as Count from
                         DIQ_data group by DID040_val")

DID040_count


# Var 3 --> DIQ172
DIQ_data["DIQ172_val"] <- NA
DIQ_data$DIQ172_val[DIQ_data$DIQ172 == 1] <- "Yes"
DIQ_data$DIQ172_val[DIQ_data$DIQ172 == 2] <- "No"
DIQ_data$DIQ172_val[DIQ_data$DIQ172 == 7] <- "Refused"
DIQ_data$DIQ172_val[DIQ_data$DIQ172 == 9] <- "Don't know"
DIQ_data$DIQ172_val[is.na(DIQ_data$DIQ172)] <- "Missing"
DIQ172_count <- sqldf("select DIQ172, DIQ172_val as Value_Description, count(DIQ172_val) as Count from
                         DIQ_data group by DIQ172")

DIQ172_count

# Var 4 --> DIQ160
DIQ_data["DIQ160_val"] <- NA
DIQ_data$DIQ160_val[DIQ_data$DIQ160 == 1] <- "Yes"
DIQ_data$DIQ160_val[DIQ_data$DIQ160 == 2] <- "No"
DIQ_data$DIQ160_val[DIQ_data$DIQ160 == 7] <- "Refused"
DIQ_data$DIQ160_val[DIQ_data$DIQ160 == 9] <- "Don't know"
DIQ_data$DIQ160_val[is.na(DIQ_data$DIQ160)] <- "Missing"
DIQ160_count <- sqldf("select DIQ160, DIQ160_val as Value_Description, count(DIQ160_val) as Count from
                         DIQ_data group by DIQ160")

DIQ160_count

# Var 5 --> DIQ260U 
DIQ_data["DIQ260U_val"] <- NA
DIQ_data$DIQ260U_val[DIQ_data$DIQ260U == 1] <- "Per day"
DIQ_data$DIQ260U_val[DIQ_data$DIQ260U == 2] <- "Per week"
DIQ_data$DIQ260U_val[DIQ_data$DIQ260U == 3] <- "Per month"
DIQ_data$DIQ260U_val[DIQ_data$DIQ260U == 4] <- "Per year"
DIQ_data$DIQ260U_val[is.na(DIQ_data$DIQ260U)] <- "Missing"
DIQ260U_count <- sqldf("select DIQ260U, DIQ260U_val as Value_Description, count(DIQ260U_val) as Count from
                         DIQ_data group by DIQ260U")

DIQ260U_count

# print 10 random rows with the changes made to the data frame (added labels)
DIQ_data[sample(nrow(DIQ_data), 10), ]

# Address Missingness in dataset
# Determine which columns have 50% or more missingness 
exclude_vars <- colnames(DIQ_data)[colSums(is.na(DIQ_data)) >= (0.5*9575)]
exclude_vars

# Exclude columns that have 50% or more missingness
DIQ_data_2 <- DIQ_data[, !colnames(DIQ_data) %in% exclude_vars]
# Since there would only be 7 variables left in the data set, need to handle missingness
# by doing one hot encoding on a majority of the variables

# Separate categorical and continuous variables
find_cont_vars <- function(factor_levels){
  cont_vars<- c()
  factor_level_names <- names(factor_levels)
  for (name in factor_level_names){
    if (factor_levels[name] > 8){
      cont_vars <- append(cont_vars, name)
    }
  }
  return(cont_vars)
}

# factor all the variables and determine which one's have > 8 levels, then double check if those are continuous vars
DIQ_all_factored <- data.frame(lapply(DIQ_data, factor))
factored_levels <- sapply(DIQ_all_factored[,sapply(DIQ_all_factored, is.factor)], nlevels)
factored_levels

# once figure out continous vars, then place in their own vector for now
cont_vars <- find_cont_vars(factored_levels)
cont_vars

# Do one hot encoding for the categorical variables that have missingness
categorical_vars <- DIQ_all_factored[, !colnames(DIQ_all_factored) %in% cont_vars]
categorical_vars

# Get the categorical variables with ony 1 level
find_one_level <- function(factor_levels){
  one_level_vars<- c()
  factor_level_names <- names(factor_levels)
  for (name in factor_level_names){
    if (factor_levels[name] == 1){
      one_level_vars <- append(one_level_vars, name)
    }
  }
  return(one_level_vars)
}

cat_vars_1level <- find_one_level(factored_levels)
cat_vars_1level

# Split into variable_present and variable_unknown
# create new variable endings
new_cat_present <- paste(cat_vars_1level, "_present", sep="") 
new_cat_unknown <- paste(cat_vars_1level, "_unknown", sep="")
new_cat_colnames <- c(new_cat_present, new_cat_unknown) # combine all names into a single list

# Add new columns to data frame
categorical_vars[new_cat_colnames] <- NA

# Changes the present variables to 1 if not missing and 0 if missing and vice versa for unknown variables
# These are only for the variables that only have 2 categories 

for (name in cat_vars_1level){

  # Change the values of the variable_present
  var_present <- paste(name, "_present", sep="")
  edited_var_pres <- categorical_vars[, var_present]
  actual_var_pres <- categorical_vars[,name]
  
  edited_var_pres[!is.na(actual_var_pres)] <- 1
  edited_var_pres[is.na(actual_var_pres)] <- 0
  categorical_vars[, var_present] <- edited_var_pres
  
  # Change the values of the variable_unknown
  var_unknown <- paste(name, "_unknown", sep="")
  edited_var_unk <- categorical_vars[, var_unknown]
  actual_var_unk <- categorical_vars[,name]
  
  edited_var_unk[is.na(actual_var_unk)] <- 1
  edited_var_unk[!is.na(actual_var_unk)] <- 0
  categorical_vars[, var_unknown] <- edited_var_unk
  
}

# Take out all of the columns with the NA's now to avoid redundancy
categorical_vars[cat_vars_1level] <- NULL

# separate the rest of the categorical vars to do one-hot encoding on them
cat_vars_morethan1_level <- categorical_vars[, !colnames(categorical_vars) %in% new_cat_colnames]
categorical_vars_1_level <- categorical_vars[, !colnames(categorical_vars) %in% names(cat_vars_morethan1_level)]
# Take out the labeled variables
del_labels <- c("DIQ010_val", "DID040_val", "DIQ172_val")
cat_vars_morethan1_level[del_labels] <- NULL

install.packages("fastDummies")
library(fastDummies)
cat_vars_w_dummyvars <- dummy_cols(cat_vars_morethan1_level)
for (name in names(cat_vars_w_dummyvars)){
  cur_var <- cat_vars_w_dummyvars[,name]
  cur_var[is.na(cur_var)] <- 0
  cat_vars_w_dummyvars[,name] <- cur_var
}

#Get only the dummy variables and take out the rest of the variables with the missing variables
exclude_rest_cat <- names(cat_vars_morethan1_level)
cat_vars_w_dummyvars[exclude_rest_cat] <- NULL

# combine all the categorical variables --> the ones with one level and the ones with more than 1 level
cleaned_categorical_vars <- data.frame(categorical_vars_1_level, cat_vars_w_dummyvars)

# Do mean imputation for the continous variables that have missingness

# Check for outliers
cont_var_data <- na.omit(DIQ_data[cont_vars])
mins <- apply(cont_var_data, 2, min)
maxes <- apply(cont_var_data, 2, max)

mins
maxes

# This will be the data frame that will be used to hold all of the cleaned variables
cont_vars_data<- DIQ_data[cont_vars]
# Use this data to calculate the mean by excluding outliers such as 6666, 5555, and 666
cont_vars_data2 <- DIQ_data[cont_vars]

# For the sake of calculating an accurate mean, change all of the 7777, 999, 9999, and 6666 to NAs for now
# Determine which values to change by inspection of data frame
cont_vars_data[cont_vars_data == 999] <- NA
cont_vars_data[cont_vars_data == 777] <- NA
cont_vars_data[cont_vars_data == 9999] <- NA
cont_vars_data[cont_vars_data == 7777] <- NA

cont_vars_data2[cont_vars_data2 == 999] <- NA
cont_vars_data2[cont_vars_data2 == 777] <- NA
cont_vars_data2[cont_vars_data2 == 9999] <- NA
cont_vars_data2[cont_vars_data2 == 7777] <- NA
cont_vars_data2[cont_vars_data2 == 666] <- NA
cont_vars_data2[cont_vars_data2 == 6666] <- NA
cont_vars_data2[cont_vars_data2 == 5555] <- NA


# Since some maxes include "999" or "9999", do not include these values when calculating the mean then do mean imputation
# This is a variable where 666 stands for "Less than 1 year"
DID040_mean <- mean(cont_vars_data2$DID040, na.rm = TRUE)
cont_vars_data$DID040[is.na(cont_vars_data$DID040)] <- DID040_mean
# This is a variable where 666 stands for "Less than 1 month"
DID060_mean <- mean(cont_vars_data2$DID060, na.rm = TRUE)
cont_vars_data$DID060[is.na(cont_vars_data$DID060)] <- DID060_mean
cont_vars_data$DID250[is.na(cont_vars_data$DID250)]<- mean(cont_vars_data$DID250, na.rm = TRUE)
cont_vars_data$DID260[is.na(cont_vars_data$DID260)]<- mean(cont_vars_data$DID260, na.rm = TRUE)
cont_vars_data$DIQ280[is.na(cont_vars_data$DIQ280)] <- mean(cont_vars_data$DIQ280, na.rm = TRUE)
cont_vars_data$DIQ300S[is.na(cont_vars_data$DIQ300S)] <- mean(cont_vars_data$DIQ300S, na.rm = TRUE)
cont_vars_data$DIQ300D[is.na(cont_vars_data$DIQ300D)]  <- mean(cont_vars_data$DIQ300D, na.rm = TRUE)
# This is a variable where 6666 stands for "Provider did not specify goal"
DID310S_mean <- mean(cont_vars_data2$DID310S, na.rm = TRUE)
cont_vars_data$DID310S[is.na(cont_vars_data$DID310S)]<- DID310S_mean
# This is a variable where 6666 stands for "Provider did not specify goal"
DID310D_mean <- mean(cont_vars_data2$DID310D, na.rm = TRUE)
cont_vars_data$DID310D[is.na(cont_vars_data$DID310D)]<- DID310D_mean
# This is a variable where 6666 stands for Never had cholesterol test and 5555 stands for Never heard of LDL
DID320_mean <- mean(cont_vars_data2$DID320, na.rm = TRUE)
cont_vars_data$DID320[is.na(cont_vars_data$DID320)]<- DID320_mean
# This is a variable where 6666 stands for Provider did not specify goal
DID330_mean <- mean(cont_vars_data2$DID330, na.rm = TRUE)
cont_vars_data$DID330[is.na(cont_vars_data$DID330)]<- DID330_mean
cont_vars_data$DID341[is.na(cont_vars_data$DID341)] <- mean(cont_vars_data$DID330, na.rm = TRUE)
cont_vars_data$DID350[is.na(cont_vars_data$DID350)] <-mean(cont_vars_data$DID350, na.rm = TRUE)

# Combine all variables
# Factor the categorical variables
DIQ_final_factored <- data.frame(lapply(cleaned_categorical_vars, factor))
# Combine the factored categorical variables and the continous variables in one dataframe
DIQ_cleaned_final <- data.frame(DIQ_final_factored, cont_vars_data)
str(DIQ_cleaned_final)

# print 10 random rows with these changes
DIQ_cleaned_final[sample(nrow(DIQ_cleaned_final), 10), ]

# Visualize the data
library(ggplot2)
ggplot(DIQ_cleaned_final, aes(x=DID040, color=DIQ175G_present)) + geom_histogram(binwidth = 10) +  xlab("Age diagnosed") 
+ ggtitle("Age Diagnosed with Diabetes by Exercise or No Exercise")