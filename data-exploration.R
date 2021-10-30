library(MASS)
library(openintro)
library(tidyverse)
library(dplyr)
library(data.table)
library(ggbiplot)
# install.packages("devtools")
# devtools::install_github("hodgettsp/cesR")
# install_github("vqv/ggbiplot")
library(cesR)
library(caret)

################################## GET DATA #############################
# pick data for 2019
# survey_data <- read_csv("ces2019-phone_clean.csv") # this reports some warnings, is there a better way to get data
get_ces("ces2019_phone") # we are getting 2019 phone survey data
# no warning from this line #4021 obs of 278 variables.
# coverts categorical variable factors acutomatically 
survey_data <- labelled::to_factor(ces2019_phone) # converts all the columns with labels as factors


################################## Sagnik Data cleaning  #############################

# how many missing values are in the dataset?
sum(is.na(survey_data)) #260346 values. So simply removing missing values are not an option

#get columns with missing values There seems to be 188 columns with missing values
missing_cols <- which(colSums(is.na(survey_data) | survey_data == "") > 0)


sum(is.na(survey_data_clean)) #prints 0 so it has no missing values


#create column vote_liberal from value based on q11
# the answer is we are looking for is (1) Liberal (Grits)     

survey_data_clean <- survey_data[,-which(names(survey_data) %in% names(missing_cols))]

# Remove non numerical answers for
#q2, q69, q25, q24, q22, q21, q20, q19, q18, q16, q15, q14 should be numerical
# these columns has answers such as "Don't know", "Refused" and "skipped" so it makes a categorical variable instead.
numercial_questions <- c( 'q2', 'q69', 'q25', 'q24', 'q22', 'q21', 'q20', 'q19', 'q18', 'q16', 'q15', 'q14')
numeric_rows <- survey_data_clean[numercial_questions]

#supposed to get warning because of NAs produced
survey_data_clean[numercial_questions] <- lapply(survey_data_clean[numercial_questions],
                                                 function(x) as.numeric(as.character(x)))
survey_data_clean <- na.omit(survey_data_clean) #1956 variables maybe we can replace with 0 or -1 or something to keep those rows



#check if all the non numerical values are removed after the loop

# this works but we have to convert this for all numeric columns
# survey_data_clean <- subset(survey_data_clean, grepl('^\\d+$', survey_data_clean$q69))
survey_data_clean <- survey_data_clean %>%
  mutate(age = 2021 -as.numeric(q2), vote_liberal = if_else(vote=='(1) Liberal (Grits)', 1, 0)) %>%
  select(-c(q1, q2, sample_id, survey_end_CES, c3, vote))
  #c3 says can we proceed with the survey
  #q1 says are you a canadian citizens (removing because non citizens can't vote and besides everyone in this survey is a citizen)
  # we can remove both this questions
  # vote tells the party which a person has either voted for or are likely to vote
  # ideally we would like to make a multnomial regression to preidict which party wins instead of one party 


# now there are several different approaches we can take to stratify the data
# my idea is that we do step-wise regression on all the 89 columns to identify which variables are statistically significant for prediction.
# we also have to make sure these variables are present in the census data as well
# Then from there we can do clustering based on the significant variables

# check for columns with only one label and add it the removed list of columns
which(sapply(survey_data_clean, function(col) length(unique(col))) == 1)


# use PCA to find out which variables are important for grouping the data
# which variables make one group differ from another. This can only be done for numeric data
numeric_cols <- select_if(survey_data_clean, is.numeric) #18 variables
survey_data.pca <- prcomp(numeric_cols, center = TRUE, scale. = TRUE)
summary(survey_data.pca) 
# the top two only explain 26% and 12 percent of the variance. this means that numerical values are not that important? 



######################################################### MODEL building ####################################
# full.model <- glm(vote_liberal ~ ., data = survey_data_clean, family = binomial) # did not converge, very high P-value

# step-wise regression
null.model <- glm(vote_liberal ~ 1, data = survey_data_clean, family = binomial)
vars <- colnames(survey_data_clean[1:84])
formula <- as.formula(paste('vote_liberal', paste(vars, collapse = ' + '), sep = ' ~ '))
step.model <- step(null.model, direction = "both", k=2, scope=formula) #gave a model but came with warning
step_coef_info <- data.frame(summary(step.model)$coefficients) %>% select(Estimate, "Pr...z..") %>% rename(P_value = Pr...z..)

# forward step-wise model
forward.model <- step(null.model, direction = "forward", k=2, scope=formula) # gives warning
forward_coef_info <- data.frame(summary(forward.model)$coefficients) %>% select(Estimate, "Pr...z..") %>% rename(P_value = Pr...z..)

# lasso regresion model
y <- survey_data_clean$vote_liberal
x <- data.matrix(survey_data_clean[, -which(names(survey_data_clean) == 'vote_liberal')])

library(glmnet)
#use misclassification as criterion for 10-fold cross validation
lass.model <- cv.glmnet(x, y, family = "binomial", type.measure = "class")
plot(lass.model)
best_lambda <- lass.model$lambda.min
lasso.coefs <- coef(lass.model, s= "lambda.min")

lasso.coefs_table <- data.frame(
  features = lasso.coefs@Dimnames[[1]][ which(lasso.coefs != 0 ) ], #intercept included
  coefs    = lasso.coefs[ which(lasso.coefs != 0 ) ]  #intercept included
)

library(Hmisc)
lasso.coefs_table$label <- rep('', nrow(lasso.coefs_table))
lasso.coefs_table$label[-1] <- survey_data %>% select(lasso.coefs_table$features[-1]) %>% label
################################## Sagnik section end #############################

