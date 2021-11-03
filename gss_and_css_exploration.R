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
library(Hmisc)
library(purrr)


gss_data <- read.csv('./gss_clean.csv')
get_ces("ces2019_phone") # we are getting 2019 phone survey data
# no warning from this line #4021 obs of 278 variables.
# coverts categorical variable factors acutomatically 
survey_data <- labelled::to_factor(ces2019_phone) # converts all the columns with labels as factors

ces_labelNames <- label(survey_data)
gss_colNames <- colnames(gss_data)

ces_strings <- str_split(ces_labelNames, ' ')
gss_strings <- str_split(gss_colNames, '_')

# get common words for each col
common_words <- intersect(unlist(gss_strings), unlist(ces_strings))
common_words <- c(common_words, c("age"))
#"first"        "total"        "at"           "work"         "place"        "province"     "year"         "status"      
# "education"    "type"         "country"      "last"         "main"         "religion"     "has"          "language"    
# "home"         "income"       "respondent"   "occupation"   "ever"         "number"       "of"           "union"       
# "with"         "in"           "household"    "grandparents" "still"        "is"           "time"         "off"         
# "satisfied"    "or"           "other"        "age"

#so the important words are
# we are removing status because everyone in the survey is a citizen
important_words <- c("work", "place", "province", "year", "language", "religion", "education", "age", "home")

gss_common_data <- gss_data %>% select(contains(important_words)) # contains many redundant variables
ces_common_data <- survey_data %>% select(contains(important_words)) # this needs manual investigations

# which labels contain the important words. look at the dictionary
ces_label_dict <- data.frame(colName = colnames(survey_data), labelName = label(survey_data))

#common questions
# age   (done)
# q3 gender note, age and gender is not the same. So we are only reporting the models by the ppl who identify both as the same (done)
# q4 province they're living in (done)
# q63 is religion important to you and religion_importance (done)
# q66a_15 Inuit, Metis, Aboriginal: To what ethnic or cultural group do you belong (done)
# q68 employment status (need clean up at the source, Skip for now)
# q52 occupation (Skip)
# q61 Highest level of education completed (done)
# p57 in survey_data and pop_center gss_data mean the same thing, but the mapping has to be modified (TBD)
# q71 and hh_size
# q70r household income, but both datasets have different levels so, some transformtation is required (Skip for now)

# transform the data in survey_data and gss_data

#age
survey_data_clean <- survey_data
sum(is.na(survey_data$age))

#gender cleaning
survey_data_clean <- survey_data %>% 
  filter(q3 == '(1) Male' | q3 == '(2) Female') %>%
  mutate(gender = if_else(q3 == '(1) Male', 'Male', 'Female')) %>%
  dplyr::rename(sex = q3)

#q4 province
survey_data_clean <- survey_data_clean %>% dplyr::rename(province = q4)
sum(is.na(survey_data_clean$province))

#q63 religion_imporatance
survey_data_clean$q63 <- str_sub(survey_data_clean$q63, 5)
survey_data_clean <- survey_data_clean %>%
  dplyr::rename(religion_importance = q63) %>%
  filter(!is.na(religion_importance) & religion_importance != " Refused") %>%
  mutate(religion_importance = trimws(religion_importance, 'l'))

gss_data <- gss_data %>% 
  dplyr::rename(religion_importance = regilion_importance) %>%
  filter(!is.na(religion_importance))


sum(is.na(survey_data_clean$q65)) #2166 out of 2576 NA, we will not use this variable
nrow(survey_data_clean)

#aboriginal
survey_data_clean <- survey_data_clean %>%
  dplyr::rename(aboriginal = q66a_15) %>%
  filter(aboriginal == '(1) Selected' | aboriginal == '(0) Not Selected') %>%
  mutate(aboriginal = if_else(aboriginal == '(1) Selected', 'Yes', 'No'))
#there are few data points saying "Don't know", so it was not worth recording

gss_data <- gss_data %>% filter(aboriginal == 'Yes' | aboriginal == 'No')

#employment status 91% of this is NA, other col main_activity 
#is also NA without clean-up at the gss_cleaning.R this info is not usable

#occupation there are no common occupation that has a string match skip for now

#education level 
survey_data_clean <- survey_data_clean %>%
  dplyr::rename(education = q61)

survey_data_clean <- survey_data_clean %>%
  filter(education != "(-8) Refused" & education != "(-9) Don't know") %>%
  mutate(education = case_when(
    education == "(9) Bachelor's degree" ~ "Bachelor",
    education == "(10) Master's degree" ~ "Above Bachelor",
    education == "(11) Professional degree or doctorate" ~ "Above Bachelor",
    TRUE ~ "Below Bachelor"
  ))

# map each category from gss to a category
# "High school diploma or a high school equivalency certificate" "Trade certificate or diploma"                                
# "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)"                  "College, CEGEP or other non-university certificate or di..." 
# "Less than high school diploma or its equivalent"              "University certificate or diploma below the bachelor's level"
# "University certificate, diploma or degree above the bach..."

gss_data <- gss_data %>%
  filter(!is.na(education)) %>%
  mutate(education = case_when(
    education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ "Bachelor",
    education == "University certificate, diploma or degree above the bach.." ~ "Above Bachelor",
    TRUE ~ "Below Bachelor",
  ))

#hh_size
survey_data_clean$q71<- as.numeric(as.character(survey_data_clean$q71))
survey_data_clean <- survey_data_clean %>%
  filter(!is.na(q71)) %>%
  dplyr::rename(hh_size = q71)

common_cols <- intersect(colnames(survey_data_clean), colnames(gss_data))

gss_common_data <- gss_data[, common_cols]
survey_common_data <- survey_data_clean[, common_cols]

# add vote variable for survey_data
survey_common_data$vote <- survey_data_clean$vote
survey_common_data <- survey_common_data %>%
  mutate(vote = case_when(
    vote == "(1) Liberal (Grits)" ~ 'Liberal',
    vote == "(3) NDP (New Democratic Party, New Democrats, NDPers)" ~ 'NDP',
    vote == "(2) Conservatives (Tory, PCs, Conservative Party of Canada)" ~ 'Conservatives',
    vote == "(5) Green Party (Greens)" ~ 'Greens',
    vote == "(6) People's Party" ~ "People's Party",
    vote == "(4) Bloc Québécois (BQ, PQ, Bloc, Parti Québécois)" ~ "Bloc Quebecois",
  ))

survey_common_data <- na.omit(survey_common_data)
  
write_csv(gss_common_data, "gss_common_data.csv")
write_csv(survey_common_data, "survey_common_data.csv")
  

