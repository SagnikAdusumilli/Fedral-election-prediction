# Here is a resource for grabbing the CES2019 data:
# https://awstringer1.github.io/sta238-book/section-short
# -tutorial-on-pulling-data-for-assignment-1.html#section-
# canadian-election-study                          

library(tidyverse)

# In this code you can load in and clean the survey data 

# First, if you don't already have it, install the devtools package:
# install.packages("devtools")

# Now use devtools to install the cesR package directly from Github:
# devtools::install_github("hodgettsp/cesR")

# Load it like any other package:
library(cesR)

# There are many different CES datasets, and they have unique codes. 
# See them with the get_cescodes() function:

get_cescodes()

# Now pick one, let's try ces2019_phone

get_ces("ces2019_phone")

survey_data <- ces2019_phone

write_csv(survey_data, "ces_clean.csv")
