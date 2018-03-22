# The original dataset can be downloaded here:
#   https://www.kaggle.com/rtatman/silicon-valley-diversity-data
# Here are some sources to learn more about dplyr and gglpt2
#   dplyr: https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html
#   ggplot2: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

# install required packages
install.packages("dplyr")
install.packages("ggplot2")

# load required packages
library(dplyr)
library(ggplot2)

# load the data
setwd("C:/Users/Karan Shukla/OneDrive/Intro2DataScience")
data <- read.csv("silicon-valley-diversity-data.csv")


# PART 1 - EXPLORE AND CLEAN THE DATA

# view the data
View(data)
str(data)
# This dataset tells us the number of employees 
  # at every company in every role
  # for every combination of race and gender

# clean the data
data <- transform(data, count = as.numeric(count))
data <- filter(data, count != "na")

# find the categories included in the dataset
companies <- unique(data["company"])
job_categories <- unique(data["job_category"])
races <- unique(data["race"])
genders <- unique(data["gender"])

# Suppose we are not interested in job title;
  # we only want to know each company's number of employees 
  # of every race and gender.
# If we explore the dataset, we notice that, 
  # for every company-race-gender demographic,
  # there is a "Totals" row, summing up
  # the number of employees with that race/gender demographic
  # across all job titles.
data_without_job <- filter(data, job_category=="Totals")
View(data_without_job)

# Inspecting the data, we realize that there are rows without gender,
  # corresponding to the "Overall Totals" for each race.
# We can get rid of these rows.
data_without_job <- filter(data_without_job, gender!="")



# PART 2A - FIND THE NUMBER OF EMPLOYEES OF EACH GENDER AT EACH COMPANY

# get a count of gender at each company
data_by_gender <- summarise(
  group_by(data_without_job, company, gender),
  count = sum(count)
)
View(data_by_gender)


# PART 2B - VISUALIZE THE NUMBER OF EMPLOYEES OF EACH GENDER AT EACH COMPANY

# initialize a ggplot2 for gender
plot_gender <- ggplot(data = data_by_gender)

# ggplot2 works by allowing layers to be added on top of each other.
# Let's start by adding a barplot layer
barplot_gender <- plot_gender + geom_bar(
  mapping = aes(x=company, y=count, fill=gender),
  stat="identity"   # 'stat' provides several transormations
)      # we use 'identity', which leaves the data as it is
barplot_gender

# flip the data so it is horizontal
barplot_gender_horizontal <- barplot_gender + coord_flip()
barplot_gender_horizontal

# plot the data as a percentage-based horizontal bar chart
barplot_gender_horizontal_percentage <- plot_gender + geom_bar(
  mapping = aes(x=company, y=count, fill=gender),
  stat="identity",      
  position="fill") +  # 'position' changes where the bars are placed
  coord_flip() + ylab("percentage")
barplot_gender_horizontal_percentage



# PART 3A - FIND THE NUMBER OF EMPLOYEES OF EACH RACE AT EACH COMPANY
data_by_race <- summarise(
  group_by(data_without_job, company, race),
  count = sum(count)
)
View(data_by_race)


# PART 3B - FIND THE NUMBER OF EMPLOYEES OF EACH RACE AT SOCIAL MEDIA COMPANIES
social_media_companies <- c("LinkedIn", "Facebook", "Twitter")
social_media_race <- filter(data_by_race, 
                            company %in% social_media_companies)

# plot the data as a horizontal bar chart
social_media_race_barchart <- ggplot(data = social_media_race) +
                                geom_bar(mapping = aes(x=company, y=count, fill=race),
                                         stat="identity",      
                                         position="dodge") +
                                coord_flip()
social_media_race_barchart