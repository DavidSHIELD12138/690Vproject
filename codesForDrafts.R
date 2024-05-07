
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

filename1="DACSS 690V Data.csv"
Data.HQ=read.csv(filename1)
filename2="DACSS 690V Map Data.csv"
data=read.csv(filename2)

library(flexdashboard)
library(tidyverse)
library(readxl)
library(ggplot2)
library(car)
library(dplyr)
library(purrr)
library(sf)
library(maps)

# see data ----------------------------------------------------------


head(Data.HQ)
head(data)

# see data types ----------------------------------------------------------

str(Data.HQ)
str(data)

# deliverable 1 ----------------------------------------------------------
#Categorical Variable: Plotting a barplot of the "Fav genre" variable

del1Draft = ggplot(Data.HQ, aes(x=reorder(`Fav.genre`, `Fav.genre`, function(x) -length(x)))) + 
  geom_bar(aes(fill="orange"), color="black", stat="count") +
  geom_text(stat='count', aes(label=..count.., y=..count..), vjust=-0.5, color="black") +
  theme_minimal() +
  labs(title="Favorite Genres in Descending Order",
       subtitle="An Analysis of Music Genre Preferences (U.S.A., 2024)",
       x="Genre",
       y="Frequency",
       caption="Source: Music & Mental Health Survey Results. Annotations: Data visualized as of 04/2024.") +
  scale_fill_identity() + 
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  annotate("text", x=1, y=max(table(Data.HQ$`Fav genre`))-10, label="", color="red")
del1Draft


# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------

# Numeircal Variable: Plotting a Histogram of the "age" variable

# Filter out non-finite values in the Age column
Data.HQ_Cleaned <- Data.HQ %>%
  filter(is.finite(Age))

# Recalculate suitable x and y positions for the annotation based on cleaned data
max_age <- max(Data.HQ_Cleaned$Age, na.rm = TRUE)
count_distribution <- table(cut(Data.HQ_Cleaned$Age, breaks = seq(min(Data.HQ_Cleaned$Age, na.rm = TRUE), max_age, by=5)))
max_count <- max(count_distribution)

# Creating a histogram of the "Age" variable with cleaned data
del2Draft = ggplot(Data.HQ_Cleaned, aes(x=Age)) +
  geom_histogram(binwidth=5, fill="orange", color="black") + # Adjust binwidth as needed
  labs(title="Age Distribution of Respondents",
       subtitle="Histogram representation of ages (U.S.A., 2024)",
       x="Age",
       y="Count",
       caption="Source: Music & Mental Health Survey Results. Annotations: Data visualized as of 04/2024.") +
  theme_minimal() +
  annotate("text", x=max_age * 0.8, y=max_count, label="Most respondents are in this age range", color="red", size=4)
del2Draft


# save del2Draft ----------------------------------------------------------
saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

#Bivariate Variable: num-num for correlation between "Hours spent on listening music per day" and "age"

Data.HQ_Cleaned <- Data.HQ %>%
  filter(!is.na(Age), !is.na(`Hours.per.day`), is.finite(Age), is.finite(`Hours.per.day`))

del3Draft = ggplot(Data.HQ_Cleaned, aes(x=Age, y=`Hours.per.day`)) +
  geom_point(alpha=0.5) +
  geom_smooth(method="lm", color="orange", se=FALSE) +  # Adds a linear regression line without confidence interval
  labs(title="Relationship Between Age and Daily Streaming Hours",
       subtitle="A Bivariate Analysis (U.S.A., 2024)",
       x="Age",
       y="Hours per Day",
       caption="Source: Music & Mental Health Survey Results. Annotations: Data visualized as of 04/2024.") +
  theme_minimal()
del3Draft 

# save del3Draft ----------------------------------------------------------
saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

library(sf)
library(maps)

# Read your data with explicit column types
data <- read_csv("DACSS 690V Map Data.csv", col_types = cols(
  `ID Year` = col_double(),
  `Year` = col_double(),
  `ID State` = col_character(),
  `State` = col_character(),
  `ID Workforce Status` = col_logical(),
  `Workforce Status` = col_logical(),
  `ID Detailed Occupation` = col_double(),
  `Detailed Occupation` = col_character(),
  `Average Wage` = col_double(),
  `Average Wage Appx MOE` = col_double(),
  `Slug State` = col_character()
))

# Get map data
states_map <- map_data("state")

# Prepare your data: unify case and potential state naming differences
data$State <- tolower(data$State)
states_map$region <- tolower(states_map$region)

# Merge your data with the map data
merged_data <- merge(states_map, data, by.x = "region", by.y = "State", all.x = TRUE)

# Plot the data
del4Draft = ggplot(merged_data, aes(x = long, y = lat, group = group, fill = `Average Wage`)) +
  geom_polygon(color = "orange") +
  coord_fixed(1.3) +
  labs(title = "Average Income of Musicians by State, 2024", 
       subtitle="A Map Data (U.S.A., 2024)",
       caption="Source: Music & Mental Health Survey Results. Annotations: Data visualized as of 04/2024.",
       fill = "Average Wage") +
  theme_minimal()
del4Draft

# save del4Draft ----------------------------------------------------------
saveRDS(del4Draft, file = "del4Draft.rds")

