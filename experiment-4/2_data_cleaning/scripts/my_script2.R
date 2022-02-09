# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(cowplot)

# read in raw data for raw data part 1 U1 to R5
raw_data_part_2 <- read_csv("data_input/raw_U6_to_U9.csv", 
                            col_names = c("timestamp_raw", #assign new column names
                                          "access_code",
                                          "captcha_code",
                                          "consent",
                                          "inclusion_criteria",
                                          "age",
                                          "gender",
                                          "rank_U6_premise",
                                          "rank_U6_conclusion_1",
                                          "rank_U6_conclusion_2",
                                          "rank_U7_premise",
                                          "rank_U7_conclusion",
                                          "rank_U8_conclusion_1",
                                          "rank_U8_conclusion_2",
                                          "rank_U9_premise",
                                          "rank_U9_conclusion",
                                          "mturk_ID_consent",
                                          "mturk_ID",
                                          "feedback"),
                            
                            skip = 1, # ignore first row which contains old column values                        
                            na ="",  # assign NA (null) to empty cells
                            trim_ws = TRUE) # trim whitespace

# inspecting the raw data
str(raw_data_part_2)
summary(raw_data_part_2)

# arrange survey data by ascending order of MTurk IDs
raw_data_part_2 <- raw_data_part_2 %>% 
  arrange(mturk_ID)

# remove rows which contain duplicate MTurk IDs (only 50 unique participants per survey)
raw_data_part_2 <- raw_data_part_2[!duplicated(raw_data_part_2$mturk_ID), ]

# remove rows where consent is not given
raw_data_part_2 <- raw_data_part_2 %>% 
  filter(!consent == "No")

# separate inclusion_criteria values into its 3 components
#  - inclusion_criteria_age
#  - inclusion_criteria_location
#  - inclusion_criteria_fluent_English

raw_data_part_2 <- raw_data_part_2 %>% 
  mutate(inclusion_criteria_age = substr(inclusion_criteria, start = 0, stop = 16)) %>% 
  mutate(inclusion_criteria_location = substr(inclusion_criteria, start = 18, stop = 78)) %>% 
  mutate(inclusion_criteria_language = substr(inclusion_criteria, start = 80, stop = 101))

# remove the column containing the unseparated inclusion criteria value
raw_data_part_2 <- raw_data_part_2 %>% 
  select(!inclusion_criteria)

# create columns for date components of timestamp into separate columns: y, m, d, h,  m, s
raw_data_part_2 <- raw_data_part_2 %>% 
  mutate(timestamp_formatted = timestamp(raw_data_part_2$timestamp_raw)) %>% 
  mutate(date = ymd_hms(timestamp_formatted, tz = "EET")) %>% 
  mutate(year = substring(date, first = 1, last = 4)) %>% 
  mutate(month = substring(date, first = 6, last = 7)) %>% 
  mutate(day = substring(date, first = 9, last = 10)) %>% 
  mutate(hour = substring(date, first = 12, last = 13)) %>% 
  mutate(minute = substring(date, first = 15, last = 16)) %>% 
  mutate(second = substring(date, first = 18, last = 19)) 

# convert date components from characters to numbers
raw_data_part_2$year <- as.numeric(raw_data_part_2$year)
raw_data_part_2$month <- as.numeric(raw_data_part_2$month)
raw_data_part_2$day <- as.numeric(raw_data_part_2$day)
raw_data_part_2$hour <- as.numeric(raw_data_part_2$hour)
raw_data_part_2$minute <- as.numeric(raw_data_part_2$minute)
raw_data_part_2$second <- as.numeric(raw_data_part_2$second)

# convert date from character to date
raw_data_part_2$date <- as.Date(raw_data_part_2$date)

# remove timestamp and date fields as components are stored separately
raw_data_part_2 <- raw_data_part_2 %>% 
  select(-c(timestamp_raw, timestamp_formatted, date))

# factor ranks according to scale used in survey
raw_data_part_2$rank_U6_premise <- factor(raw_data_part_2$rank_U6_premise, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U6_premise)[1] <- "1"
levels(raw_data_part_2$rank_U6_premise)[2] <- "2"
levels(raw_data_part_2$rank_U6_premise)[3] <- "3"
levels(raw_data_part_2$rank_U6_premise)[4] <- "4"
levels(raw_data_part_2$rank_U6_premise)[5] <- "5"
levels(raw_data_part_2$rank_U6_premise)[6] <- "6"
levels(raw_data_part_2$rank_U6_premise)[7] <- "7"
levels(raw_data_part_2$rank_U6_premise)[8] <- "8"
levels(raw_data_part_2$rank_U6_premise)[9] <- "9"
levels(raw_data_part_2$rank_U6_premise)[10] <- "10"
summary(raw_data_part_2$rank_U6_premise)

raw_data_part_2$rank_U6_conclusion_1 <- factor(raw_data_part_2$rank_U6_conclusion_1, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U6_conclusion_1)[1] <- "1"
levels(raw_data_part_2$rank_U6_conclusion_1)[2] <- "2"
levels(raw_data_part_2$rank_U6_conclusion_1)[3] <- "3"
levels(raw_data_part_2$rank_U6_conclusion_1)[4] <- "4"
levels(raw_data_part_2$rank_U6_conclusion_1)[5] <- "5"
levels(raw_data_part_2$rank_U6_conclusion_1)[6] <- "6"
levels(raw_data_part_2$rank_U6_conclusion_1)[7] <- "7"
levels(raw_data_part_2$rank_U6_conclusion_1)[8] <- "8"
levels(raw_data_part_2$rank_U6_conclusion_1)[9] <- "9"
levels(raw_data_part_2$rank_U6_conclusion_1)[10] <- "10"
summary(raw_data_part_2$rank_U6_conclusion_1)

raw_data_part_2$rank_U6_conclusion_2 <- factor(raw_data_part_2$rank_U6_conclusion_2, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U6_conclusion_2)[1] <- "1"
levels(raw_data_part_2$rank_U6_conclusion_2)[2] <- "2"
levels(raw_data_part_2$rank_U6_conclusion_2)[3] <- "3"
levels(raw_data_part_2$rank_U6_conclusion_2)[4] <- "4"
levels(raw_data_part_2$rank_U6_conclusion_2)[5] <- "5"
levels(raw_data_part_2$rank_U6_conclusion_2)[6] <- "6"
levels(raw_data_part_2$rank_U6_conclusion_2)[7] <- "7"
levels(raw_data_part_2$rank_U6_conclusion_2)[8] <- "8"
levels(raw_data_part_2$rank_U6_conclusion_2)[9] <- "9"
levels(raw_data_part_2$rank_U6_conclusion_2)[10] <- "10"
summary(raw_data_part_2$rank_U6_conclusion_2)

raw_data_part_2$rank_U7_premise <- factor(raw_data_part_2$rank_U7_premise, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U7_premise)[1] <- "1"
levels(raw_data_part_2$rank_U7_premise)[2] <- "2"
levels(raw_data_part_2$rank_U7_premise)[3] <- "3"
levels(raw_data_part_2$rank_U7_premise)[4] <- "4"
levels(raw_data_part_2$rank_U7_premise)[5] <- "5"
levels(raw_data_part_2$rank_U7_premise)[6] <- "6"
levels(raw_data_part_2$rank_U7_premise)[7] <- "7"
levels(raw_data_part_2$rank_U7_premise)[8] <- "8"
levels(raw_data_part_2$rank_U7_premise)[9] <- "9"
levels(raw_data_part_2$rank_U7_premise)[10] <- "10"
summary(raw_data_part_2$rank_U7_premise)

raw_data_part_2$rank_U7_conclusion <- factor(raw_data_part_2$rank_U7_conclusion, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U7_conclusion)[1] <- "1"
levels(raw_data_part_2$rank_U7_conclusion)[2] <- "2"
levels(raw_data_part_2$rank_U7_conclusion)[3] <- "3"
levels(raw_data_part_2$rank_U7_conclusion)[4] <- "4"
levels(raw_data_part_2$rank_U7_conclusion)[5] <- "5"
levels(raw_data_part_2$rank_U7_conclusion)[6] <- "6"
levels(raw_data_part_2$rank_U7_conclusion)[7] <- "7"
levels(raw_data_part_2$rank_U7_conclusion)[8] <- "8"
levels(raw_data_part_2$rank_U7_conclusion)[9] <- "9"
levels(raw_data_part_2$rank_U7_conclusion)[10] <- "10"
summary(raw_data_part_2$rank_U7_conclusion)

raw_data_part_2$rank_U8_conclusion_1 <- factor(raw_data_part_2$rank_U8_conclusion_1, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U8_conclusion_1)[1] <- "1"
levels(raw_data_part_2$rank_U8_conclusion_1)[2] <- "2"
levels(raw_data_part_2$rank_U8_conclusion_1)[3] <- "3"
levels(raw_data_part_2$rank_U8_conclusion_1)[4] <- "4"
levels(raw_data_part_2$rank_U8_conclusion_1)[5] <- "5"
levels(raw_data_part_2$rank_U8_conclusion_1)[6] <- "6"
levels(raw_data_part_2$rank_U8_conclusion_1)[7] <- "7"
levels(raw_data_part_2$rank_U8_conclusion_1)[8] <- "8"
levels(raw_data_part_2$rank_U8_conclusion_1)[9] <- "9"
levels(raw_data_part_2$rank_U8_conclusion_1)[10] <- "10"
summary(raw_data_part_2$rank_U8_conclusion_1)

raw_data_part_2$rank_U8_conclusion_2 <- factor(raw_data_part_2$rank_U8_conclusion_2, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U8_conclusion_2)[1] <- "1"
levels(raw_data_part_2$rank_U8_conclusion_2)[2] <- "2"
levels(raw_data_part_2$rank_U8_conclusion_2)[3] <- "3"
levels(raw_data_part_2$rank_U8_conclusion_2)[4] <- "4"
levels(raw_data_part_2$rank_U8_conclusion_2)[5] <- "5"
levels(raw_data_part_2$rank_U8_conclusion_2)[6] <- "6"
levels(raw_data_part_2$rank_U8_conclusion_2)[7] <- "7"
levels(raw_data_part_2$rank_U8_conclusion_2)[8] <- "8"
levels(raw_data_part_2$rank_U8_conclusion_2)[9] <- "9"
levels(raw_data_part_2$rank_U8_conclusion_2)[10] <- "10"
summary(raw_data_part_2$rank_U8_conclusion_2)

raw_data_part_2$rank_U9_premise <- factor(raw_data_part_2$rank_U9_premise, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U9_premise)[1] <- "1"
levels(raw_data_part_2$rank_U9_premise)[2] <- "2"
levels(raw_data_part_2$rank_U9_premise)[3] <- "3"
levels(raw_data_part_2$rank_U9_premise)[4] <- "4"
levels(raw_data_part_2$rank_U9_premise)[5] <- "5"
levels(raw_data_part_2$rank_U9_premise)[6] <- "6"
levels(raw_data_part_2$rank_U9_premise)[7] <- "7"
levels(raw_data_part_2$rank_U9_premise)[8] <- "8"
levels(raw_data_part_2$rank_U9_premise)[9] <- "9"
levels(raw_data_part_2$rank_U9_premise)[10] <- "10"
summary(raw_data_part_2$rank_U9_premise)

raw_data_part_2$rank_U9_conclusion <- factor(raw_data_part_2$rank_U9_conclusion, 
                                             levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_U9_conclusion)[1] <- "1"
levels(raw_data_part_2$rank_U9_conclusion)[2] <- "2"
levels(raw_data_part_2$rank_U9_conclusion)[3] <- "3"
levels(raw_data_part_2$rank_U9_conclusion)[4] <- "4"
levels(raw_data_part_2$rank_U9_conclusion)[5] <- "5"
levels(raw_data_part_2$rank_U9_conclusion)[6] <- "6"
levels(raw_data_part_2$rank_U9_conclusion)[7] <- "7"
levels(raw_data_part_2$rank_U9_conclusion)[8] <- "8"
levels(raw_data_part_2$rank_U9_conclusion)[9] <- "9"
levels(raw_data_part_2$rank_U9_conclusion)[10] <- "10"
summary(raw_data_part_2$rank_U9_conclusion)

# remove mturk_id column which contains personal identifiable information
raw_data_part_2 <- raw_data_part_2%>% 
  select(!mturk_ID)

# for each rank, count the rank and its percentage frequency (N = 37)
data_U6_premise <- raw_data_part_2 %>% 
  count(rank_U6_premise, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U6_premise) %>% 
  select(rank_U6_premise, percentage)

data_U6_conclusion_1 <- raw_data_part_2 %>% 
  count(rank_U6_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U6_conclusion_1) %>% 
  select(rank_U6_conclusion_1, percentage)

data_U6_conclusion_2 <- raw_data_part_2 %>% 
  count(rank_U6_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U6_conclusion_2) %>% 
  select(rank_U6_conclusion_2, percentage)

data_U7_premise <- raw_data_part_2 %>% 
  count(rank_U7_premise, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U7_premise) %>% 
  select(rank_U7_premise, percentage)

data_U7_conclusion <- raw_data_part_2 %>% 
  count(rank_U7_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U7_conclusion) %>% 
  select(rank_U7_conclusion, percentage)

data_U8_conclusion_1 <- raw_data_part_2 %>% 
  count(rank_U8_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U8_conclusion_1) %>% 
  select(rank_U8_conclusion_1, percentage)

data_U8_conclusion_2 <- raw_data_part_2 %>% 
  count(rank_U8_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U8_conclusion_2) %>% 
  select(rank_U8_conclusion_2, percentage)

data_U9_premise <- raw_data_part_2 %>% 
  count(rank_U9_premise, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U9_premise) %>% 
  select(rank_U9_premise, percentage)

data_U9_conclusion <- raw_data_part_2 %>% 
  count(rank_U9_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U9_conclusion) %>% 
  select(rank_U9_conclusion, percentage)

# create a tibble with only demographic data
demographic_data <- raw_data_part_2 %>% 
  select(age, gender) %>% 
  arrange(age)

# create a tibble with age and its frequency 
age_data <- raw_data_part_2 %>% 
  count(age, sort = TRUE) %>% 
  arrange(age)

# create a tibble with gender and its frequency 
gender_data <- raw_data_part_2 %>% 
  count(gender, sort = TRUE) %>% 
  arrange(gender)

# create a theme which can be used to style bar plots
grey_theme <- theme(text = element_text(size = 16),
                    plot.title = element_text(size = 14),
                    axis.text.x = element_text(color = "grey20",
                                               size = 10,
                                               hjust = 0.5,
                                               vjust = 0.5),
                    axis.text.y = element_text(color = "grey20",
                                               size = 10, 
                                               hjust = 0.5,
                                               vjust = 0.5),
                    axis.title.x = element_text(size = 12),
                    axis.title.y = element_text(size=12))

# visualisation of demographic data
demographic_plot <- ggplot(data = demographic_data,
                           mapping = aes(x = age, 
                                         y = gender)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, color = "deepskyblue3") + 
  labs(title = "Age vs Gender",
       x = "Age",
       y = "Gender") +
  grey_theme


demographic_plot

# visualisation of rank and percentage frequency for postulates U1 to U5
# create a bar plot of the % frequency of each rank per rule (premise / conclusion) in each postulate
# combine all the plots into a single plot
# save the combined plot

rank_U6_premise_bar <- ggplot(data = data_U6_premise,
                                 mapping = aes(x = rank_U6_premise,
                                               y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U6 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U6_premise_bar

rank_U6_conclusion_1_bar <- ggplot(data = data_U6_conclusion_1,
                              mapping = aes(x = rank_U6_conclusion_1,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U6 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U6_conclusion_1_bar

rank_U6_conclusion_2_bar <- ggplot(data = data_U6_conclusion_2,
                                   mapping = aes(x = rank_U6_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U6 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U6_conclusion_2_bar

rank_U7_premise_bar <- ggplot(data = data_U7_premise,
                              mapping = aes(x = rank_U7_premise,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U7 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U7_premise_bar

rank_U7_conclusion_bar <- ggplot(data = data_U7_conclusion,
                              mapping = aes(x = rank_U7_conclusion,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U7 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U7_conclusion_bar

rank_U8_conclusion_1_bar <- ggplot(data = data_U8_conclusion_1,
                                   mapping = aes(x = rank_U8_conclusion_1,
                                                 y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U8 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U8_conclusion_1_bar

rank_U8_conclusion_2_bar <- ggplot(data = data_U8_conclusion_2,
                                   mapping = aes(x = rank_U8_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U8 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U8_conclusion_2_bar

rank_U9_premise_bar <- ggplot(data = data_U9_premise,
                              mapping = aes(x = rank_U9_premise,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U9 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U9_premise_bar

rank_U9_conclusion_bar <- ggplot(data = data_U9_conclusion,
                                 mapping = aes(x = rank_U9_conclusion,
                                               y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U9 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U9_conclusion_bar

# combine plots of all rules formatted as a 5 x 3 matrix of plots on a single page
rank_U6_to_U9_plot <- plot_grid(rank_U6_premise_bar,
                                rank_U6_conclusion_1_bar,
                                rank_U6_conclusion_2_bar,
                                rank_U7_premise_bar,
                                rank_U7_conclusion_bar,
                                rank_U8_conclusion_1_bar,
                                rank_U8_conclusion_2_bar,
                                rank_U9_premise_bar,
                                rank_U9_conclusion_bar,
                                ncol = 3,
                                nrow = 3)

rank_U6_to_U9_plot

ggsave(plot = rank_U6_to_U9_plot,
       filename = "figure_output/my_plot_U6_to_U9.jpg")

# view summary of cleaned data 
str(raw_data_part_2)
summary(raw_data_part_2)

# write cleaned data to file
write_csv(x = raw_data_part_2,
          file = "data_output/cleaned_data_U6_to_U9.csv")
