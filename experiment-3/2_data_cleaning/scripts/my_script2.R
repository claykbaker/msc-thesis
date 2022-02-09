# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)

# read in raw data for raw data part 1 R1 to R4
raw_data_part_2 <- read_csv("data_input/raw_data_part_2_R5_to_R8.csv", 
                            col_names = c("timestamp_raw", #assign new column names
                                          "captcha_code",
                                          "batch_code",
                                          "consent",
                                          "inclusion_criteria",
                                          "age",
                                          "gender",
                                          "rank_R5_premise",
                                          "rank_R5_conclusion_1",
                                          "rank_R5_conclusion_2",
                                          "rank_R6_premise",
                                          "rank_R6_conclusion",
                                          "rank_R7_premise",
                                          "rank_R7_conclusion",
                                          "rank_R8_premise",
                                          "rank_R8_conclusion_1",
                                          "rank_R8_conclusion_2",
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
raw_data_part_2$rank_R5_premise <- factor(raw_data_part_2$rank_R5_premise, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R5_premise)[1] <- "1"
levels(raw_data_part_2$rank_R5_premise)[2] <- "2"
levels(raw_data_part_2$rank_R5_premise)[3] <- "3"
levels(raw_data_part_2$rank_R5_premise)[4] <- "4"
levels(raw_data_part_2$rank_R5_premise)[5] <- "5"
levels(raw_data_part_2$rank_R5_premise)[6] <- "6"
levels(raw_data_part_2$rank_R5_premise)[7] <- "7"
levels(raw_data_part_2$rank_R5_premise)[8] <- "8"
levels(raw_data_part_2$rank_R5_premise)[9] <- "9"
levels(raw_data_part_2$rank_R5_premise)[10] <- "10"
summary(raw_data_part_2$rank_R5_premise)

raw_data_part_2$rank_R5_conclusion_1 <- factor(raw_data_part_2$rank_R5_conclusion_1, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R5_conclusion_1)[1] <- "1"
levels(raw_data_part_2$rank_R5_conclusion_1)[2] <- "2"
levels(raw_data_part_2$rank_R5_conclusion_1)[3] <- "3"
levels(raw_data_part_2$rank_R5_conclusion_1)[4] <- "4"
levels(raw_data_part_2$rank_R5_conclusion_1)[5] <- "5"
levels(raw_data_part_2$rank_R5_conclusion_1)[6] <- "6"
levels(raw_data_part_2$rank_R5_conclusion_1)[7] <- "7"
levels(raw_data_part_2$rank_R5_conclusion_1)[8] <- "8"
levels(raw_data_part_2$rank_R5_conclusion_1)[9] <- "9"
levels(raw_data_part_2$rank_R5_conclusion_1)[10] <- "10"
summary(raw_data_part_2$rank_R5_conclusion_1)

raw_data_part_2$rank_R5_conclusion_2 <- factor(raw_data_part_2$rank_R5_conclusion_2, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R5_conclusion_2)[1] <- "1"
levels(raw_data_part_2$rank_R5_conclusion_2)[2] <- "2"
levels(raw_data_part_2$rank_R5_conclusion_2)[3] <- "3"
levels(raw_data_part_2$rank_R5_conclusion_2)[4] <- "4"
levels(raw_data_part_2$rank_R5_conclusion_2)[5] <- "5"
levels(raw_data_part_2$rank_R5_conclusion_2)[6] <- "6"
levels(raw_data_part_2$rank_R5_conclusion_2)[7] <- "7"
levels(raw_data_part_2$rank_R5_conclusion_2)[8] <- "8"
levels(raw_data_part_2$rank_R5_conclusion_2)[9] <- "9"
levels(raw_data_part_2$rank_R5_conclusion_2)[10] <- "10"
summary(raw_data_part_2$rank_R5_conclusion_2)

raw_data_part_2$rank_R6_premise <- factor(raw_data_part_2$rank_R6_premise, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R6_premise)[1] <- "1"
levels(raw_data_part_2$rank_R6_premise)[2] <- "2"
levels(raw_data_part_2$rank_R6_premise)[3] <- "3"
levels(raw_data_part_2$rank_R6_premise)[4] <- "4"
levels(raw_data_part_2$rank_R6_premise)[5] <- "5"
levels(raw_data_part_2$rank_R6_premise)[6] <- "6"
levels(raw_data_part_2$rank_R6_premise)[7] <- "7"
levels(raw_data_part_2$rank_R6_premise)[8] <- "8"
levels(raw_data_part_2$rank_R6_premise)[9] <- "9"
levels(raw_data_part_2$rank_R6_premise)[10] <- "10"
summary(raw_data_part_2$rank_R6_premise)

raw_data_part_2$rank_R6_conclusion <- factor(raw_data_part_2$rank_R6_conclusion, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R6_conclusion)[1] <- "1"
levels(raw_data_part_2$rank_R6_conclusion)[2] <- "2"
levels(raw_data_part_2$rank_R6_conclusion)[3] <- "3"
levels(raw_data_part_2$rank_R6_conclusion)[4] <- "4"
levels(raw_data_part_2$rank_R6_conclusion)[5] <- "5"
levels(raw_data_part_2$rank_R6_conclusion)[6] <- "6"
levels(raw_data_part_2$rank_R6_conclusion)[7] <- "7"
levels(raw_data_part_2$rank_R6_conclusion)[8] <- "8"
levels(raw_data_part_2$rank_R6_conclusion)[9] <- "9"
levels(raw_data_part_2$rank_R6_conclusion)[10] <- "10"
summary(raw_data_part_2$rank_R6_conclusion)

raw_data_part_2$rank_R7_premise <- factor(raw_data_part_2$rank_R7_premise, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R7_premise)[1] <- "1"
levels(raw_data_part_2$rank_R7_premise)[2] <- "2"
levels(raw_data_part_2$rank_R7_premise)[3] <- "3"
levels(raw_data_part_2$rank_R7_premise)[4] <- "4"
levels(raw_data_part_2$rank_R7_premise)[5] <- "5"
levels(raw_data_part_2$rank_R7_premise)[6] <- "6"
levels(raw_data_part_2$rank_R7_premise)[7] <- "7"
levels(raw_data_part_2$rank_R7_premise)[8] <- "8"
levels(raw_data_part_2$rank_R7_premise)[9] <- "9"
levels(raw_data_part_2$rank_R7_premise)[10] <- "10"
summary(raw_data_part_2$rank_R7_premise)

raw_data_part_2$rank_R7_conclusion <- factor(raw_data_part_2$rank_R7_conclusion, 
                                             levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R7_conclusion)[1] <- "1"
levels(raw_data_part_2$rank_R7_conclusion)[2] <- "2"
levels(raw_data_part_2$rank_R7_conclusion)[3] <- "3"
levels(raw_data_part_2$rank_R7_conclusion)[4] <- "4"
levels(raw_data_part_2$rank_R7_conclusion)[5] <- "5"
levels(raw_data_part_2$rank_R7_conclusion)[6] <- "6"
levels(raw_data_part_2$rank_R7_conclusion)[7] <- "7"
levels(raw_data_part_2$rank_R7_conclusion)[8] <- "8"
levels(raw_data_part_2$rank_R7_conclusion)[9] <- "9"
levels(raw_data_part_2$rank_R7_conclusion)[10] <- "10"
summary(raw_data_part_2$rank_R7_conclusion)

raw_data_part_2$rank_R8_premise <- factor(raw_data_part_2$rank_R8_premise, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R8_premise)[1] <- "1"
levels(raw_data_part_2$rank_R8_premise)[2] <- "2"
levels(raw_data_part_2$rank_R8_premise)[3] <- "3"
levels(raw_data_part_2$rank_R8_premise)[4] <- "4"
levels(raw_data_part_2$rank_R8_premise)[5] <- "5"
levels(raw_data_part_2$rank_R8_premise)[6] <- "6"
levels(raw_data_part_2$rank_R8_premise)[7] <- "7"
levels(raw_data_part_2$rank_R8_premise)[8] <- "8"
levels(raw_data_part_2$rank_R8_premise)[9] <- "9"
levels(raw_data_part_2$rank_R8_premise)[10] <- "10"
summary(raw_data_part_2$rank_R8_premise)

raw_data_part_2$rank_R8_conclusion_1 <- factor(raw_data_part_2$rank_R8_conclusion_1, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R8_conclusion_1)[1] <- "1"
levels(raw_data_part_2$rank_R8_conclusion_1)[2] <- "2"
levels(raw_data_part_2$rank_R8_conclusion_1)[3] <- "3"
levels(raw_data_part_2$rank_R8_conclusion_1)[4] <- "4"
levels(raw_data_part_2$rank_R8_conclusion_1)[5] <- "5"
levels(raw_data_part_2$rank_R8_conclusion_1)[6] <- "6"
levels(raw_data_part_2$rank_R8_conclusion_1)[7] <- "7"
levels(raw_data_part_2$rank_R8_conclusion_1)[8] <- "8"
levels(raw_data_part_2$rank_R8_conclusion_1)[9] <- "9"
levels(raw_data_part_2$rank_R8_conclusion_1)[10] <- "10"
summary(raw_data_part_2$rank_R8_conclusion_1)

raw_data_part_2$rank_R8_conclusion_2 <- factor(raw_data_part_2$rank_R8_conclusion_2, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_2$rank_R8_conclusion_2)[1] <- "1"
levels(raw_data_part_2$rank_R8_conclusion_2)[2] <- "2"
levels(raw_data_part_2$rank_R8_conclusion_2)[3] <- "3"
levels(raw_data_part_2$rank_R8_conclusion_2)[4] <- "4"
levels(raw_data_part_2$rank_R8_conclusion_2)[5] <- "5"
levels(raw_data_part_2$rank_R8_conclusion_2)[6] <- "6"
levels(raw_data_part_2$rank_R8_conclusion_2)[7] <- "7"
levels(raw_data_part_2$rank_R8_conclusion_2)[8] <- "8"
levels(raw_data_part_2$rank_R8_conclusion_2)[9] <- "9"
levels(raw_data_part_2$rank_R8_conclusion_2)[10] <- "10"
summary(raw_data_part_2$rank_R8_conclusion_2)

# remove mturk_id column which contains personal identifiable information
raw_data_part_2 <- raw_data_part_2%>% 
  select(!mturk_ID)

# for each rank, count the rank and its percentage frequency
data_R5_premise <- raw_data_part_2 %>% 
  count(rank_R5_premise, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R5_premise) %>% 
  select(rank_R5_premise, percentage)

data_R5_conclusion_1 <- raw_data_part_2 %>% 
  count(rank_R5_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R5_conclusion_1) %>% 
  select(rank_R5_conclusion_1, percentage)

data_R5_conclusion_2 <- raw_data_part_2 %>% 
  count(rank_R5_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R5_conclusion_2) %>% 
  select(rank_R5_conclusion_2, percentage)

data_R6_premise <- raw_data_part_2 %>% 
  count(rank_R6_premise, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R6_premise) %>% 
  select(rank_R6_premise, percentage)

data_R6_conclusion <- raw_data_part_2 %>% 
  count(rank_R6_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R6_conclusion) %>% 
  select(rank_R6_conclusion, percentage)

data_R7_premise <- raw_data_part_2 %>% 
  count(rank_R7_premise, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R7_premise) %>% 
  select(rank_R7_premise, percentage)

data_R7_conclusion <- raw_data_part_2 %>% 
  count(rank_R7_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R7_conclusion) %>% 
  select(rank_R7_conclusion, percentage)


data_R8_premise <- raw_data_part_2 %>% 
  count(rank_R8_premise, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R8_premise) %>% 
  select(rank_R8_premise, percentage)

data_R8_conclusion_1 <- raw_data_part_2 %>% 
  count(rank_R8_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R8_conclusion_1) %>% 
  select(rank_R8_conclusion_1, percentage)

data_R8_conclusion_2 <- raw_data_part_2 %>% 
  count(rank_R8_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R8_conclusion_2) %>% 
  select(rank_R8_conclusion_2, percentage)

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

# visualisation of rank and percentage frequency for postulates R5 to R8
# create a bar plot of the % frequency of each rank per rule (premise / conclusion) in each postulate
# combine all the plots into a single plot
# save the combined plot

rank_R5_premise_bar <- ggplot(data = data_R5_premise,
                                   mapping = aes(x = rank_R5_premise,
                                                 y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R5 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R5_premise_bar

rank_R5_conclusion_1_bar <- ggplot(data = data_R5_conclusion_1,
                              mapping = aes(x = rank_R5_conclusion_1,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R5 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R5_conclusion_1_bar

rank_R5_conclusion_2_bar <- ggplot(data = data_R5_conclusion_2,
                                   mapping = aes(x = rank_R5_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R5 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R5_conclusion_2_bar


rank_R6_premise_bar <- ggplot(data = data_R6_premise,
                              mapping = aes(x = rank_R6_premise,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R6 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R6_premise_bar


rank_R6_conclusion_bar <- ggplot(data = data_R6_conclusion,
                              mapping = aes(x = rank_R6_conclusion,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R6 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R6_conclusion_bar


rank_R7_premise_bar <- ggplot(data = data_R7_premise,
                              mapping = aes(x = rank_R7_premise,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R7 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R7_premise_bar


rank_R7_conclusion_bar <- ggplot(data = data_R7_conclusion,
                                 mapping = aes(x = rank_R7_conclusion,
                                               y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R7 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R7_conclusion_bar

rank_R8_premise_bar <- ggplot(data = data_R8_premise,
                              mapping = aes(x = rank_R8_premise,
                                            y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R8 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R8_premise_bar

rank_R8_conclusion_1_bar <- ggplot(data = data_R8_conclusion_1,
                                   mapping = aes(x = rank_R8_conclusion_1,
                                                 y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R8 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R8_conclusion_1_bar

rank_R8_conclusion_2_bar <- ggplot(data = data_R8_conclusion_2,
                                   mapping = aes(x = rank_R8_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R8 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R8_conclusion_2_bar


rank_R5_to_R8_plot <- rank_R5_premise_bar +
  rank_R5_conclusion_1_bar +
  rank_R5_conclusion_2_bar +
  rank_R6_premise_bar +
  rank_R6_conclusion_bar +
  rank_R7_premise_bar +
  rank_R7_conclusion_bar +
  rank_R8_premise_bar +
  rank_R8_conclusion_1_bar +
  rank_R8_conclusion_2_bar 

rank_R5_to_R8_plot

plot_layout()

ggsave(plot = rank_R5_to_R8_plot,
       filename = "figure_output/my_plot_R5_to_R8.jpg")

# view summary of cleaned data 
str(raw_data_part_2)
summary(raw_data_part_2)

# write cleaned data to file
write_csv(x = raw_data_part_2,
          file = "data_output/raw-data-part-2-cleaned.csv")
