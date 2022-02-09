# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)
library(cowplot)

# read in raw data for raw data part 1 U1 to R5
raw_data_part_1 <- read_csv("data_input/raw_U1_to_U5.csv", 
                          col_names = c("timestamp_raw", #assign new column names
                                        "captcha_code",
                                        "consent",
                                        "inclusion_criteria",
                                        "age",
                                        "gender",
                                        "practice_question",
                                        "rank_U1_conclusion",
                                        "rank_U2_premise",
                                        "rank_U2_conclusion_1",
                                        "rank_U2_conclusion_2",
                                        "rank_U3_premise",
                                        "rank_U3_conclusion",
                                        "rank_U4_premise_1",
                                        "rank_U4_premise_2",
                                        "rank_U4_premise_3",
                                        "rank_U4_premise_4",
                                        "rank_U4_conclusion_1",
                                        "rank_U4_conclusion_2",
                                        "rank_U5_conclusion",
                                        "mturk_ID_consent",
                                        "mturk_ID",
                                        "feedback"),

                          skip = 1, # ignore first row which contains old column values                        
                          na ="",  # assign NA (null) to empty cells
                          trim_ws = TRUE) # trim whitespace

# inspecting the raw data
str(raw_data_part_1)
summary(raw_data_part_1)
  
# arrange survey data by ascending order of MTurk IDs
raw_data_part_1 <- raw_data_part_1 %>% 
  arrange(mturk_ID)

# remove rows which contain duplicate MTurk IDs (only 50 unique participants per survey)
raw_data_part_1 <- raw_data_part_1[!duplicated(raw_data_part_1$mturk_ID), ]

# remove rows where consent is not given
raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!consent == "No")

# separate inclusion_criteria values into its 3 components
#  - inclusion_criteria_age
#  - inclusion_criteria_location
#  - inclusion_criteria_fluent_English

raw_data_part_1 <- raw_data_part_1 %>% 
  mutate(inclusion_criteria_age = substr(inclusion_criteria, start = 0, stop = 16)) %>% 
  mutate(inclusion_criteria_location = substr(inclusion_criteria, start = 18, stop = 78)) %>% 
  mutate(inclusion_criteria_language = substr(inclusion_criteria, start = 80, stop = 101))

# remove the column containing the unseparated inclusion criteria value
raw_data_part_1 <- raw_data_part_1 %>% 
  select(!inclusion_criteria)

# create columns for date components of timestamp into separate columns: y, m, d, h,  m, s
raw_data_part_1 <- raw_data_part_1 %>% 
  mutate(timestamp_formatted = timestamp(raw_data_part_1$timestamp_raw)) %>% 
  mutate(date = ymd_hms(timestamp_formatted, tz = "EET")) %>% 
  mutate(year = substring(date, first = 1, last = 4)) %>% 
  mutate(month = substring(date, first = 6, last = 7)) %>% 
  mutate(day = substring(date, first = 9, last = 10)) %>% 
  mutate(hour = substring(date, first = 12, last = 13)) %>% 
  mutate(minute = substring(date, first = 15, last = 16)) %>% 
  mutate(second = substring(date, first = 18, last = 19)) 

# convert date components from characters to numbers
raw_data_part_1$year <- as.numeric(raw_data_part_1$year)
raw_data_part_1$month <- as.numeric(raw_data_part_1$month)
raw_data_part_1$day <- as.numeric(raw_data_part_1$day)
raw_data_part_1$hour <- as.numeric(raw_data_part_1$hour)
raw_data_part_1$minute <- as.numeric(raw_data_part_1$minute)
raw_data_part_1$second <- as.numeric(raw_data_part_1$second)

# convert date from character to date
raw_data_part_1$date <- as.Date(raw_data_part_1$date)

# remove timestamp and date fields as components are stored separately
raw_data_part_1 <- raw_data_part_1 %>% 
  select(-c(timestamp_raw, timestamp_formatted, date))

# factor ranks according to scale used in survey
raw_data_part_1$rank_R1_conclusion_1 <- factor(raw_data_part_1$rank_R1_conclusion_1, 
                                         levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R1_conclusion_1)[1] <- "1"
levels(raw_data_part_1$rank_R1_conclusion_1)[2] <- "2"
levels(raw_data_part_1$rank_R1_conclusion_1)[3] <- "3"
levels(raw_data_part_1$rank_R1_conclusion_1)[4] <- "4"
levels(raw_data_part_1$rank_R1_conclusion_1)[5] <- "5"
levels(raw_data_part_1$rank_R1_conclusion_1)[6] <- "6"
levels(raw_data_part_1$rank_R1_conclusion_1)[7] <- "7"
levels(raw_data_part_1$rank_R1_conclusion_1)[8] <- "8"
levels(raw_data_part_1$rank_R1_conclusion_1)[9] <- "9"
levels(raw_data_part_1$rank_R1_conclusion_1)[10] <- "10"
summary(raw_data_part_1$rank_R1_conclusion_1)

raw_data_part_1$rank_U1_conclusion <- factor(raw_data_part_1$rank_U1_conclusion, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U1_conclusion)[1] <- "1"
levels(raw_data_part_1$rank_U1_conclusion)[2] <- "2"
levels(raw_data_part_1$rank_U1_conclusion)[3] <- "3"
levels(raw_data_part_1$rank_U1_conclusion)[4] <- "4"
levels(raw_data_part_1$rank_U1_conclusion)[5] <- "5"
levels(raw_data_part_1$rank_U1_conclusion)[6] <- "6"
levels(raw_data_part_1$rank_U1_conclusion)[7] <- "7"
levels(raw_data_part_1$rank_U1_conclusion)[8] <- "8"
levels(raw_data_part_1$rank_U1_conclusion)[9] <- "9"
levels(raw_data_part_1$rank_U1_conclusion)[10] <- "10"
summary(raw_data_part_1$rank_U1_conclusion)

raw_data_part_1$rank_U2_premise <- factor(raw_data_part_1$rank_U2_premise, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U2_premise)[1] <- "1"
levels(raw_data_part_1$rank_U2_premise)[2] <- "2"
levels(raw_data_part_1$rank_U2_premise)[3] <- "3"
levels(raw_data_part_1$rank_U2_premise)[4] <- "4"
levels(raw_data_part_1$rank_U2_premise)[5] <- "5"
levels(raw_data_part_1$rank_U2_premise)[6] <- "6"
levels(raw_data_part_1$rank_U2_premise)[7] <- "7"
levels(raw_data_part_1$rank_U2_premise)[8] <- "8"
levels(raw_data_part_1$rank_U2_premise)[9] <- "9"
levels(raw_data_part_1$rank_U2_premise)[10] <- "10"
summary(raw_data_part_1$rank_U2_premise)

raw_data_part_1$rank_U2_conclusion_1 <- factor(raw_data_part_1$rank_U2_conclusion_1, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U2_conclusion_1)[1] <- "1"
levels(raw_data_part_1$rank_U2_conclusion_1)[2] <- "2"
levels(raw_data_part_1$rank_U2_conclusion_1)[3] <- "3"
levels(raw_data_part_1$rank_U2_conclusion_1)[4] <- "4"
levels(raw_data_part_1$rank_U2_conclusion_1)[5] <- "5"
levels(raw_data_part_1$rank_U2_conclusion_1)[6] <- "6"
levels(raw_data_part_1$rank_U2_conclusion_1)[7] <- "7"
levels(raw_data_part_1$rank_U2_conclusion_1)[8] <- "8"
levels(raw_data_part_1$rank_U2_conclusion_1)[9] <- "9"
levels(raw_data_part_1$rank_U2_conclusion_1)[10] <- "10"
summary(raw_data_part_1$rank_U2_conclusion_1)


raw_data_part_1$rank_U2_conclusion_2 <- factor(raw_data_part_1$rank_U2_conclusion_2, 
                                             levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U2_conclusion_2)[1] <- "1"
levels(raw_data_part_1$rank_U2_conclusion_2)[2] <- "2"
levels(raw_data_part_1$rank_U2_conclusion_2)[3] <- "3"
levels(raw_data_part_1$rank_U2_conclusion_2)[4] <- "4"
levels(raw_data_part_1$rank_U2_conclusion_2)[5] <- "5"
levels(raw_data_part_1$rank_U2_conclusion_2)[6] <- "6"
levels(raw_data_part_1$rank_U2_conclusion_2)[7] <- "7"
levels(raw_data_part_1$rank_U2_conclusion_2)[8] <- "8"
levels(raw_data_part_1$rank_U2_conclusion_2)[9] <- "9"
levels(raw_data_part_1$rank_U2_conclusion_2)[10] <- "10"
summary(raw_data_part_1$rank_U2_conclusion_2)

raw_data_part_1$rank_U3_premise <- factor(raw_data_part_1$rank_U3_premise, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U3_premise)[1] <- "1"
levels(raw_data_part_1$rank_U3_premise)[2] <- "2"
levels(raw_data_part_1$rank_U3_premise)[3] <- "3"
levels(raw_data_part_1$rank_U3_premise)[4] <- "4"
levels(raw_data_part_1$rank_U3_premise)[5] <- "5"
levels(raw_data_part_1$rank_U3_premise)[6] <- "6"
levels(raw_data_part_1$rank_U3_premise)[7] <- "7"
levels(raw_data_part_1$rank_U3_premise)[8] <- "8"
levels(raw_data_part_1$rank_U3_premise)[9] <- "9"
levels(raw_data_part_1$rank_U3_premise)[10] <- "10"
summary(raw_data_part_1$rank_U3_premise)

raw_data_part_1$rank_U3_conclusion <- factor(raw_data_part_1$rank_U3_conclusion, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U3_conclusion)[1] <- "1"
levels(raw_data_part_1$rank_U3_conclusion)[2] <- "2"
levels(raw_data_part_1$rank_U3_conclusion)[3] <- "3"
levels(raw_data_part_1$rank_U3_conclusion)[4] <- "4"
levels(raw_data_part_1$rank_U3_conclusion)[5] <- "5"
levels(raw_data_part_1$rank_U3_conclusion)[6] <- "6"
levels(raw_data_part_1$rank_U3_conclusion)[7] <- "7"
levels(raw_data_part_1$rank_U3_conclusion)[8] <- "8"
levels(raw_data_part_1$rank_U3_conclusion)[9] <- "9"
levels(raw_data_part_1$rank_U3_conclusion)[10] <- "10"
summary(raw_data_part_1$rank_U3_conclusion)

raw_data_part_1$rank_U4_premise_1 <- factor(raw_data_part_1$rank_U4_premise_1, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U4_premise_1)[1] <- "1"
levels(raw_data_part_1$rank_U4_premise_1)[2] <- "2"
levels(raw_data_part_1$rank_U4_premise_1)[3] <- "3"
levels(raw_data_part_1$rank_U4_premise_1)[4] <- "4"
levels(raw_data_part_1$rank_U4_premise_1)[5] <- "5"
levels(raw_data_part_1$rank_U4_premise_1)[6] <- "6"
levels(raw_data_part_1$rank_U4_premise_1)[7] <- "7"
levels(raw_data_part_1$rank_U4_premise_1)[8] <- "8"
levels(raw_data_part_1$rank_U4_premise_1)[9] <- "9"
levels(raw_data_part_1$rank_U4_premise_1)[10] <- "10"
summary(raw_data_part_1$rank_U4_premise_1)


raw_data_part_1$rank_U4_premise_2 <- factor(raw_data_part_1$rank_U4_premise_2, 
                                            levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U4_premise_2)[1] <- "1"
levels(raw_data_part_1$rank_U4_premise_2)[2] <- "2"
levels(raw_data_part_1$rank_U4_premise_2)[3] <- "3"
levels(raw_data_part_1$rank_U4_premise_2)[4] <- "4"
levels(raw_data_part_1$rank_U4_premise_2)[5] <- "5"
levels(raw_data_part_1$rank_U4_premise_2)[6] <- "6"
levels(raw_data_part_1$rank_U4_premise_2)[7] <- "7"
levels(raw_data_part_1$rank_U4_premise_2)[8] <- "8"
levels(raw_data_part_1$rank_U4_premise_2)[9] <- "9"
levels(raw_data_part_1$rank_U4_premise_2)[10] <- "10"
summary(raw_data_part_1$rank_U4_premise_2)

raw_data_part_1$rank_U4_premise_3 <- factor(raw_data_part_1$rank_U4_premise_3, 
                                            levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U4_premise_3)[1] <- "1"
levels(raw_data_part_1$rank_U4_premise_3)[2] <- "2"
levels(raw_data_part_1$rank_U4_premise_3)[3] <- "3"
levels(raw_data_part_1$rank_U4_premise_3)[4] <- "4"
levels(raw_data_part_1$rank_U4_premise_3)[5] <- "5"
levels(raw_data_part_1$rank_U4_premise_3)[6] <- "6"
levels(raw_data_part_1$rank_U4_premise_3)[7] <- "7"
levels(raw_data_part_1$rank_U4_premise_3)[8] <- "8"
levels(raw_data_part_1$rank_U4_premise_3)[9] <- "9"
levels(raw_data_part_1$rank_U4_premise_3)[10] <- "10"
summary(raw_data_part_1$rank_U4_premise_3)

raw_data_part_1$rank_U4_premise_4 <- factor(raw_data_part_1$rank_U4_premise_4, 
                                            levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U4_premise_4)[1] <- "1"
levels(raw_data_part_1$rank_U4_premise_4)[2] <- "2"
levels(raw_data_part_1$rank_U4_premise_4)[3] <- "3"
levels(raw_data_part_1$rank_U4_premise_4)[4] <- "4"
levels(raw_data_part_1$rank_U4_premise_4)[5] <- "5"
levels(raw_data_part_1$rank_U4_premise_4)[6] <- "6"
levels(raw_data_part_1$rank_U4_premise_4)[7] <- "7"
levels(raw_data_part_1$rank_U4_premise_4)[8] <- "8"
levels(raw_data_part_1$rank_U4_premise_4)[9] <- "9"
levels(raw_data_part_1$rank_U4_premise_4)[10] <- "10"
summary(raw_data_part_1$rank_U4_premise_4)

raw_data_part_1$rank_U4_conclusion_1 <- factor(raw_data_part_1$rank_U4_conclusion_1, 
                                            levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U4_conclusion_1)[1] <- "1"
levels(raw_data_part_1$rank_U4_conclusion_1)[2] <- "2"
levels(raw_data_part_1$rank_U4_conclusion_1)[3] <- "3"
levels(raw_data_part_1$rank_U4_conclusion_1)[4] <- "4"
levels(raw_data_part_1$rank_U4_conclusion_1)[5] <- "5"
levels(raw_data_part_1$rank_U4_conclusion_1)[6] <- "6"
levels(raw_data_part_1$rank_U4_conclusion_1)[7] <- "7"
levels(raw_data_part_1$rank_U4_conclusion_1)[8] <- "8"
levels(raw_data_part_1$rank_U4_conclusion_1)[9] <- "9"
levels(raw_data_part_1$rank_U4_conclusion_1)[10] <- "10"
summary(raw_data_part_1$rank_U4_conclusion_1)

raw_data_part_1$rank_U4_conclusion_2 <- factor(raw_data_part_1$rank_U4_conclusion_2, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U4_conclusion_2)[1] <- "1"
levels(raw_data_part_1$rank_U4_conclusion_2)[2] <- "2"
levels(raw_data_part_1$rank_U4_conclusion_2)[3] <- "3"
levels(raw_data_part_1$rank_U4_conclusion_2)[4] <- "4"
levels(raw_data_part_1$rank_U4_conclusion_2)[5] <- "5"
levels(raw_data_part_1$rank_U4_conclusion_2)[6] <- "6"
levels(raw_data_part_1$rank_U4_conclusion_2)[7] <- "7"
levels(raw_data_part_1$rank_U4_conclusion_2)[8] <- "8"
levels(raw_data_part_1$rank_U4_conclusion_2)[9] <- "9"
levels(raw_data_part_1$rank_U4_conclusion_2)[10] <- "10"
summary(raw_data_part_1$rank_U4_conclusion_2)

raw_data_part_1$rank_U5_conclusion <- factor(raw_data_part_1$rank_U5_conclusion, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_U5_conclusion)[1] <- "1"
levels(raw_data_part_1$rank_U5_conclusion)[2] <- "2"
levels(raw_data_part_1$rank_U5_conclusion)[3] <- "3"
levels(raw_data_part_1$rank_U5_conclusion)[4] <- "4"
levels(raw_data_part_1$rank_U5_conclusion)[5] <- "5"
levels(raw_data_part_1$rank_U5_conclusion)[6] <- "6"
levels(raw_data_part_1$rank_U5_conclusion)[7] <- "7"
levels(raw_data_part_1$rank_U5_conclusion)[8] <- "8"
levels(raw_data_part_1$rank_U5_conclusion)[9] <- "9"
levels(raw_data_part_1$rank_U5_conclusion)[10] <- "10"
summary(raw_data_part_1$rank_U5_conclusion)

# remove rows for which participants did not complete part 2 of the survey
raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A1BAUJXS46OWDO")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A1BIJGB7XA0GME")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A1PR74OHURJNTO")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A1V3I0EG3DVY29")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A1YQY0AAF65RMZ")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A2VE2JM0RFYIOU")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A2ZDEERVRN5AMC")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A3A8P4UR9A0DWQ")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A3BO3HMV0F7Q5H")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A3S3WYVCVWW8IZ")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A3UDUHUVFKD833")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "A71NZTTHS4AMB")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(!mturk_ID == "AFERIASET17LW")
                      
                      
# remove mturk_id column which contains personal identifiable information
raw_data_part_1 <- raw_data_part_1%>% 
  select(!mturk_ID)

# for each rank, count the rank and its percentage frequency (N = 37)
data_U1_conclusion <- raw_data_part_1 %>% 
  count(rank_U1_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U1_conclusion) %>% 
  select(rank_U1_conclusion, percentage)

data_U2_premise <- raw_data_part_1 %>% 
  count(rank_U2_premise, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U2_premise) %>% 
  select(rank_U2_premise, percentage)

data_U2_conclusion_1 <- raw_data_part_1 %>% 
  count(rank_U2_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U2_conclusion_1) %>% 
  select(rank_U2_conclusion_1, percentage)

data_U2_conclusion_2 <- raw_data_part_1 %>% 
  count(rank_U2_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U2_conclusion_2) %>% 
  select(rank_U2_conclusion_2, percentage)

data_U3_premise <- raw_data_part_1 %>% 
  count(rank_U3_premise, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U3_premise) %>% 
  select(rank_U3_premise, percentage)

data_U3_conclusion <- raw_data_part_1 %>% 
  count(rank_U3_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U3_conclusion) %>% 
  select(rank_U3_conclusion, percentage)

data_U4_premise_1 <- raw_data_part_1 %>% 
  count(rank_U4_premise_1, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U4_premise_1) %>% 
  select(rank_U4_premise_1, percentage)

data_U4_premise_2 <- raw_data_part_1 %>% 
  count(rank_U4_premise_2, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U4_premise_2) %>% 
  select(rank_U4_premise_2, percentage)

data_U4_premise_3 <- raw_data_part_1 %>% 
  count(rank_U4_premise_3, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U4_premise_3) %>% 
  select(rank_U4_premise_3, percentage)

data_U4_premise_4 <- raw_data_part_1 %>% 
  count(rank_U4_premise_4, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U4_premise_4) %>% 
  select(rank_U4_premise_4, percentage)

data_U4_conclusion_1 <- raw_data_part_1 %>% 
  count(rank_U4_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U4_conclusion_1) %>% 
  select(rank_U4_conclusion_1, percentage)

data_U4_conclusion_2 <- raw_data_part_1 %>% 
  count(rank_U4_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U4_conclusion_2) %>% 
  select(rank_U4_conclusion_2, percentage)

data_U5_conclusion <- raw_data_part_1 %>% 
  count(rank_U5_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/37*100) %>% 
  arrange(rank_U5_conclusion) %>% 
  select(rank_U5_conclusion, percentage)

# create a tibble with only demographic data
demographic_data <- raw_data_part_1 %>% 
  select(age, gender) %>% 
  arrange(age)

# create a tibble with age and its frequency 
age_data <- raw_data_part_1 %>% 
  count(age, sort = TRUE) %>% 
  arrange(age)

# create a tibble with gender and its frequency 
gender_data <- raw_data_part_1 %>% 
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

rank_U1_conclusion_bar <- ggplot(data = data_U1_conclusion,
                     mapping = aes(x = rank_U1_conclusion,
                                   y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "U1 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U1_conclusion_bar


rank_U2_premise_bar <- ggplot(data = data_U2_premise,
                                   mapping = aes(x = rank_U2_premise,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U2 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U2_premise_bar

rank_U2_conclusion_1_bar <- ggplot(data = data_U2_conclusion_1,
                                   mapping = aes(x = rank_U2_conclusion_1,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U2 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U2_conclusion_1_bar


rank_U2_conclusion_2_bar <- ggplot(data = data_U2_conclusion_2,
                                   mapping = aes(x = rank_U2_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U2 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U2_conclusion_2_bar

rank_U3_premise_bar <- ggplot(data = data_U3_premise,
                                   mapping = aes(x = rank_U3_premise,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U3 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U3_premise_bar

rank_U3_conclusion_bar <- ggplot(data = data_U3_conclusion,
                              mapping = aes(x = rank_U3_conclusion,
                                            y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U3 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U3_conclusion_bar

rank_U4_premise_1_bar <- ggplot(data = data_U4_premise_1,
                                 mapping = aes(x = rank_U4_premise_1,
                                               y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U4 premise 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U4_premise_1_bar


rank_U4_premise_2_bar <- ggplot(data = data_U4_premise_2,
                                mapping = aes(x = rank_U4_premise_2,
                                              y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U4 premise 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U4_premise_2_bar

rank_U4_premise_3_bar <- ggplot(data = data_U4_premise_3,
                                mapping = aes(x = rank_U4_premise_3,
                                              y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U4 premise 3",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U4_premise_3_bar

rank_U4_premise_4_bar <- ggplot(data = data_U4_premise_4,
                                mapping = aes(x = rank_U4_premise_4,
                                              y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U4 premise 4",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U4_premise_4_bar

rank_U4_conclusion_1_bar <- ggplot(data = data_U4_conclusion_1,
                                 mapping = aes(x = rank_U4_conclusion_1,
                                               y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U4 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U4_conclusion_1_bar

rank_U4_conclusion_2_bar <- ggplot(data = data_U4_conclusion_2,
                                   mapping = aes(x = rank_U4_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U4 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U4_conclusion_2_bar

rank_U5_conclusion_bar <- ggplot(data = data_U5_conclusion,
                                   mapping = aes(x = rank_U5_conclusion,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "U5 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_U5_conclusion_bar

# combine plots of all rules formatted as a 5 x 3 matrix of plots on a single page
rank_U1_to_U5_plot <- plot_grid(rank_U1_conclusion_bar,
                                rank_U2_premise_bar,
                                rank_U2_conclusion_1_bar,
                                rank_U2_conclusion_2_bar,
                                rank_U3_premise_bar,
                                rank_U3_conclusion_bar,
                                rank_U4_premise_1_bar,
                                rank_U4_premise_2_bar,
                                rank_U4_premise_3_bar,
                                rank_U4_premise_4_bar,
                                rank_U4_conclusion_1_bar,
                                rank_U4_conclusion_2_bar,
                                rank_U5_conclusion_bar,
                                ncol = 3,
                                nrow = 5)

rank_U1_to_U5_plot

ggsave(plot = rank_U1_to_U5_plot,
       filename = "figure_output/my_plot_U1_to_U5.jpg")

# view summary of cleaned data 
str(raw_data_part_1)
summary(raw_data_part_1)

# write cleaned data to file
write_csv(x = raw_data_part_1,
          file = "data_output/cleaned_data_U1_to_U5.csv")
