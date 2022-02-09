# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)

# read in raw data for raw data part 1 R1 to R4
raw_data_part_1 <- read_csv("data_input/raw_data_part_1_R1_to_R4.csv", 
                          col_names = c("timestamp_raw", #assign new column names
                                        "captcha_code",
                                        "consent",
                                        "inclusion_criteria",
                                        "age",
                                        "gender",
                                        "practice_question",
                                        "rank_R1_conclusion_1",
                                        "rank_R1_conclusion_2",
                                        "rank_R2_premise",
                                        "rank_R2_conclusion",
                                        "rank_R3_premise",
                                        "rank_R3_conclusion_1",
                                        "rank_R3_conclusion_2",
                                        "rank_R4_conclusion",
                                        "mturk_ID_consent",
                                        "mturk_ID",
                                        "future_participation",
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

raw_data_part_1$rank_R1_conclusion_2 <- factor(raw_data_part_1$rank_R1_conclusion_2, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R1_conclusion_2)[1] <- "1"
levels(raw_data_part_1$rank_R1_conclusion_2)[2] <- "2"
levels(raw_data_part_1$rank_R1_conclusion_2)[3] <- "3"
levels(raw_data_part_1$rank_R1_conclusion_2)[4] <- "4"
levels(raw_data_part_1$rank_R1_conclusion_2)[5] <- "5"
levels(raw_data_part_1$rank_R1_conclusion_2)[6] <- "6"
levels(raw_data_part_1$rank_R1_conclusion_2)[7] <- "7"
levels(raw_data_part_1$rank_R1_conclusion_2)[8] <- "8"
levels(raw_data_part_1$rank_R1_conclusion_2)[9] <- "9"
levels(raw_data_part_1$rank_R1_conclusion_2)[10] <- "10"
summary(raw_data_part_1$rank_R1_conclusion_2)

raw_data_part_1$rank_R2_premise <- factor(raw_data_part_1$rank_R2_premise, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R2_premise)[1] <- "1"
levels(raw_data_part_1$rank_R2_premise)[2] <- "2"
levels(raw_data_part_1$rank_R2_premise)[3] <- "3"
levels(raw_data_part_1$rank_R2_premise)[4] <- "4"
levels(raw_data_part_1$rank_R2_premise)[5] <- "5"
levels(raw_data_part_1$rank_R2_premise)[6] <- "6"
levels(raw_data_part_1$rank_R2_premise)[7] <- "7"
levels(raw_data_part_1$rank_R2_premise)[8] <- "8"
levels(raw_data_part_1$rank_R2_premise)[9] <- "9"
levels(raw_data_part_1$rank_R2_premise)[10] <- "10"
summary(raw_data_part_1$rank_R2_premise)

raw_data_part_1$rank_R2_conclusion <- factor(raw_data_part_1$rank_R2_conclusion, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R2_conclusion)[1] <- "1"
levels(raw_data_part_1$rank_R2_conclusion)[2] <- "2"
levels(raw_data_part_1$rank_R2_conclusion)[3] <- "3"
levels(raw_data_part_1$rank_R2_conclusion)[4] <- "4"
levels(raw_data_part_1$rank_R2_conclusion)[5] <- "5"
levels(raw_data_part_1$rank_R2_conclusion)[6] <- "6"
levels(raw_data_part_1$rank_R2_conclusion)[7] <- "7"
levels(raw_data_part_1$rank_R2_conclusion)[8] <- "8"
levels(raw_data_part_1$rank_R2_conclusion)[9] <- "9"
levels(raw_data_part_1$rank_R2_conclusion)[10] <- "10"
summary(raw_data_part_1$rank_R2_conclusion)


raw_data_part_1$rank_R3_premise <- factor(raw_data_part_1$rank_R3_premise, 
                                             levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R3_premise)[1] <- "1"
levels(raw_data_part_1$rank_R3_premise)[2] <- "2"
levels(raw_data_part_1$rank_R3_premise)[3] <- "3"
levels(raw_data_part_1$rank_R3_premise)[4] <- "4"
levels(raw_data_part_1$rank_R3_premise)[5] <- "5"
levels(raw_data_part_1$rank_R3_premise)[6] <- "6"
levels(raw_data_part_1$rank_R3_premise)[7] <- "7"
levels(raw_data_part_1$rank_R3_premise)[8] <- "8"
levels(raw_data_part_1$rank_R3_premise)[9] <- "9"
levels(raw_data_part_1$rank_R3_premise)[10] <- "10"
summary(raw_data_part_1$rank_R3_premise)

raw_data_part_1$rank_R3_conclusion_1 <- factor(raw_data_part_1$rank_R3_conclusion_1, 
                                          levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R3_conclusion_1)[1] <- "1"
levels(raw_data_part_1$rank_R3_conclusion_1)[2] <- "2"
levels(raw_data_part_1$rank_R3_conclusion_1)[3] <- "3"
levels(raw_data_part_1$rank_R3_conclusion_1)[4] <- "4"
levels(raw_data_part_1$rank_R3_conclusion_1)[5] <- "5"
levels(raw_data_part_1$rank_R3_conclusion_1)[6] <- "6"
levels(raw_data_part_1$rank_R3_conclusion_1)[7] <- "7"
levels(raw_data_part_1$rank_R3_conclusion_1)[8] <- "8"
levels(raw_data_part_1$rank_R3_conclusion_1)[9] <- "9"
levels(raw_data_part_1$rank_R3_conclusion_1)[10] <- "10"
summary(raw_data_part_1$rank_R3_conclusion_1)

raw_data_part_1$rank_R3_conclusion_2 <- factor(raw_data_part_1$rank_R3_conclusion_2, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R3_conclusion_2)[1] <- "1"
levels(raw_data_part_1$rank_R3_conclusion_2)[2] <- "2"
levels(raw_data_part_1$rank_R3_conclusion_2)[3] <- "3"
levels(raw_data_part_1$rank_R3_conclusion_2)[4] <- "4"
levels(raw_data_part_1$rank_R3_conclusion_2)[5] <- "5"
levels(raw_data_part_1$rank_R3_conclusion_2)[6] <- "6"
levels(raw_data_part_1$rank_R3_conclusion_2)[7] <- "7"
levels(raw_data_part_1$rank_R3_conclusion_2)[8] <- "8"
levels(raw_data_part_1$rank_R3_conclusion_2)[9] <- "9"
levels(raw_data_part_1$rank_R3_conclusion_2)[10] <- "10"
summary(raw_data_part_1$rank_R3_conclusion_2)

raw_data_part_1$rank_R4_conclusion <- factor(raw_data_part_1$rank_R4_conclusion, 
                                               levels = c(1,2,3,4,5,6,7,8,9,10))
levels(raw_data_part_1$rank_R4_conclusion)[1] <- "1"
levels(raw_data_part_1$rank_R4_conclusion)[2] <- "2"
levels(raw_data_part_1$rank_R4_conclusion)[3] <- "3"
levels(raw_data_part_1$rank_R4_conclusion)[4] <- "4"
levels(raw_data_part_1$rank_R4_conclusion)[5] <- "5"
levels(raw_data_part_1$rank_R4_conclusion)[6] <- "6"
levels(raw_data_part_1$rank_R4_conclusion)[7] <- "7"
levels(raw_data_part_1$rank_R4_conclusion)[8] <- "8"
levels(raw_data_part_1$rank_R4_conclusion)[9] <- "9"
levels(raw_data_part_1$rank_R4_conclusion)[10] <- "10"
summary(raw_data_part_1$rank_R4_conclusion)


# remove rows for which participants did not complete part 2 of the survey
valid_mturk_IDs <- c("A10T0161B70BXP",
                       "A122LRCSBAD6DC",
                        "A13B52O6OQBPO",
                        "A17PAE30BGBO1T",
                        "A18SXC3JEN1O0U",
                       "A1G187YBG0DVMQ",
                       "A1JJYY622DGE5L",
                       "A1NZFJHVJ9CNTO",
                       "A1P2RQ166VS5BT",
                       "A1V2H0UF94ATWY",
                       "A23KAJRDVCVGOE",
                       "A2EMTSJS47K8SU",
                       "A2P8V5SKYLL5I4",
                       "A2VRDE2FHCBMF8",
                       "A308FEI88TJ8WV",
                       "A34CPKFZXBX1PO",
                       "A34DYM8J0X5VK",
                       "A3CTXNQ2GXIQSP",
                       "A3HOBJ4PJUOCUN",
                       "A3JC9VPPTHNKVL",
                       "A7204W9E8IR1R",
                       "A7P3R1AIA4TVV",
                       "ADAL2H41ZBWAK",
                       "AGC307XG0RIR4",
                       "AHYTT3T8W9Y9Y",
                       "AJM4334V07JDQ",
                       "AJXIC6Q5EM76P",
                       "AM2WGEJDWJY1I",                        
                       "AMPMTF5IAAMK8",
                       "ANPCXN619ACW9",
                       "AOS2PVHT2HYTL",
                       "APPOO1EBDMS7M",
                       "AW0K78T4I2T72",
                       "AWKJJC3QFU9VW",
                       "AZ8JL3QNIPY4U")

raw_data_part_1 <- raw_data_part_1 %>% 
  filter(mturk_ID %in% valid_mturk_IDs)
                      
                      
# remove mturk_id column which contains personal identifiable information
raw_data_part_1 <- raw_data_part_1%>% 
  select(!mturk_ID)

# for each rank, count the rank and its percentage frequency (N = 35)
data_R1_conclusion_1 <- raw_data_part_1 %>% 
  count(rank_R1_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R1_conclusion_1) %>% 
  select(rank_R1_conclusion_1, percentage)

data_R1_conclusion_2 <- raw_data_part_1 %>% 
  count(rank_R1_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R1_conclusion_2) %>% 
  select(rank_R1_conclusion_2, percentage)

data_R2_premise <- raw_data_part_1 %>% 
  count(rank_R2_premise, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R2_premise) %>% 
  select(rank_R2_premise, percentage)

data_R2_conclusion <- raw_data_part_1 %>% 
  count(rank_R2_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R2_conclusion) %>% 
  select(rank_R2_conclusion, percentage)

data_R3_premise <- raw_data_part_1 %>% 
  count(rank_R3_premise, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R3_premise) %>% 
  select(rank_R3_premise, percentage)

data_R3_conclusion_1 <- raw_data_part_1 %>% 
  count(rank_R3_conclusion_1, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R3_conclusion_1) %>% 
  select(rank_R3_conclusion_1, percentage)

data_R3_conclusion_2 <- raw_data_part_1 %>% 
  count(rank_R3_conclusion_2, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R3_conclusion_2) %>% 
  select(rank_R3_conclusion_2, percentage)

data_R4_conclusion <- raw_data_part_1 %>% 
  count(rank_R4_conclusion, sort = TRUE) %>% 
  mutate(percentage = n/35*100) %>% 
  arrange(rank_R4_conclusion) %>% 
  select(rank_R4_conclusion, percentage)

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

# visualisation of rank and percentage frequency for postulates R1 to R4
# create a bar plot of the % frequency of each rank per rule (premise / conclusion) in each postulate
# combine all the plots into a single plot
# save the combined plot

rank_R1_conclusion_1_bar <- ggplot(data = data_R1_conclusion_1,
                     mapping = aes(x = rank_R1_conclusion_1,
                                   y = percentage)) +
  geom_bar(stat="identity") + #stat argument needed to include y variable in aes function for bar plot
  labs(title = "R1 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R1_conclusion_1_bar

rank_R1_conclusion_2_bar <- ggplot(data = data_R1_conclusion_2,
                                   mapping = aes(x = rank_R1_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "R1 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R1_conclusion_2_bar

rank_R2_premise_bar <- ggplot(data = data_R2_premise,
                                   mapping = aes(x = rank_R2_premise,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "R2 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R2_premise_bar

rank_R2_conclusion_bar <- ggplot(data = data_R2_conclusion,
                              mapping = aes(x = rank_R2_conclusion,
                                            y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "R2 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R2_conclusion_bar

rank_R3_premise_bar <- ggplot(data = data_R3_premise,
                                 mapping = aes(x = rank_R3_premise,
                                               y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "R3 premise",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R3_premise_bar

rank_R3_conclusion_1_bar <- ggplot(data = data_R3_conclusion_1,
                                 mapping = aes(x = rank_R3_conclusion_1,
                                               y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "R3 conclusion 1",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R3_conclusion_1_bar

rank_R3_conclusion_2_bar <- ggplot(data = data_R3_conclusion_2,
                                   mapping = aes(x = rank_R3_conclusion_2,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "R3 conclusion 2",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R3_conclusion_2_bar

rank_R4_conclusion_bar <- ggplot(data = data_R4_conclusion,
                                   mapping = aes(x = rank_R4_conclusion,
                                                 y = percentage)) +
  geom_bar(stat="identity") +
  labs(title = "R4 conclusion",
       x = "Rank",
       y = "Frequency (%)") +
  ylim(0,100) +
  grey_theme

rank_R4_conclusion_bar


rank_R1_to_R4_plot <- rank_R1_conclusion_1_bar +
  rank_R1_conclusion_2_bar +
  rank_R2_premise_bar +
  rank_R2_conclusion_bar +
  rank_R3_premise_bar +
  rank_R3_conclusion_1_bar +
  rank_R3_conclusion_2_bar +
  rank_R4_conclusion_bar

rank_R1_to_R4_plot

plot_layout()

ggsave(plot = rank_R1_to_R4_plot,
       filename = "figure_output/my_plot_R1_to_R4.jpg")

# view summary of cleaned data 
str(raw_data_part_1)
summary(raw_data_part_1)

# write cleaned data to file
write_csv(x = raw_data_part_1,
          file = "data_output/raw-data-part-1-cleaned.csv")
