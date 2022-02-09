# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)

# read in raw data for mini survey 3
mini_survey_2 <- read_csv("data_input/mini-survey-2.csv", 
                          col_names = c("timestamp_raw", #assign new column names
                                        "consent",
                                        "captcha_code",
                                        "language_question",
                                        "rank_statement_7",
                                        "explanation_statement_7",
                                        "rank_statement_8",
                                        "explanation_statement_8",
                                        "rank_statement_9",
                                        "explanation_statement_9",
                                        "rank_statement_10",
                                        "explanation_statement_10",
                                        "rank_statement_11",
                                        "explanation_statement_11",
                                        "rank_statement_12",
                                        "explanation_statement_12",
                                        "mturk_ID_consent",
                                        "mturk_ID",
                                        "feedback"),
                          skip = 1, # ignore first row which contains old column values                        
                          na ="",  # assign NA (null) to empty cells
                          trim_ws = TRUE) # trim whitespace

# inspecting the raw data
str(mini_survey_2)
summary(mini_survey_2)

# arrange survey data by ascending order of MTurk IDs
mini_survey_2 <- mini_survey_2 %>% 
  arrange(mturk_ID)

# remove rows which contain duplicate MTurk IDs (only 30 unique participants per survey)
mini_survey_2 <- mini_survey_2[!duplicated(mini_survey_2$mturk_ID), ]

# remove rows where consent is not given
mini_survey_2 <- mini_survey_2 %>% 
  filter(!consent == "No")

# create columns for date components of timestamp into separate columns: y, m, d, h,  m, s
mini_survey_2 <- mini_survey_2 %>% 
  mutate(timestamp_formatted = timestamp(mini_survey_2$timestamp_raw)) %>% 
  mutate(date = ymd_hms(timestamp_formatted, tz = "EET")) %>% 
  mutate(year = substring(date, first = 1, last = 4)) %>% 
  mutate(month = substring(date, first = 6, last = 7)) %>% 
  mutate(day = substring(date, first = 9, last = 10)) %>% 
  mutate(hour = substring(date, first = 12, last = 13)) %>% 
  mutate(minute = substring(date, first = 15, last = 16)) %>% 
  mutate(second = substring(date, first = 18, last = 19)) 

# convert date components from characters to numbers
mini_survey_2$year <- as.numeric(mini_survey_2$year)
mini_survey_2$month <- as.numeric(mini_survey_2$month)
mini_survey_2$day <- as.numeric(mini_survey_2$day)
mini_survey_2$hour <- as.numeric(mini_survey_2$hour)
mini_survey_2$minute <- as.numeric(mini_survey_2$minute)
mini_survey_2$second <- as.numeric(mini_survey_2$second)

# convert date from character to date
mini_survey_2$date <- as.Date(mini_survey_2$date)

# remove timestamp and date fields as components are stored separately
mini_survey_2 <- mini_survey_2 %>% 
  select(-c(timestamp_raw, timestamp_formatted, date))

# factor ranks according to scale used in survey
mini_survey_2$rank_statement_7 <- factor(mini_survey_2$rank_statement_7, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_2$rank_statement_7)[1] <- "strongly disagree"
levels(mini_survey_2$rank_statement_7)[2] <- "disagree"
levels(mini_survey_2$rank_statement_7)[3] <- "neutral"
levels(mini_survey_2$rank_statement_7)[4] <- "agree"
levels(mini_survey_2$rank_statement_7)[5] <- "strongly agree"
summary(mini_survey_2$rank_statement_7)

mini_survey_2$rank_statement_8 <- factor(mini_survey_2$rank_statement_8, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_2$rank_statement_8)[1] <- "strongly disagree"
levels(mini_survey_2$rank_statement_8)[2] <- "disagree"
levels(mini_survey_2$rank_statement_8)[3] <- "neutral"
levels(mini_survey_2$rank_statement_8)[4] <- "agree"
levels(mini_survey_2$rank_statement_8)[5] <- "strongly agree"
summary(mini_survey_2$rank_statement_8)

mini_survey_2$rank_statement_9 <- factor(mini_survey_2$rank_statement_9, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_2$rank_statement_9)[1] <- "strongly disagree"
levels(mini_survey_2$rank_statement_9)[2] <- "disagree"
levels(mini_survey_2$rank_statement_9)[3] <- "neutral"
levels(mini_survey_2$rank_statement_9)[4] <- "agree"
levels(mini_survey_2$rank_statement_9)[5] <- "strongly agree"
summary(mini_survey_2$rank_statement_9)

mini_survey_2$rank_statement_10 <- factor(mini_survey_2$rank_statement_10, 
                                         levels = c(1,2,3,4,5))

levels(mini_survey_2$rank_statement_10)[1] <- "strongly disagree"
levels(mini_survey_2$rank_statement_10)[2] <- "disagree"
levels(mini_survey_2$rank_statement_10)[3] <- "neutral"
levels(mini_survey_2$rank_statement_10)[4] <- "agree"
levels(mini_survey_2$rank_statement_10)[5] <- "strongly agree"
summary(mini_survey_2$rank_statement_10)

mini_survey_2$rank_statement_11 <- factor(mini_survey_2$rank_statement_11, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_2$rank_statement_11)[1] <- "strongly disagree"
levels(mini_survey_2$rank_statement_11)[2] <- "disagree"
levels(mini_survey_2$rank_statement_11)[3] <- "neutral"
levels(mini_survey_2$rank_statement_11)[4] <- "agree"
levels(mini_survey_2$rank_statement_11)[5] <- "strongly agree"
summary(mini_survey_2$rank_statement_11)

mini_survey_2$rank_statement_12 <- factor(mini_survey_2$rank_statement_12, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_2$rank_statement_12)[1] <- "strongly disagree"
levels(mini_survey_2$rank_statement_12)[2] <- "disagree"
levels(mini_survey_2$rank_statement_12)[3] <- "neutral"
levels(mini_survey_2$rank_statement_12)[4] <- "agree"
levels(mini_survey_2$rank_statement_12)[5] <- "strongly agree"
summary(mini_survey_2$rank_statement_12)

# remove mturk_id column which contains personal identifiable information
mini_survey_2 <- mini_survey_2 %>% 
  select(!mturk_ID)

# visualisation of rank for statements 1 to 6
# create a bar plot of the frequency of each rank per statement
# combine all the plots into a single plot
# save the combined plot

grey_theme <- theme(text = element_text(size = 16),
                    axis.text.x = element_text(color = "grey20",
                                               size = 10,
                                               angle = 45,
                                               hjust = 0.5,
                                               vjust = 0.5),
                    axis.text.y = element_text(color = "grey20",
                                               size = 10, 
                                               hjust = 0.5,
                                               vjust = 0.5),
                    axis.title.x = element_text(size = 14),
                    axis.title.y = element_text(size=14))

rank_7_bar <- ggplot(data = mini_survey_2,
                     mapping = aes(x = rank_statement_7)) +
  geom_bar() +
  labs(title = "Statement 7",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_7_bar

rank_8_bar <- ggplot(data = mini_survey_2,
                     mapping = aes(x = rank_statement_8)) +
  geom_bar() +
  labs(title = "Statement 8",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_8_bar 

rank_9_bar <- ggplot(data = mini_survey_2,
                     mapping = aes(x = rank_statement_9)) +
  geom_bar() +
  labs(title = "Statement 9",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_9_bar 

rank_10_bar <- ggplot(data = mini_survey_2,
                     mapping = aes(x = rank_statement_10)) +
  geom_bar() +
  labs(title = "Statement 10",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_10_bar  

rank_11_bar <- ggplot(data = mini_survey_2,
                     mapping = aes(x = rank_statement_11)) +
  geom_bar() +
  labs(title = "Statement 11",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_11_bar

rank_12_bar <- ggplot(data = mini_survey_2,
                     mapping = aes(x = rank_statement_12)) +
  geom_bar() +
  labs(title = "Statement 12",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_12_bar

rank_7_to_12_plot <- rank_7_bar + 
  rank_8_bar + 
  rank_9_bar + 
  rank_10_bar + 
  rank_11_bar + 
  rank_12_bar
plot_layout()

ggsave(plot = rank_7_to_12_plot,
       filename = "figure_output/my_plot_statement_7_to_12.jpg")

# view summary of cleaned data 
str(mini_survey_2)
summary(mini_survey_2)

# write cleaned data to file
write_csv(x = mini_survey_2,
          file = "data_output/mini-survey-2-cleaned.csv")
