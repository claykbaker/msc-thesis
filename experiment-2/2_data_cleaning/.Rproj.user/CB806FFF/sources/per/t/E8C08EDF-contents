# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)

# read in raw data for mini survey 1
mini_survey_1 <- read_csv("data_input/mini-survey-1.csv", 
                          col_names = c("timestamp_raw", #assign new column names
                                        "consent",
                                        "captcha_code",
                                        "language_question",
                                        "rank_statement_1",
                                        "explanation_statement_1",
                                        "rank_statement_2",
                                        "explanation_statement_2",
                                        "rank_statement_3",
                                        "explanation_statement_3",
                                        "rank_statement_4",
                                        "explanation_statement_4",
                                        "rank_statement_5",
                                        "explanation_statement_5",
                                        "rank_statement_6",
                                        "explanation_statement_6",
                                        "mturk_ID_consent",
                                        "mturk_ID",
                                        "feedback"),
                          skip = 1, # ignore first row which contains old column values                        
                          na ="",  # assign NA (null) to empty cells
                          trim_ws = TRUE) # trim whitespace

# inspecting the raw data
str(mini_survey_1)
summary(mini_survey_1)

# arrange survey data by ascending order of MTurk IDs
mini_survey_1 <- mini_survey_1 %>% 
  arrange(mturk_ID)

# remove rows which contain duplicate MTurk IDs (only 30 unique participants per survey)
mini_survey_1 <- mini_survey_1[!duplicated(mini_survey_1$mturk_ID), ]

# remove rows where consent is not given
mini_survey_1 <- mini_survey_1 %>% 
  filter(!consent == "No")

# create columns for date components of timestamp into separate columns: y, m, d, h,  m, s
mini_survey_1 <- mini_survey_1 %>% 
  mutate(timestamp_formatted = timestamp(mini_survey_1$timestamp_raw)) %>% 
  mutate(date = ymd_hms(timestamp_formatted, tz = "EET")) %>% 
  mutate(year = substring(date, first = 1, last = 4)) %>% 
  mutate(month = substring(date, first = 6, last = 7)) %>% 
  mutate(day = substring(date, first = 9, last = 10)) %>% 
  mutate(hour = substring(date, first = 12, last = 13)) %>% 
  mutate(minute = substring(date, first = 15, last = 16)) %>% 
  mutate(second = substring(date, first = 18, last = 19)) 

# convert date components from characters to numbers
mini_survey_1$year <- as.numeric(mini_survey_1$year)
mini_survey_1$month <- as.numeric(mini_survey_1$month)
mini_survey_1$day <- as.numeric(mini_survey_1$day)
mini_survey_1$hour <- as.numeric(mini_survey_1$hour)
mini_survey_1$minute <- as.numeric(mini_survey_1$minute)
mini_survey_1$second <- as.numeric(mini_survey_1$second)

# convert date from character to date
mini_survey_1$date <- as.Date(mini_survey_1$date)

# remove timestamp and date fields as components are stored separately
mini_survey_1 <- mini_survey_1 %>% 
  select(-c(timestamp_raw, timestamp_formatted, date))
 
# factor ranks according to scale used in survey
mini_survey_1$rank_statement_1 <- factor(mini_survey_1$rank_statement_1, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_1$rank_statement_1)[1] <- "strongly disagree"
levels(mini_survey_1$rank_statement_1)[2] <- "disagree"
levels(mini_survey_1$rank_statement_1)[3] <- "neutral"
levels(mini_survey_1$rank_statement_1)[4] <- "agree"
levels(mini_survey_1$rank_statement_1)[5] <- "strongly agree"
summary(mini_survey_1$rank_statement_1)

mini_survey_1$rank_statement_2 <- factor(mini_survey_1$rank_statement_2, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_1$rank_statement_2)[1] <- "strongly disagree"
levels(mini_survey_1$rank_statement_2)[2] <- "disagree"
levels(mini_survey_1$rank_statement_2)[3] <- "neutral"
levels(mini_survey_1$rank_statement_2)[4] <- "agree"
levels(mini_survey_1$rank_statement_2)[5] <- "strongly agree"
summary(mini_survey_1$rank_statement_2)

mini_survey_1$rank_statement_3 <- factor(mini_survey_1$rank_statement_3, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_1$rank_statement_3)[1] <- "strongly disagree"
levels(mini_survey_1$rank_statement_3)[2] <- "disagree"
levels(mini_survey_1$rank_statement_3)[3] <- "neutral"
levels(mini_survey_1$rank_statement_3)[4] <- "agree"
levels(mini_survey_1$rank_statement_3)[5] <- "strongly agree"
summary(mini_survey_1$rank_statement_3)

mini_survey_1$rank_statement_4 <- factor(mini_survey_1$rank_statement_4, 
                                         levels = c(1,2,3,4,5))

levels(mini_survey_1$rank_statement_4)[1] <- "strongly disagree"
levels(mini_survey_1$rank_statement_4)[2] <- "disagree"
levels(mini_survey_1$rank_statement_4)[3] <- "neutral"
levels(mini_survey_1$rank_statement_4)[4] <- "agree"
levels(mini_survey_1$rank_statement_4)[5] <- "strongly agree"
summary(mini_survey_1$rank_statement_4)

mini_survey_1$rank_statement_5 <- factor(mini_survey_1$rank_statement_5, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_1$rank_statement_5)[1] <- "strongly disagree"
levels(mini_survey_1$rank_statement_5)[2] <- "disagree"
levels(mini_survey_1$rank_statement_5)[3] <- "neutral"
levels(mini_survey_1$rank_statement_5)[4] <- "agree"
levels(mini_survey_1$rank_statement_5)[5] <- "strongly agree"
summary(mini_survey_1$rank_statement_5)

mini_survey_1$rank_statement_6 <- factor(mini_survey_1$rank_statement_6, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_1$rank_statement_6)[1] <- "strongly disagree"
levels(mini_survey_1$rank_statement_6)[2] <- "disagree"
levels(mini_survey_1$rank_statement_6)[3] <- "neutral"
levels(mini_survey_1$rank_statement_6)[4] <- "agree"
levels(mini_survey_1$rank_statement_6)[5] <- "strongly agree"
summary(mini_survey_1$rank_statement_6)

# remove mturk_id column which contains personal identifiable information
mini_survey_1 <- mini_survey_1 %>% 
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

rank_1_bar <- ggplot(data = mini_survey_1,
                     mapping = aes(x = rank_statement_1)) +
  geom_bar() +
  labs(title = "Statement 1",
         x = "Rank",
         y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_1_bar

rank_2_bar <- ggplot(data = mini_survey_1,
                     mapping = aes(x = rank_statement_2)) +
  geom_bar() +
  labs(title = "Statement 2",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_2_bar 

rank_3_bar <- ggplot(data = mini_survey_1,
                                mapping = aes(x = rank_statement_3)) +
  geom_bar() +
  labs(title = "Statement 3",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_3_bar 

rank_4_bar <- ggplot(data = mini_survey_1,
                     mapping = aes(x = rank_statement_4)) +
  geom_bar() +
  labs(title = "Statement 4",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_4_bar  

rank_5_bar <- ggplot(data = mini_survey_1,
                     mapping = aes(x = rank_statement_5)) +
  geom_bar() +
  labs(title = "Statement 5",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_5_bar

rank_6_bar <- ggplot(data = mini_survey_1,
                     mapping = aes(x = rank_statement_6)) +
  geom_bar() +
  labs(title = "Statement 6",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_6_bar

rank_1_to_6_plot <- rank_1_bar + 
                    rank_2_bar + 
                    rank_3_bar + 
                    rank_4_bar + 
                    rank_5_bar + 
                    rank_6_bar
plot_layout()

ggsave(plot = rank_1_to_6_plot,
       filename = "figure_output/my_plot_statement_1_to_6.jpg")

# view summary of cleaned data 
str(mini_survey_1)
summary(mini_survey_1)

# write cleaned data to file
write_csv(x = mini_survey_1,
          file = "data_output/mini-survey-1-cleaned.csv")
