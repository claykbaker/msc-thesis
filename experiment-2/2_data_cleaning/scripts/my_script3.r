# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)

# read in raw data for mini survey 3
mini_survey_3 <- read_csv("data_input/mini-survey-3.csv", 
                          col_names = c("timestamp_raw", #assign new column names
                                        "consent",
                                        "captcha_code",
                                        "language_question",
                                        "rank_statement_13",
                                        "explanation_statement_13",
                                        "rank_statement_14",
                                        "explanation_statement_14",
                                        "rank_statement_15",
                                        "explanation_statement_15",
                                        "rank_statement_16",
                                        "explanation_statement_16",
                                        "rank_statement_17",
                                        "explanation_statement_17",
                                        "rank_statement_18",
                                        "explanation_statement_18",
                                        "mturk_ID_consent",
                                        "mturk_ID",
                                        "feedback"),
                          skip = 1, # ignore first row which contains old column values                        
                          na ="",  # assign NA (null) to empty cells
                          trim_ws = TRUE) # trim whitespace

# inspecting the raw data
str(mini_survey_3)
summary(mini_survey_3)

# arrange survey data by ascending order of MTurk IDs
mini_survey_3 <- mini_survey_3 %>% 
  arrange(mturk_ID)

# remove rows which contain duplicate MTurk IDs (only 30 unique participants per survey)
mini_survey_3 <- mini_survey_3[!duplicated(mini_survey_3$mturk_ID), ]

# remove rows where consent is not given
mini_survey_3 <- mini_survey_3 %>% 
  filter(!consent == "No")

# create columns for date components of timestamp into separate columns: y, m, d, h,  m, s
mini_survey_3 <- mini_survey_3 %>% 
  mutate(timestamp_formatted = timestamp(mini_survey_3$timestamp_raw)) %>% 
  mutate(date = ymd_hms(timestamp_formatted, tz = "EET")) %>% 
  mutate(year = substring(date, first = 1, last = 4)) %>% 
  mutate(month = substring(date, first = 6, last = 7)) %>% 
  mutate(day = substring(date, first = 9, last = 10)) %>% 
  mutate(hour = substring(date, first = 12, last = 13)) %>% 
  mutate(minute = substring(date, first = 15, last = 16)) %>% 
  mutate(second = substring(date, first = 18, last = 19)) 

# convert date components from characters to numbers
mini_survey_3$year <- as.numeric(mini_survey_3$year)
mini_survey_3$month <- as.numeric(mini_survey_3$month)
mini_survey_3$day <- as.numeric(mini_survey_3$day)
mini_survey_3$hour <- as.numeric(mini_survey_3$hour)
mini_survey_3$minute <- as.numeric(mini_survey_3$minute)
mini_survey_3$second <- as.numeric(mini_survey_3$second)

# convert date from character to date
mini_survey_3$date <- as.Date(mini_survey_3$date)

# remove timestamp and date fields as components are stored separately
mini_survey_3 <- mini_survey_3 %>% 
  select(-c(timestamp_raw, timestamp_formatted, date))

# factor ranks according to scale used in survey
mini_survey_3$rank_statement_13 <- factor(mini_survey_3$rank_statement_13, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_3$rank_statement_13)[1] <- "strongly disagree"
levels(mini_survey_3$rank_statement_13)[2] <- "disagree"
levels(mini_survey_3$rank_statement_13)[3] <- "neutral"
levels(mini_survey_3$rank_statement_13)[4] <- "agree"
levels(mini_survey_3$rank_statement_13)[5] <- "strongly agree"
summary(mini_survey_3$rank_statement_13)

mini_survey_3$rank_statement_14 <- factor(mini_survey_3$rank_statement_14, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_3$rank_statement_14)[1] <- "strongly disagree"
levels(mini_survey_3$rank_statement_14)[2] <- "disagree"
levels(mini_survey_3$rank_statement_14)[3] <- "neutral"
levels(mini_survey_3$rank_statement_14)[4] <- "agree"
levels(mini_survey_3$rank_statement_14)[5] <- "strongly agree"
summary(mini_survey_3$rank_statement_14)

mini_survey_3$rank_statement_15 <- factor(mini_survey_3$rank_statement_15, 
                                         levels = c(1,2,3,4,5))
levels(mini_survey_3$rank_statement_15)[1] <- "strongly disagree"
levels(mini_survey_3$rank_statement_15)[2] <- "disagree"
levels(mini_survey_3$rank_statement_15)[3] <- "neutral"
levels(mini_survey_3$rank_statement_15)[4] <- "agree"
levels(mini_survey_3$rank_statement_15)[5] <- "strongly agree"
summary(mini_survey_3$rank_statement_15)

mini_survey_3$rank_statement_16 <- factor(mini_survey_3$rank_statement_16, 
                                          levels = c(1,2,3,4,5))

levels(mini_survey_3$rank_statement_16)[1] <- "strongly disagree"
levels(mini_survey_3$rank_statement_16)[2] <- "disagree"
levels(mini_survey_3$rank_statement_16)[3] <- "neutral"
levels(mini_survey_3$rank_statement_16)[4] <- "agree"
levels(mini_survey_3$rank_statement_16)[5] <- "strongly agree"
summary(mini_survey_3$rank_statement_16)

mini_survey_3$rank_statement_17 <- factor(mini_survey_3$rank_statement_17, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_3$rank_statement_17)[1] <- "strongly disagree"
levels(mini_survey_3$rank_statement_17)[2] <- "disagree"
levels(mini_survey_3$rank_statement_17)[3] <- "neutral"
levels(mini_survey_3$rank_statement_17)[4] <- "agree"
levels(mini_survey_3$rank_statement_17)[5] <- "strongly agree"
summary(mini_survey_3$rank_statement_17)

mini_survey_3$rank_statement_18 <- factor(mini_survey_3$rank_statement_18, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_3$rank_statement_18)[1] <- "strongly disagree"
levels(mini_survey_3$rank_statement_18)[2] <- "disagree"
levels(mini_survey_3$rank_statement_18)[3] <- "neutral"
levels(mini_survey_3$rank_statement_18)[4] <- "agree"
levels(mini_survey_3$rank_statement_18)[5] <- "strongly agree"
summary(mini_survey_3$rank_statement_18)

# remove mturk_id column which contains personal identifiable information
mini_survey_3 <- mini_survey_3 %>% 
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

rank_13_bar <- ggplot(data = mini_survey_3,
                     mapping = aes(x = rank_statement_13)) +
  geom_bar() +
  labs(title = "Statement 13",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_13_bar

rank_14_bar <- ggplot(data = mini_survey_3,
                     mapping = aes(x = rank_statement_14)) +
  geom_bar() +
  labs(title = "Statement 14",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_14_bar 

rank_15_bar <- ggplot(data = mini_survey_3,
                     mapping = aes(x = rank_statement_15)) +
  geom_bar() +
  labs(title = "Statement 15",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_15_bar 

rank_16_bar <- ggplot(data = mini_survey_3,
                      mapping = aes(x = rank_statement_16)) +
  geom_bar() +
  labs(title = "Statement 16",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_16_bar

rank_17_bar <- ggplot(data = mini_survey_3,
                      mapping = aes(x = rank_statement_17)) +
  geom_bar() +
  labs(title = "Statement 17",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_17_bar

rank_18_bar <- ggplot(data = mini_survey_3,
                      mapping = aes(x = rank_statement_18)) +
  geom_bar() +
  labs(title = "Statement 18",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_18_bar

rank_13_to_18_plot <- rank_13_bar + 
  rank_14_bar + 
  rank_15_bar + 
  rank_16_bar + 
  rank_17_bar + 
  rank_18_bar
plot_layout()


ggsave(plot = rank_13_to_18_plot,
       filename = "figure_output/my_plot_statement_13_to_18.jpg")

# view summary of cleaned data 
str(mini_survey_3)
summary(mini_survey_3)

# write cleaned data to file
write_csv(x = mini_survey_3,
          file = "data_output/mini-survey-3-cleaned.csv")
