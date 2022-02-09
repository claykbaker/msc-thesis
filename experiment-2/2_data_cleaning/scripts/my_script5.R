# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)

# read in raw data for mini survey 5
mini_survey_5 <- read_csv("data_input/mini-survey-5.csv", 
                          col_names = c("timestamp_raw", #assign new column names
                                        "consent",
                                        "captcha_code",
                                        "language_question",
                                        "rank_statement_25",
                                        "explanation_statement_25",
                                        "rank_statement_26",
                                        "explanation_statement_26",
                                        "rank_statement_27",
                                        "explanation_statement_27",
                                        "rank_statement_28",
                                        "explanation_statement_28",
                                        "rank_statement_29",
                                        "explanation_statement_29",
                                        "rank_statement_30",
                                        "explanation_statement_30",
                                        "mturk_ID_consent",
                                        "mturk_ID",
                                        "feedback"),
                          skip = 1, # ignore first row which contains old column values                        
                          na ="",  # assign NA (null) to empty cells
                          trim_ws = TRUE) # trim whitespace

# inspecting the raw data
str(mini_survey_5)
summary(mini_survey_5)

# arrange survey data by ascending order of MTurk IDs
mini_survey_5 <- mini_survey_5 %>% 
  arrange(mturk_ID)

# remove rows which contain duplicate MTurk IDs (only 30 unique participants per survey)
mini_survey_5 <- mini_survey_5[!duplicated(mini_survey_5$mturk_ID), ]

# remove rows where consent is not given
mini_survey_5 <- mini_survey_5 %>% 
  filter(!consent == "No")

# create columns for date components of timestamp into separate columns: y, m, d, h,  m, s
mini_survey_5 <- mini_survey_5 %>% 
  mutate(timestamp_formatted = timestamp(mini_survey_5$timestamp_raw)) %>% 
  mutate(date = ymd_hms(timestamp_formatted, tz = "EET")) %>% 
  mutate(year = substring(date, first = 1, last = 4)) %>% 
  mutate(month = substring(date, first = 6, last = 7)) %>% 
  mutate(day = substring(date, first = 9, last = 10)) %>% 
  mutate(hour = substring(date, first = 12, last = 13)) %>% 
  mutate(minute = substring(date, first = 15, last = 16)) %>% 
  mutate(second = substring(date, first = 18, last = 19)) 

# convert date components from characters to numbers
mini_survey_5$year <- as.numeric(mini_survey_5$year)
mini_survey_5$month <- as.numeric(mini_survey_5$month)
mini_survey_5$day <- as.numeric(mini_survey_5$day)
mini_survey_5$hour <- as.numeric(mini_survey_5$hour)
mini_survey_5$minute <- as.numeric(mini_survey_5$minute)
mini_survey_5$second <- as.numeric(mini_survey_5$second)

# convert date from character to date
mini_survey_5$date <- as.Date(mini_survey_5$date)

# remove timestamp and date fields as components are stored separately
mini_survey_5 <- mini_survey_5 %>% 
  select(-c(timestamp_raw, timestamp_formatted, date))

# factor ranks according to scale used in survey
mini_survey_5$rank_statement_25 <- factor(mini_survey_5$rank_statement_25, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_5$rank_statement_25)[1] <- "strongly disagree"
levels(mini_survey_5$rank_statement_25)[2] <- "disagree"
levels(mini_survey_5$rank_statement_25)[3] <- "neutral"
levels(mini_survey_5$rank_statement_25)[4] <- "agree"
levels(mini_survey_5$rank_statement_25)[5] <- "strongly agree"
summary(mini_survey_5$rank_statement_25)

mini_survey_5$rank_statement_26 <- factor(mini_survey_5$rank_statement_26, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_5$rank_statement_26)[1] <- "strongly disagree"
levels(mini_survey_5$rank_statement_26)[2] <- "disagree"
levels(mini_survey_5$rank_statement_26)[3] <- "neutral"
levels(mini_survey_5$rank_statement_26)[4] <- "agree"
levels(mini_survey_5$rank_statement_26)[5] <- "strongly agree"
summary(mini_survey_5$rank_statement_26)

mini_survey_5$rank_statement_27 <- factor(mini_survey_5$rank_statement_27, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_5$rank_statement_27)[1] <- "strongly disagree"
levels(mini_survey_5$rank_statement_27)[2] <- "disagree"
levels(mini_survey_5$rank_statement_27)[3] <- "neutral"
levels(mini_survey_5$rank_statement_27)[4] <- "agree"
levels(mini_survey_5$rank_statement_27)[5] <- "strongly agree"
summary(mini_survey_5$rank_statement_27)

mini_survey_5$rank_statement_28 <- factor(mini_survey_5$rank_statement_28, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_5$rank_statement_28)[1] <- "strongly disagree"
levels(mini_survey_5$rank_statement_28)[2] <- "disagree"
levels(mini_survey_5$rank_statement_28)[3] <- "neutral"
levels(mini_survey_5$rank_statement_28)[4] <- "agree"
levels(mini_survey_5$rank_statement_28)[5] <- "strongly agree"
summary(mini_survey_5$rank_statement_28)

mini_survey_5$rank_statement_29 <- factor(mini_survey_5$rank_statement_29, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_5$rank_statement_29)[1] <- "strongly disagree"
levels(mini_survey_5$rank_statement_29)[2] <- "disagree"
levels(mini_survey_5$rank_statement_29)[3] <- "neutral"
levels(mini_survey_5$rank_statement_29)[4] <- "agree"
levels(mini_survey_5$rank_statement_29)[5] <- "strongly agree"
summary(mini_survey_5$rank_statement_29)

mini_survey_5$rank_statement_30 <- factor(mini_survey_5$rank_statement_30, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_5$rank_statement_30)[1] <- "strongly disagree"
levels(mini_survey_5$rank_statement_30)[2] <- "disagree"
levels(mini_survey_5$rank_statement_30)[3] <- "neutral"
levels(mini_survey_5$rank_statement_30)[4] <- "agree"
levels(mini_survey_5$rank_statement_30)[5] <- "strongly agree"
summary(mini_survey_5$rank_statement_30)

# remove mturk_id column which contains personal identifiable information
mini_survey_5 <- mini_survey_5 %>% 
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

rank_25_bar <- ggplot(data = mini_survey_5,
                      mapping = aes(x = rank_statement_25)) +
  geom_bar() +
  labs(title = "Statement 25",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_25_bar

rank_26_bar <- ggplot(data = mini_survey_5,
                      mapping = aes(x = rank_statement_26)) +
  geom_bar() +
  labs(title = "Statement 26",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_26_bar

rank_27_bar <- ggplot(data = mini_survey_5,
                      mapping = aes(x = rank_statement_27)) +
  geom_bar() +
  labs(title = "Statement 27",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_27_bar 

rank_28_bar <- ggplot(data = mini_survey_5,
                      mapping = aes(x = rank_statement_28)) +
  geom_bar() +
  labs(title = "Statement 28",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_28_bar

rank_29_bar <- ggplot(data = mini_survey_5,
                      mapping = aes(x = rank_statement_29)) +
  geom_bar() +
  labs(title = "Statement 29",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_29_bar

rank_30_bar <- ggplot(data = mini_survey_5,
                      mapping = aes(x = rank_statement_30)) +
  geom_bar() +
  labs(title = "Statement 30",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_30_bar

rank_25_to_30_plot <- rank_25_bar + 
  rank_26_bar + 
  rank_27_bar + 
  rank_28_bar + 
  rank_29_bar + 
  rank_30_bar

plot_layout()

ggsave(plot = rank_25_to_30_plot,
       filename = "figure_output/my_plot_statement_25_to_30.jpg")

# view summary of cleaned data 
str(mini_survey_5)
summary(mini_survey_5)

# write cleaned data to file
write_csv(x = mini_survey_5,
          file = "data_output/mini-survey-5-cleaned.csv")
