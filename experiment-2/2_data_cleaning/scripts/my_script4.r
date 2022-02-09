# load packages 
library(tidyverse)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(patchwork)
library(ggthemes)

# read in raw data for mini survey 4
mini_survey_4 <- read_csv("data_input/mini-survey-4.csv", 
                          col_names = c("timestamp_raw", #assign new column names
                                        "consent",
                                        "captcha_code",
                                        "language_question",
                                        "rank_statement_19",
                                        "explanation_statement_19",
                                        "rank_statement_20",
                                        "explanation_statement_20",
                                        "rank_statement_21",
                                        "explanation_statement_21",
                                        "rank_statement_22",
                                        "explanation_statement_22",
                                        "rank_statement_23",
                                        "explanation_statement_13",
                                        "rank_statement_24",
                                        "explanation_statement_24",
                                        "mturk_ID_consent",
                                        "mturk_ID",
                                        "feedback"),
                          skip = 1, # ignore first row which contains old column values                        
                          na ="",  # assign NA (null) to empty cells
                          trim_ws = TRUE) # trim whitespace

# inspecting the raw data
str(mini_survey_4)
summary(mini_survey_4)

# arrange survey data by ascending order of MTurk IDs
mini_survey_4 <- mini_survey_4 %>% 
  arrange(mturk_ID)

# remove rows which contain duplicate MTurk IDs (only 30 unique participants per survey)
mini_survey_4 <- mini_survey_4[!duplicated(mini_survey_4$mturk_ID), ]

# remove rows where consent is not given
mini_survey_4 <- mini_survey_4 %>% 
  filter(!consent == "No")

# create columns for date components of timestamp into separate columns: y, m, d, h,  m, s
mini_survey_4 <- mini_survey_4 %>% 
  mutate(timestamp_formatted = timestamp(mini_survey_4$timestamp_raw)) %>% 
  mutate(date = ymd_hms(timestamp_formatted, tz = "EET")) %>% 
  mutate(year = substring(date, first = 1, last = 4)) %>% 
  mutate(month = substring(date, first = 6, last = 7)) %>% 
  mutate(day = substring(date, first = 9, last = 10)) %>% 
  mutate(hour = substring(date, first = 12, last = 13)) %>% 
  mutate(minute = substring(date, first = 15, last = 16)) %>% 
  mutate(second = substring(date, first = 18, last = 19)) 

# convert date components from characters to numbers
mini_survey_4$year <- as.numeric(mini_survey_4$year)
mini_survey_4$month <- as.numeric(mini_survey_4$month)
mini_survey_4$day <- as.numeric(mini_survey_4$day)
mini_survey_4$hour <- as.numeric(mini_survey_4$hour)
mini_survey_4$minute <- as.numeric(mini_survey_4$minute)
mini_survey_4$second <- as.numeric(mini_survey_4$second)

# convert date from character to date
mini_survey_4$date <- as.Date(mini_survey_4$date)

# remove timestamp and date fields as components are stored separately
mini_survey_4 <- mini_survey_4 %>% 
  select(-c(timestamp_raw, timestamp_formatted, date))

# factor ranks according to scale used in survey
mini_survey_4$rank_statement_19 <- factor(mini_survey_4$rank_statement_19, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_4$rank_statement_19)[1] <- "strongly disagree"
levels(mini_survey_4$rank_statement_19)[2] <- "disagree"
levels(mini_survey_4$rank_statement_19)[3] <- "neutral"
levels(mini_survey_4$rank_statement_19)[4] <- "agree"
levels(mini_survey_4$rank_statement_19)[5] <- "strongly agree"
summary(mini_survey_4$rank_statement_19)

mini_survey_4$rank_statement_20 <- factor(mini_survey_4$rank_statement_20, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_4$rank_statement_20)[1] <- "strongly disagree"
levels(mini_survey_4$rank_statement_20)[2] <- "disagree"
levels(mini_survey_4$rank_statement_20)[3] <- "neutral"
levels(mini_survey_4$rank_statement_20)[4] <- "agree"
levels(mini_survey_4$rank_statement_20)[5] <- "strongly agree"
summary(mini_survey_4$rank_statement_20)

mini_survey_4$rank_statement_21 <- factor(mini_survey_4$rank_statement_21, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_4$rank_statement_21)[1] <- "strongly disagree"
levels(mini_survey_4$rank_statement_21)[2] <- "disagree"
levels(mini_survey_4$rank_statement_21)[3] <- "neutral"
levels(mini_survey_4$rank_statement_21)[4] <- "agree"
levels(mini_survey_4$rank_statement_21)[5] <- "strongly agree"
summary(mini_survey_4$rank_statement_21)

mini_survey_4$rank_statement_22 <- factor(mini_survey_4$rank_statement_22, 
                                          levels = c(1,2,3,4,5))

levels(mini_survey_4$rank_statement_22)[1] <- "strongly disagree"
levels(mini_survey_4$rank_statement_22)[2] <- "disagree"
levels(mini_survey_4$rank_statement_22)[3] <- "neutral"
levels(mini_survey_4$rank_statement_22)[4] <- "agree"
levels(mini_survey_4$rank_statement_22)[5] <- "strongly agree"
summary(mini_survey_4$rank_statement_22)

mini_survey_4$rank_statement_23 <- factor(mini_survey_4$rank_statement_23, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_4$rank_statement_23)[1] <- "strongly disagree"
levels(mini_survey_4$rank_statement_23)[2] <- "disagree"
levels(mini_survey_4$rank_statement_23)[3] <- "neutral"
levels(mini_survey_4$rank_statement_23)[4] <- "agree"
levels(mini_survey_4$rank_statement_23)[5] <- "strongly agree"
summary(mini_survey_4$rank_statement_23)

mini_survey_4$rank_statement_24 <- factor(mini_survey_4$rank_statement_24, 
                                          levels = c(1,2,3,4,5))
levels(mini_survey_4$rank_statement_24)[1] <- "strongly disagree"
levels(mini_survey_4$rank_statement_24)[2] <- "disagree"
levels(mini_survey_4$rank_statement_24)[3] <- "neutral"
levels(mini_survey_4$rank_statement_24)[4] <- "agree"
levels(mini_survey_4$rank_statement_24)[5] <- "strongly agree"
summary(mini_survey_4$rank_statement_24)

# remove mturk_id column which contains personal identifiable information
mini_survey_4 <- mini_survey_4 %>% 
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

rank_19_bar <- ggplot(data = mini_survey_4,
                      mapping = aes(x = rank_statement_19)) +
  geom_bar() +
  labs(title = "Statement 19",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_19_bar

rank_20_bar <- ggplot(data = mini_survey_4,
                      mapping = aes(x = rank_statement_20)) +
  geom_bar() +
  labs(title = "Statement 20",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_20_bar

rank_21_bar <- ggplot(data = mini_survey_4,
                      mapping = aes(x = rank_statement_21)) +
  geom_bar() +
  labs(title = "Statement 21",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_21_bar 

rank_22_bar <- ggplot(data = mini_survey_4,
                      mapping = aes(x = rank_statement_22)) +
  geom_bar() +
  labs(title = "Statement 22",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_22_bar

rank_23_bar <- ggplot(data = mini_survey_4,
                      mapping = aes(x = rank_statement_23)) +
  geom_bar() +
  labs(title = "Statement 23",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_23_bar

rank_24_bar <- ggplot(data = mini_survey_4,
                      mapping = aes(x = rank_statement_24)) +
  geom_bar() +
  labs(title = "Statement 24",
       x = "Rank",
       y = "No. of participants") +
  ylim(0,30) +
  grey_theme

rank_24_bar

rank_19_to_24_plot <- rank_19_bar + 
  rank_20_bar + 
  rank_21_bar + 
  rank_22_bar + 
  rank_23_bar + 
  rank_24_bar

plot_layout()

ggsave(plot = rank_19_to_24_plot,
       filename = "figure_output/my_plot_statement_19_to_24.jpg")

# view summary of cleaned data 
str(mini_survey_4)
summary(mini_survey_4)

# write cleaned data to file
write_csv(x = mini_survey_4,
          file = "data_output/mini-survey-4-cleaned.csv")