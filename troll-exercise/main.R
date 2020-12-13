library(tidyverse)
library(lubridate)
library(ggthemes)

theme_set(theme_bw())

# data downloaded from https://github.com/fivethirtyeight/russian-troll-tweets
# assemble the file paths
generic_path <- file.path("data", "IRAhandle_tweets_")
file_paths <- paste(generic_path, 1:13, ".csv", sep = "")

# load the data
data <- lapply(X = file_paths, FUN = read_csv) %>%
  do.call(rbind, .)

# filter and parse date and time, add columns
data_filt <- data %>%
  mutate(publish_date = parse_date_time(publish_date, orders = "m/d/y H:M")) %>%
  mutate(harvested_date = parse_date_time(harvested_date, orders = "m/d/y H:M")) %>%
  mutate(publish_year_month = ym(paste0(year(publish_date), " ", month(publish_date)))) %>%
  mutate(publish_year_month_day = ymd(paste0(year(publish_date), " ", month(publish_date), " ", day(publish_date)))) %>%
  filter(year(publish_date) >= 2015 & year(publish_date) < 2018)

# tweets by month or day
data_per_month <- data_filt %>%
  group_by(publish_year_month) %>%
  summarise(n = n())

data_per_day <- data_filt %>%
  group_by(publish_year_month_day) %>%
  summarise(n = n())

# initial plot
data_per_day %>%
  ggplot(aes(x = publish_year_month_day, y = n)) +
  # geom_point() +
  geom_segment(aes(y = 0, xend = publish_year_month_day, yend = n)) +
  xlab("Days") +
  ylab("Tweets per day")

# what is the max peak
data_per_day %>%
  filter(n == max(n)) %>%
  .$publish_year_month_day

# wikileaks: October 7th
data_filt %>%
  filter(publish_year_month_day == "2016-10-07") %>%
  View()


# plot with account category
data_per_day_cat <- data_filt %>%
  group_by(publish_year_month_day, account_category) %>%
  summarise(n = n())

data_per_day_cat %>%
  filter(account_category %in% c("LeftTroll", "RightTroll")) %>%
  ggplot() +
  geom_segment(data = data_per_day,
               aes(x = publish_year_month_day, y = 0, xend = publish_year_month_day, yend = n),
               color = "gray80") +
  geom_segment(aes(x = publish_year_month_day, y = 0, xend = publish_year_month_day, yend = n, color = account_category),
               alpha = 0.7) +
  scale_color_manual(values = c("LeftTroll" = "steelblue", "RightTroll" = "firebrick")) +
  xlab("Days") +
  ylab("Tweets per day")
