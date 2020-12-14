library(tidyverse)


# read in child mortality data
mortality <- read_csv(file = file.path("data", "mortality_rates_clean.csv"))

# check integrity
dim(mortality)
str(mortality)

# tidy format
mortality_tidy <- mortality %>%
  pivot_longer(2:ncol(.), names_to = "year", values_to = "mortality")

# plot all curves
ggplot(mortality_tidy, aes(x = year, y = mortality, group = iso, color = iso)) +
  geom_line() +
  scale_color_viridis_d() +
  theme(legend.position = "none")



# read context data: GDP per country
gdps <- read_csv(file = file.path("data", "gdps.csv"),
                 skip = 3)

gdps_tidy <- gdps %>%
  rename(iso = `Country Code`) %>%
  select(-ends_with("Name"), -ends_with("Code")) %>%
  pivot_longer(2:ncol(.), names_to = "year", values_to = "gdp")

# fuse tables
fused_data <- mortality_tidy %>%
  left_join(gdps_tidy, by = c("iso", "year")) %>%
  filter(!is.na(gdp), !is.na(mortality)) %>%
  filter(iso != "WLD")

# check for one country
# Afghanistan
fused_data %>%
  filter(iso == "AFG") %>%
  ggplot(aes(x = gdp, y = mortality)) +
  geom_path() +
  geom_point(aes(alpha = year), size = 3, stroke = 0) +
  theme(legend.position = "none")

# Venezuela
fused_data %>%
  filter(iso == "HTI") %>%
  ggplot(aes(x = gdp, y = mortality, label = year)) +
  geom_path() +
  geom_point(aes(alpha = year), size = 3, stroke = 0) +
  geom_label() +
  theme(legend.position = "none")

# all countries
fused_data %>%
  ggplot(aes(x = gdp, y = mortality, group = iso, color = iso)) +
  geom_path() +
  xlim(c(0, 10e8)) +
  theme(legend.position = "none")
