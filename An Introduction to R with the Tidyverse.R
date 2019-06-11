# Load the Tidyverse ------------------------------------------------------
# install.packages("tidyverse")
library(tidyverse)

# R and Data Manipulation -------------------------------------------------
# Importing Data
store_data <- read_csv("store_data.csv")

getwd()

?read_csv

# Data Frames
store_data

# Filter Observations
store_data %>% 
  filter(gender == "Female")

store_data %>% 
  filter(store_spend > 100)

store_data %>% 
  filter(gender == "Female", store_spend > 100)

# Arrange Observations
store_data %>% 
  arrange(store_trans)

store_data %>% 
  arrange(desc(store_trans))

# Select Variables
store_data %>% 
  select(store_spend, age, gender)

# Mutate Variables
store_data %>% 
  mutate(store_spend = store_spend / 100)

# Join Data Frames
sat_data <- read_csv("sat_data.csv")

crm_data <- store_data %>% 
  left_join(sat_data, by = "id")

crm_data

# Exercise
online_data <- read_csv("online_data.csv")

store_data %>% 
  left_join(sat_data, by = "id") %>% 
  left_join(online_data, by = "id") %>% 
  filter(country == "US") %>% 
  select(id, gender, store_spend, online_spend) %>%
  mutate(total_spend = store_spend + online_spend) %>% 
  arrange(desc(total_spend))

# Data Description and Visualization --------------------------------------
# Describing Discrete Data
crm_data %>% 
  count(gender)

crm_data %>% 
  count(gender, country)

# Visualizing Discrete Data
ggplot(crm_data, aes(x = gender)) +
  geom_bar()

ggplot(crm_data, aes(x = gender, fill = country)) +
  geom_bar()

ggplot(crm_data, aes(x = gender, fill = country)) +
  geom_bar(position = "fill")

# Describing Continuous Data
crm_data %>% 
  summarize(avg_store_spend = mean(store_spend))

crm_data %>% 
  summarize(
    avg_store_spend = mean(store_spend),
    avg_sat_overall = mean(sat_overall)
  )

# Visualizing Continuous Data
ggplot(crm_data, aes(x = store_spend)) +
  geom_histogram()

ggplot(crm_data, aes(x = store_spend)) +
  geom_histogram(bins = 10)

crm_data %>% 
  ggplot(aes(x = store_spend, y = sat_overall)) +
  geom_point()

ggplot(crm_data, aes(x = log(store_spend + 1), y = sat_overall)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

# Describing Discrete and Continuous Data
crm_data %>% 
  group_by(gender, country) %>% 
  summarize(
    n = n(),
    avg_store_spend = mean(store_spend),
    avg_sat_overall = mean(sat_overall)
  ) %>% 
  arrange(desc(avg_store_spend))

crm_data %>% 
  group_by(gender) %>% 
  summarize(n = n())

# Visualizing Discrete and Continuous Data
crm_data %>%   
  ggplot(aes(x = gender, y = sat_overall)) +
  geom_boxplot()

crm_data %>%   
  ggplot(aes(x = sat_overall, fill = gender)) +
  geom_density(alpha = 0.5)

crm_data %>% 
  ggplot(
    aes(
      x = log(store_spend + 1), 
      y = sat_overall,
      color = gender
    )
  ) +
  geom_jitter(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ country) +
  ggtitle("Store Spend by Overall Satisfaction")

# Exercise
ca_crm_data <- store_data %>% 
  left_join(sat_data, by = "id") %>% 
  left_join(online_data, by = "id") %>% 
  filter(country == "CA") %>% 
  mutate(total_spend = store_spend + online_spend)

ca_crm_data %>% 
  count(gender)

ca_crm_data %>% 
  ggplot(aes(x = gender)) +
  geom_bar()

ca_crm_data %>% 
  ggplot(aes(x = credit_score, y = sat_overall)) +
  geom_point() +
  geom_smooth(method = "lm")

ca_crm_data %>% 
  group_by(gender) %>% 
  summarize(avg_total_spend = mean(total_spend))

ca_crm_data %>% 
  ggplot(
    aes(
      x = log(total_spend + 1), 
      y = sat_overall, 
      color = gender
    )
  ) +
  geom_point(show.legend = FALSE, alpha = 0.5) +
  geom_smooth(method = "lm")

# Cleaning Data and Summarization -----------------------------------------
# Gather Columns
online_data

online_data <- online_data %>% 
  gather(
    key = week,
    value = visits,
    week1_visit:week4_visit
  )

online_data

# Spread Columns
online_data %>% 
  spread(key = week, value = visits)

# Separate and Unite Columns
online_data <- online_data %>% 
  separate(year_mo, c("year", "month"))

online_data

online_data %>% 
  unite(year_mo, year, month)

# Wrangling <--> Summarization
online_data %>% 
  group_by(id) %>% 
  summarize(total_visits = sum(visits)) %>% 
  left_join(sat_data, by = "id") %>% 
  ggplot(aes(x = total_visits, y = sat_overall, color = country)) +
  geom_point(size = 2, alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE)

online_data %>% 
  group_by(id) %>% 
  summarize(total_visits = sum(visits)) %>% 
  left_join(sat_data, by = "id") %>% 
  ggplot(aes(x = total_visits, y = sat_overall)) +
  geom_point(size = 2, alpha = 0.5, aes(color = country)) + 
  geom_smooth(method = "lm", se = FALSE)

# Inspecting Data
online_data

faithful

str(faithful)
summary(faithful)
class(faithful)
dim(faithful)
names(faithful)
head(faithful)
tail(faithful)

as_tibble(faithful)

# Data Types and Coercion
online_data %>% 
  filter(visits > 700) %>% 
  ggplot(aes(x = id, y = visits)) +
  geom_col()

online_data %>% 
  filter(visits > 700) %>% 
  ggplot(aes(x = as.factor(id), y = visits)) +
  geom_col()

# Missing Values
online_data %>% 
  group_by(year) %>% 
  summarize(avg_visits = mean(visits))

online_data %>% 
  group_by(year) %>% 
  summarize(avg_visits = mean(visits, na.rm = TRUE))

online_data2 <- online_data %>% 
  filter(visits != is.na(visits))

unique(online_data2$visits)

# Exercise
crm_data <- online_data %>% 
  separate(week, c("week", "temp")) %>% 
  select(-temp) %>% 
  group_by(id) %>% 
  summarize(total_visits = sum(visits, na.rm = TRUE)) %>% 
  left_join(sat_data, by = "id") %>% 
  left_join(store_data, by = "id")

crm_data %>% 
  group_by(country, gender) %>% 
  summarize(avg_visit = mean(total_visits)) %>% 
  ggplot(aes(x = gender, y = avg_visit, fill = country)) +
  geom_col(position = "dodge")

crm_data %>% 
  ggplot(aes(x = total_visits, y = log(store_spend + 1))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

crm_data %>% 
  ggplot(
    aes(
      x = total_visits, 
      y = log(store_spend + 1), 
      color = country
    )
  ) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE)

