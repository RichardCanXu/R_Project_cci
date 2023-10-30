# This R script is to analyze and compare different
# variables from the 2021 Quality of Government data set
# to determine how the amount of conflict in a country
# will impact other aspects of its society
# Richard Xu – 8/4/2023

# Load tidyverse, stargazer

library(tidyverse)
library(stargazer)

# Read data
setwd("C:/Users/haoxing/Desktop/poli5/Quality of Government")
library(tidyverse)
qog <- readRDS("qog_data.rds")

qog_data <- qog %>% select(bti_ci, bci_bci, bti_mes, wdi_nerp)

my_table <- as.data.frame(qog_data)

stargazer(my_table, type = "html", covariate.labels = c("Conflict Intensity", "Corruption Index", "Economy Status", "Primary School Enrollment"),out = "qogdata_descriptive.html")

# Corruption Index vs Military Expenditure (% of GDP)

# Summary of Corruption Index and Military Expenditure

summary(qog$bci_bci)
summary(qog$wdi_expmil)

# Histogram of Corruption Index
qog %>% 
  ggplot(aes(x = bci_bci)) +
  geom_histogram(breaks = seq(0, 75, 5),
                 color = "black",
                 fill = "gray") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Corruption Index Around The World",
       x = "Corruption Index",
       y = "Frequency") +
  theme_bw()

# Histogram of Economy Status
qog %>% 
  ggplot(aes(x = bti_mes)) +
  geom_histogram(breaks = seq(0, 10, 1),
                 color = "black",
                 fill = "gray") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(title = "Economy Status Around The World",
       x = "Economy Status",
       y = "Frequency") +
  theme_bw()

# Histogram of Primary School Enrollment
qog %>% 
  ggplot(aes(x = wdi_nerp)) +
  geom_histogram(breaks = seq(0, 100, 10),
                 color = "black",
                 fill = "gray") +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  labs(title = "Primary School Enrollment (% net) Around The World",
       x = "Primary School Enrollment",
       y = "Frequency") +
  theme_bw()


# Histogram of Conflict Intensity
qog %>% 
  ggplot(aes(x = bti_ci)) +
  geom_histogram(breaks = seq(0, 10, 1),
                 color = "black",
                 fill = "gray") +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  labs(title = "Conflict Intensity Around The World",
       x = "Conflict Intensity",
       y = "Frequency") +
  theme_bw()

# Scatter plot of Conflict Intensity and Corruption
qog %>% 
  ggplot(aes(x = bti_ci, y = bci_bci)) +
  geom_point() +
  labs(
    title = "Conflict Intensity and Corruption Index",
    y = "Corruption Index",
    x = "Conflict Intensity",
    caption = "Source: Quality of Government Dataset"
  ) +
  geom_smooth(method = lm, se =FALSE) +
  theme_bw()

# Scatter plot of Conflict Intensity and Economy Status
qog %>% 
  ggplot(aes(x = bti_ci, y = bti_mes)) +
  geom_point() +
  labs(
    title = "Conflict Intensity and Economy Status",
    y = "Economy Status",
    x = "Conflict Intensity",
    caption = "Source: Quality of Government Dataset"
  ) +
  geom_smooth(method = lm, se =FALSE) +
  theme_bw()

# Scatter plot of Conflict Intensity and Primary School enrollment
qog %>% 
  ggplot(aes(x = bti_ci, y = wdi_nerp)) +
  geom_point() +
  labs(
    title = "Conflict Intensity and Primary School Enrollment",
    y = "Primary School Enrollment (% net)",
    x = "Conflict Intensity",
    caption = "Source: Quality of Government Dataset"
  ) +
  geom_smooth(method = lm, se =FALSE) +
  theme_bw()

# Regression
m1 <- lm(bci_bci ~ bti_ci, data = qog)
m2 <- lm(bti_mes ~ bti_ci, data = qog)
m3 <- lm(wdi_nerp ~ bti_ci, data = qog)
summary(m1)

# Regression table

# Make regression table:
# Enter regression objects
stargazer(m1, m2, m3, 
          # Specify format
          type = "html", 
          # Label independent variables
          covariate.labels = c("Conflict Intensity"),
          # Label dependent variable
          column.labels   = c("Corruption Index", "Economy Status", "Primary School Enrollment (% net)"),
          # Saves table as .html file (open file and copy-paste into Word doc)
          out = "conflict_models.html")
