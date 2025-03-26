# The effects of participation in cash transfer programs on political support for the government
# Aydin, Salvi and Solidoro

# Loading the required libraries
library(tidyverse)
library(ggplot2)
library(kableExtra)
library(rddensity)
library(modelsummary)

# Importing the data
load("data/gov_transfers.rda")

# Making Support binomial (1 ~ current government favored to the previous one, 0 or 0.5 ~ current government not favored to the previous one)
gov_transfers <- gov_transfers |> 
  mutate(Support = case_when(Support == 1 ~ 1, T ~ 0))

# Creating tibbles by different bandwiths
quart_bw <- gov_transfers |>
  filter(Income_Centered > -0.005 & Income_Centered < 0.005)

half_bw <- gov_transfers |>
  filter(-0.01 < Income_Centered & Income_Centered < 0.01)

# RDD graphing by different bandwiths
full_graph <- gov_transfers |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "navy", fill = "lightblue") +
  theme_light() 
full_graph

half_graph <- half_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "glm", method.args = list(family = binomial), color = "navy", fill = "lightblue") +
  theme_light()
half_graph

quart_graph <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", method.args = list(family = binomial), color = "navy", fill = "lightblue") +
  theme_light()
quart_graph

# RDD calculation by different specifications

# Linear specifications

full_lm <- glm(Support ~ Participation + Income_Centered, family = binomial, data = gov_transfers)

half_lm <- glm(Support ~ Participation + Income_Centered, family = binomial, data = half_bw)

quart_lm <- glm(Support ~ Participation + Income_Centered, family = binomial, data = quart_bw)

# Slope varying specifications 
full_slope <- glm(Support ~ Participation + Income_Centered + Participation * Income_Centered, family = binomial, data = gov_transfers)

half_slope <- glm(Support ~ Participation + Income_Centered + Participation * Income_Centered, family = binomial, data = half_bw)

quart_slope <- glm(Support ~ Participation + Income_Centered + Participation * Income_Centered, family = binomial, data = quart_bw)

# Specifications controlling for age
full_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = gov_transfers)

half_age <- glm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = half_bw)

quart_age <- glm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = quart_bw)

# Specification comparisons
linear_specs <- msummary(list(full_lm, half_lm, quart_lm), output = "markdown", stars = T)
linear_specs

slope_specs <- msummary(list(full_slope, half_slope, quart_slope), output = "markdown", stars = T)
slope_specs

age_specs <- msummary(list(full_age, half_age, quart_age), output = "markdown", stars = T)
age_specs