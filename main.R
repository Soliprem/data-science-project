#The effects of participation in cash transfer programs on political support for the government
#Aydin, Salvi and Solidoro

#Loading the required libraries
library(tidyverse)
library(kableExtra)
library(GGally)
library(ggpubr)
library(rddensity)
library(modelsummary)

#Importing and editing the data
load("data/gov_transfers.rda")

gov_transfers <- gov_transfers |> 
  rename(Normalized_Income = Normalized_Income)

#Exploring the data
head(gov_transfers)

# Exploring the data
gov_transfers |>
  summary() |>
  kable("markdown") |>
  save_kable("artifacts/gov_transfers.md")

gov_transfers |> 
  ggpairs()

# Checking for fuzziness
gov_transfers |>
  group_by(Normalized_Income < 0, Participation) |>
  count() |>
  kable("markdown") |>
  save_kable("artifacts/gov_transfers_fuzziness.md")

gov_transfers |> 
  ggplot(aes(x = Normalized_Income, y = Participation)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(color = "darkcyan", position = position_jitter(width = 0, height = 0.12)) +
  theme_light()

#Checking for manipulation
density <- gov_transfers$Normalized_Income |> 
  rddensity(c = 0)

density |> 
  summary() |> 
  capture.output() |> 
  kable()

density |>
  rdplotdensity(X = gov_transfers$Normalized_Income, type = "both", xlabel = "Normalized Income", ylabel = "Density")

#Creating tibbles by different bandwiths
quart_bw <- gov_transfers |> 
  filter(Normalized_Income > -0.005 & Normalized_Income < 0.005)

half_bw <- gov_transfers |> 
  filter(-0.01 < Normalized_Income & Normalized_Income < 0.01)

#RDD graphing by different specifications
full_graph <- gov_transfers |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") + 
  theme_light()

full_graph_q <- gov_transfers |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") + 
  theme_light()

full_graph_c <- gov_transfers |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") + 
  theme_light()

half_graph <- half_bw |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") + 
  theme_light()

half_graph_q <- half_bw |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") + 
  theme_light()

half_graph_c <- half_bw |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") + 
  theme_light()

quart_graph <- quart_bw |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") + 
  theme_light()

quart_graph_q <- quart_bw |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") + 
  theme_light()

quart_graph_c <- quart_bw |> 
  ggplot(aes(x = Normalized_Income, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") + 
  labs(xlab = "", ylab = "") +
  theme_light()

ggarrange(plotlist = list(full_graph, full_graph_q, full_graph_c, half_graph, half_graph_q, half_graph_c, quart_graph, quart_graph_q, quart_graph_c), ncol = 3, nrow = 3)

#RDD calculation by different specifications
full_lm <- lm(Support ~ Participation + Normalized_Income, data = gov_transfers)

half_lm <- lm(Support ~ Participation + Normalized_Income, data = half_bw)

quart_lm <- lm(Support ~ Participation + Normalized_Income, data = quart_bw)

full_q <- lm(Support ~ Participation + poly(Normalized_Income, degree = 2), data = gov_transfers)

half_q <- lm(Support ~ Participation + poly(Normalized_Income, degree = 2), data = half_bw)

quart_q <- lm(Support ~ Participation + poly(Normalized_Income, degree = 2), data = quart_bw)

full_c<- lm(Support ~ Participation + poly(Normalized_Income, degree = 3), data = gov_transfers)

half_c <- lm(Support ~ Participation + poly(Normalized_Income, degree = 3), data = half_bw)

quart_c <- lm(Support ~ Participation + poly(Normalized_Income, degree = 3), data = quart_bw)

full_slope <- lm(Support ~ Participation + Normalized_Income + Participation * Normalized_Income, data = gov_transfers)

half_slope <- lm(Support ~ Participation + Normalized_Income + Participation * Normalized_Income, data = half_bw)

quart_slope <- lm(Support ~ Participation + Normalized_Income + Participation * Normalized_Income, data = quart_bw)

full_age <- lm(Support ~ Participation + Normalized_Income + Participation * Normalized_Income + Age, data = gov_transfers)

half_age <- lm(Support ~ Participation + Normalized_Income + Participation * Normalized_Income + Age, data = gov_transfers)

quart_age <- lm(Support ~ Participation + Normalized_Income + Participation * Normalized_Income + Age, data = half_bw)

full_specs <- msummary(
  list(full_lm, full_q, full_c, full_slope, full_age), output = "markdown", stars = T)
full_specs

half_specs <- msummary(
  list(half_lm, half_q, half_c, half_slope, half_age), output = "markdown", stars = T)
half_specs

quart_specs <- msummary(
  list(quart_lm, quart_q, quart_c, quart_slope, quart_age), output = "markdown", stars = T)
quart_specs
