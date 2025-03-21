#The effects of participation in cash transfer programs on political support for the government
#Aydin, Salvi and Solidoro

#Loading the required libraries
library(tidyverse)
library(kableExtra)
library(GGally)
library(gridExtra)
library(rddensity)
library(modelsummary)

#Importing the data
load("data/gov_transfers.rda")
head(gov_transfers)

#Exploring the data
gov_transfers |> 
  summary() |> 
  kable()

gov_transfers |> 
  ggpairs()

#Checking for fuzziness
gov_transfers |> 
  group_by(Income_Centered < 0, Participation) |> 
  count() |> 
  kable()

gov_transfers |> 
  ggplot(aes(x = Income_Centered, y = Participation)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(color = "darkcyan", position = position_jitter(width = 0, height = 0.12)) +
  theme_light()

#Checking for manipulation
density <- gov_transfers$Income_Centered |> 
  rddensity(c = 0)
density |> 
  summary()

density_plot <- density |> 
  rdplotdensity(X = gov_transfers$Income_Centered, type = "both")
density_plot

#Creating tibbles by different bandwiths
quart_bw <- gov_transfers |> 
  filter(Income_Centered > -0.005 & Income_Centered < 0.005)

half_bw <- gov_transfers |> 
  filter(-0.01 < Income_Centered & Income_Centered < 0.01)

#RDD graphing by different specifications
full_graph <- gov_transfers |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") + 
  theme_light()
full_graph

full_graph_q <- gov_transfers |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") + 
  theme_light()
full_graph_q

full_graph_c <- gov_transfers |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") + 
  theme_light()
full_graph_c

half_graph <- half_bw |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") + 
  theme_light()
half_graph

half_graph_q <- half_bw |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") + 
  theme_light()
half_graph_q

half_graph_c <- half_bw |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") + 
  theme_light()
half_graph_c

quart_graph <- quart_bw |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") + 
  theme_light()
quart_graph

quart_graph_q <- quart_bw |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") + 
  theme_light()
quart_graph_q

quart_graph_c <- quart_bw |> 
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") + 
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") + 
  theme_light()
quart_graph_c

grid.arrange(grobs = list(full_graph, full_graph_q, full_graph_c, half_graph, half_graph_q, half_graph_c, quart_graph, quart_graph_q, quart_graph_c), ncol = 3)

#RDD calculation by different specifications
full_lm <- lm(Support ~ Participation + Income_Centered, data = gov_transfers)
full_lm |>
  summary()

half_lm <- lm(Support ~ Participation + Income_Centered, data = half_bw)
half_lm |> 
  summary()

quart_lm <- lm(Support ~ Participation + Income_Centered, data = quart_bw)
quart_lm |> 
  summary()

full_q <- lm(Support ~ Participation + poly(Income_Centered, degree = 2), data = gov_transfers)
full_q |> 
  summary()

half_q <- lm(Support ~ Participation + poly(Income_Centered, degree = 2), data = half_bw)
half_q |> 
  summary()

quart_q <- lm(Support ~ Participation + poly(Income_Centered, degree = 2), data = quart_bw)
quart_q |> 
  summary()

full_c<- lm(Support ~ Participation + poly(Income_Centered, degree = 3), data = gov_transfers)
full_c |> 
  summary()

half_c <- lm(Support ~ Participation + poly(Income_Centered, degree = 3), data = half_bw)
half_c |> 
  summary()

quart_c <- lm(Support ~ Participation + poly(Income_Centered, degree = 3), data = quart_bw)
quart_c |> 
  summary()

full_slope <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered, data = gov_transfers)
full_slope |>
  summary()

half_slope <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered, data = half_bw)
half_slope |> 
  summary()

quart_slope <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered, data = quart_bw)
quart_slope |> 
  summary()

full_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, data = gov_transfers)
full_age |> 
  summary()

half_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, data = gov_transfers)
half_age |> 
  summary()

quart_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, data = half_bw)
quart_age |> 
  summary()

full_specs <- modelsummary(
  list(full_lm, full_q, full_c, full_slope, full_age),
  stars = T)
full_specs

half_specs <- modelsummary(
  list(half_lm, half_q, half_c, half_slope, half_age),
  stars = T)
half_specs

quart_specs <- modelsummary(
  list(quart_lm, quart_q, quart_c, quart_slope, quart_age),
  stars = T)
quart_specs