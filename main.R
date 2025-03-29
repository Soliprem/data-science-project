# The effects of participation in cash transfer programs on political support for the government
# Aydin, Salvi and Solidoro

# Loading the required libraries
library(tidyverse)
library(kableExtra)
library(GGally)
library(ggpubr)
library(grid)
library(rddensity)
library(modelsummary)

# Importing the data
load("data/gov_transfers.rda")

# Exploring the data
head(gov_transfers)

gov_transfers |>
  summary() |>
  kable("markdown") |>
  save_kable("artifacts/gov_transfexxrs.md")

gov_transfers |>
  ggpairs()

# Checking for fuzziness
gov_transfers |>
  group_by(Income_Centered < 0, Participation) |>
  count() |>
  kable("markdown") |>
  save_kable("artifacts/gov_transfers_fuzziness.md")

gov_transfers |>
  ggplot(aes(x = Income_Centered, y = Participation)) +
  geom_vline(xintercept = 0, color = "gray") +
  geom_point(color = "darkcyan", position = position_jitter(width = 0, height = 0.12)) +
  theme_light()

# Checking for manipulation
density <- gov_transfers$Income_Centered |>
  rddensity(c = 0)

density |>
  summary() |>
  capture.output() |>
  kable("markdown") |> 
  save_kable("artifacts/gov_transfers_rd_density.md")

density |>
  rdplotdensity(X = gov_transfers$Income_Centered, type = "both", xlabel = "Normalized Income", ylabel = "Density")

# Creting binomial Support and tibbles by different bandwiths
gov_transfers <- gov_transfers |> 
  mutate(Support_Binomial = case_when(Support == 1 ~ 1, T ~ 0))

quart_bw <- gov_transfers |>
  filter(Income_Centered > -0.005 & Income_Centered < 0.005)

half_bw <- gov_transfers |>
  filter(-0.01 < Income_Centered & Income_Centered < 0.01)

# RDD graphing by different specifications
#Full graphs
full_graph <- gov_transfers |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") +
  theme_light()

full_graph_q <- gov_transfers |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") +
  theme_light()

full_graph_c <- gov_transfers |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") +
  theme_light()

#half graphs
half_graph <- half_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") +
  theme_light()

half_graph_q <- half_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") +
  theme_light()

half_graph_c <- half_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") +
  theme_light()

#quart graphs
quart_graph <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", color = "navy", fill = "lightblue") +
  theme_light()

quart_graph_q <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "navy", fill = "lightblue") +
  theme_light()

quart_graph_c <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "navy", fill = "lightblue") +
  theme_light()

#plots <- ggarrange(full_graph + rremove("ylab") + rremove("xlab"), full_graph_q + rremove("ylab") + rremove("xlab"), full_graph_c + rremove("ylab") + rremove("xlab"), half_graph + rremove("ylab") + rremove("xlab"), half_graph_q + rremove("ylab") + rremove("xlab"), half_graph_c + rremove("ylab") + rremove("xlab"), quart_graph + rremove("ylab") + rremove("xlab"), quart_graph_q + rremove("ylab") + rremove("xlab"), quart_graph_c + rremove("ylab") + rremove("xlab"),
#                  ncol = 3, nrow = 3)
# 
#annotate_figure(plots, left = textGrob("Support", rot = 90, vjust = 1, gp = gpar(cex = 1.3)),
#                bottom = textGrob("Normalized Income", gp = gpar(cex = 1.3)))


# RDD calculation by different specifications

## Linear specifications

full_lm <- lm(Support ~ Participation + Income_Centered, data = gov_transfers)

half_lm <- lm(Support ~ Participation + Income_Centered, data = half_bw)

quart_lm <- lm(Support ~ Participation + Income_Centered, data = quart_bw)

## Quadratic specifications 
full_q <- lm(Support ~ Participation + Income_Centered + I(Income_Centered^2), data = gov_transfers)

half_q <- lm(Support ~ Participation + Income_Centered + I(Income_Centered^2), data = half_bw)

quart_q <- lm(Support ~ Participation + Income_Centered + I(Income_Centered^2), data = quart_bw)

## Cubic specifications
full_c <- lm(Support ~ Participation + Income_Centered + I(Income_Centered^2) + I(Income_Centered^3), data = gov_transfers)

half_c <- lm(Support ~ Participation + Income_Centered + I(Income_Centered^2) + I(Income_Centered^3), data = half_bw)

quart_c <- lm(Support ~ Participation + Income_Centered + I(Income_Centered^2) + I(Income_Centered^3), data = quart_bw)

## Slope interaction specifications 
full_slope <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered, data = gov_transfers)

half_slope <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered, data = half_bw)

quart_slope <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered, data = quart_bw)

## Interaction specifications controlling for age
full_age <- lm(Support ~ Participation + Income_Centered +
  Participation * Income_Centered + Age, data = gov_transfers)

half_age <- lm(Support ~ Participation + Income_Centered +
  Participation * Income_Centered + Age, data = half_bw)

quart_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, data = quart_bw)

## Logit specifications with slope interactions controlling for age
full_lgt <- glm(Support_Binomial ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = gov_transfers)

half_lgt <- glm(Support_Binomial ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = half_bw)

quart_lgt <- glm(Support_Binomial ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = quart_bw)

# Specification comparisons
# full_specs <- msummary(
#   list(full_lm, full_q, full_c, full_slope, full_age),
#   output = "markdown", stars = T
# )
# full_specs
# 
# half_specs <- msummary(
#   list(half_lm, half_q, half_c, half_slope, half_age),
#   output = "markdown", stars = T
# )
# half_specs
# 
# quart_specs <- msummary(
#   list(quart_lm, quart_q, quart_c, quart_slope, quart_age),
#   output = "markdown", stars = T
# )
# quart_specs
# 
# full_comparison  <- msummary(
#   list(full_lm, full_q, full_c),
#   output = "markdown", stars = T
# )
# full_comparison
# 
# half_comparison <- msummary(
#   list(half_lm, half_q, half_c),
#   output = "markdown", stars = T
# )
# half_comparison
# 
# quart_comparison  <- msummary(
#   list(quart_lm, quart_q, quart_c),
#   output = "markdown", stars = T
# )
# quart_comparison

overall_comparison <- msummary(
  list(
    "Full ^1" = full_lm, 
    "Full ^2" = full_q, 
    "Full ^3" = full_c,
    "Full Slope" = full_slope,
    "Full Age" = full_age,
    "Full Lgt" = full_lgt,
    "Half ^1" = half_lm, 
    "Half ^2" = half_q, 
    "Half ^3" = half_c, 
    "Half Slope" = half_slope,
    "Half Age" = half_age,
    "Half Lgt" = half_lgt,
    "Quart ^1" = quart_lm, 
    "Quart ^2" = quart_q, 
    "Quart ^3" = quart_c,
    "Quart Slope" = quart_slope,
    "Quart Age" = quart_age,
    "Quart Lgt" = quart_lgt
    ),
  output = "artifacts/all_comp.md", stars = T
)
#quart model summary & graph comparison
quart_comparison <- msummary(
  list(
    "Quart ^2" = quart_q, 
    "Quart Slope" = quart_slope,
    "Quart Age" = quart_age,
    "Quart Lgt" = quart_lgt
  ),
  output = "artifacts/quart_comp.md", stars = T
)
### WIP MODEL GRAPHS
quart_graph_slope <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", mapping = aes(y = predict(quart_slope, quart_bw)), color = "navy", fill = "lightblue") +
  theme_light()

quart_graph_age <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "lm", mapping = aes(y = predict(quart_age, quart_bw)), color = "navy", fill = "lightblue") +
  theme_light()

quart_graph_logit <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support_Binomial, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_smooth(method = "glm", mapping = aes(y = predict(quart_lgt, quart_bw)), color = "navy", fill = "lightblue") +
  theme_light()
