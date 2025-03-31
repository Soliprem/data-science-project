# The effects of participation in cash transfer programs on political support for the government
# Aydin, Salvi and Solidoro

# Loading the required libraries
library(tidyverse)
library(kableExtra)
library(GGally)
library(grid)
library(rddensity)
library(modelsummary)
library(ggpubr)

# Importing the data
load("data/gov_transfers.rda")

# Exploring the data
head(gov_transfers)

gov_transfers |>
  ggpairs()
ggsave("artifacts/ggpairs.png", width = 21, height = 14.8, unit = "cm")

# Checking the data
## Checking for fuzziness
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

## Checking for manipulation
density <- gov_transfers$Income_Centered |>
  rddensity(c = 0)

density |>
  summary() |>
  capture.output() |>
  kable("markdown") |> 
  save_kable("artifacts/gov_transfers_rd_density.md")

density |>
  rdplotdensity(X = gov_transfers$Income_Centered, type = "both", xlabel ="Income_Centered", ylabel = "Density")
ggsave("artifacts/rd_density_plot.png", width = 21, height = 14.8, unit = "cm")

# Creating a binomial Support variable and tibbles of different bandwiths
gov_transfers <- gov_transfers |> 
  mutate(Support_Binomial = case_when(Support == 1 ~ 1, T ~ 0))

quart_bw <- gov_transfers |>
  filter(Income_Centered > -0.005 & Income_Centered < 0.005)

half_bw <- gov_transfers |>
  filter(-0.01 < Income_Centered & Income_Centered < 0.01)

## Exploring the edited data
gov_transfers |>
  summary() |>
  kable("markdown") |>
  save_kable("artifacts/gov_transfers.md")

quart_bw |>
  summary() |>
  kable("markdown") |>
  save_kable("artifacts/quart_bw.md")

half_bw |>
  summary() |>
  kable("markdown") |>
  save_kable("artifacts/half_bw.md")

# RDD calculation by different specifications and bandwiths
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
full_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, data = gov_transfers)

half_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, data = half_bw)

quart_age <- lm(Support ~ Participation + Income_Centered + Participation * Income_Centered + Age, data = quart_bw)

## Logit specifications with slope interactions controlling for age
full_lgt <- glm(Support_Binomial ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = gov_transfers)

half_lgt <- glm(Support_Binomial ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = half_bw)

quart_lgt <- glm(Support_Binomial ~ Participation + Income_Centered + Participation * Income_Centered + Age, family = binomial, data = quart_bw)

# Specification comparisons
## Specifications in the paper's main body (selected from quarter bandwith)
paper_comp <- msummary(
  list(
    "Quadratic" = quart_q, 
    "Slope" = quart_slope,
    "Slope and Age" = quart_age,
    "Slope and Age, Logistic" = quart_lgt
  ),
  output = "artifacts/paper_comp.md", stars = T
)

## Specifications in Appendix A (full bandwith)
full_comp <- msummary(
  list(
    "Linear" = full_lm,
    "Quadratic" = full_q,
    "Cubic" = full_c,
    "Slope" = full_slope,
    "Slope and Age" = full_age,
    "Slope and Age, Logistic" = full_lgt
  ),
  output = "artifacts/full_comp.md", stars = T
)

## Specifications in Appendix B (half bandwith)
half_comp <- msummary(
  list(
    "Linear" = half_lm,
    "Quadratic" = half_q,
    "Cubic" = half_c,
    "Slope" = half_slope,
    "Slope and Age" = half_age,
    "Slope and Age, Logistic" = half_lgt
  ),
  output = "artifacts/half_comp.md", stars = T
)

## Specifications in Appendix C (others from quarter bandwith)
quart_comp_other <- msummary(
  list(
    "Linear" = half_lm,
    "Cubic" = half_c
  ),
  output = "artifacts/quart_comp_other.md", stars = T
)

# Graphing the specifications in the paper's main body
## Quadratic specification
pred_q <- predict(quart_q, se.fit = T)
graph_q <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_line(aes(y = pred_q$fit), color = "navy", size = 1) +
  geom_ribbon(aes(ymin = pred_q$fit - 1.96 * pred_q$se.fit, ymax = pred_q$fit + 1.96 * pred_q$se.fit), fill = "lightblue", alpha = 0.5) +
  ggtitle("Quadratic") +
  theme_light() +
  lapply(list("xlab", "ylab"), rremove)

## Slope specification
pred_slope <- predict(quart_slope, se.fit = T)
graph_slope <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_line(aes(y = pred_slope$fit), color = "navy", size = 1) +
  geom_ribbon(aes(ymin = pred_slope$fit - 1.96 * pred_slope$se.fit, ymax = pred_slope$fit + 1.96 * pred_slope$se.fit), fill = "lightblue", alpha = 0.5) +
  ggtitle("Slope") +
  theme_light() +
  lapply(list("xlab", "ylab"), rremove)

## Slope and age specification
pred_age <- predict(quart_age, mutate(quart_bw, Age = mean(Age)), se.fit = T)
graph_age <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_line(aes(y = pred_age$fit), color = "navy", size = 1) +
  geom_ribbon(aes(ymin = pred_age$fit - 1.96 * pred_age$se.fit, ymax = pred_age$fit + 1.96 * pred_age$se.fit), fill = "lightblue", alpha = 0.5) +
  ggtitle("Slope and Age") +
  theme_light() +
  lapply(list("xlab", "ylab"), rremove)

## Logistic specification
pred_lgt <- predict(quart_lgt, mutate(quart_bw, Age = mean(Age)), type = "response", se.fit = T)
graph_lgt <- quart_bw |>
  ggplot(aes(x = Income_Centered, y = Support_Binomial, group = Participation)) +
  geom_point(color = "darkcyan") +
  geom_vline(xintercept = 0, color = "gray") +
  geom_line(aes(y = pred_lgt$fit), color = "navy", size = 1) +
  geom_ribbon(aes(ymin = pred_lgt$fit - 1.96 * pred_lgt$se.fit, ymax = pred_lgt$fit + 1.96 * pred_lgt$se.fit), fill = "lightblue", alpha = 0.5) +
  ggtitle("Slope and Age, Logistic") +
  theme_light() +
  lapply(list("xlab", "ylab"), rremove)

## Merging the graphs
ggarrange(graph_q, graph_slope, graph_age, graph_lgt, ncol = 2, nrow = 2) |> 
  annotate_figure(left = textGrob("Support", rot = 90, vjust = 0.5), bottom = textGrob("Income_Centered"))

ggsave("artifacts/plots.png", width = 21, height = 14.8, unit = "cm")