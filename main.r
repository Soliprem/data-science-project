library(tidyverse)
library(readxl)

df <- read_excel("./data/Freedom_In_The_50_States_2023.xlsx") %>%
        select(-"Important notes for fiscal data (mouse over to see comment)")
glimpse(df)
