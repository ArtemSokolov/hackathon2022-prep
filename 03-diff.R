library(tidyverse)

## Current team assignments
X <- read_csv("data/assignments.csv", col_types=cols())
X %>% group_by(Assignment) %>% summarize(n())

## Current list of registrants
Y <- read_csv("data/tidy.csv", col_types=cols()) %>%
    rename(!!!g_sn) %>% mutate_at(g_ct, recode, !!!g_il) %>%
    filter( !(Name %in% X$Name) ) %>%
    select(Name, all_of(g_ct), -endosomes) %>%
    gather(Challenge, Interest, -Name) %>%
    filter(Interest == 3) %>% arrange(Name)
