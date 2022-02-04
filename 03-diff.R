library(tidyverse)

## Intereset level (il)
il <- c("Very interested"     = 3,
        "Somewhat interested" = 2,
        "Not interested"      = 1)

## Challenge tags (ct)
ct <- c("01-artifacts",
        "02-segmentation",
        "03-virtual-IF",
        "04-VAE",
        "05-cross-talk",
        "06-viz-comp",
        "07-TB-scale",
        "08-thumbnails",
        "09-galaxy",
        "10-neuroglancer",
        "endosomes",		## Endosomes challenge is cancelled
        "11-cosmetic")

## Shorthand notations (sn) for column names
sn <- set_names(10:21, ct) %>%
    c(Email       = 2,
      Institution = 4,
      Stage       = 5,
      Network     = 6,
      Experience  = 7,
      Languages   = 8,
      GPU         = 9)

## Exclude champions and folks who decided not to participate
excl <- scan("data/exclude.txt", what=character())

## Current team assignments
X <- read_csv("data/current.csv", col_types=cols())
X %>% group_by(Assignment) %>% summarize(n())

## Current list of registrants
Y <- read_csv("data/updated.csv", col_types=cols()) %>%
    rename(!!!sn) %>% filter( !(Email %in% excl) ) %>%
    filter( !(Name %in% X$Name) ) %>%
    mutate_at(ct, recode, !!!il) %>%
    select(Name, all_of(ct), -endosomes) %>%
    gather(Challenge, Interest, -Name) %>%
    filter(Interest == 3)
    
Y %>% group_by(Challenge) %>% summarize(n())
