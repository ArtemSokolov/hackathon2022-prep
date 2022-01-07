library(tidyverse)
library(seriation)

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
        "11-endosomes",
        "12-cosmetic")

## Shorthand notations (sn) for column names
sn <- set_names(10:21, ct) %>%
    c(Email       = 2,
      Institution = 4,
      Stage       = 5,
      Network     = 6,
      Experience  = 7,
      Languages   = 8,
      GPU         = 9)

## Load and clean up all registrant information
X <- read_csv('data/registration.csv', col_types=cols()) %>%
    rename(!!!sn) %>%
    mutate_at(ct, replace_na,  "Not interested")
stopifnot(anyNA(X)==FALSE)
write_csv(X, "data/reg-clean.csv")

## Compute the distance matrix (DM) of challenge interest
DM <- X %>% select(Name, all_of(ct)) %>%
    mutate_at(ct, recode, !!!il) %>% as.data.frame() %>%
    column_to_rownames("Name") %>% dist

## Compute optimal leaf ordering (olo)
olo <- hclust(DM) %>% reorder(DM) %>%
    dendextend::order.hclust() %>% labels(DM)[.]

## Overall view of registrant interest in the challenges
Z <- X %>% select(Name, all_of(ct)) %>%
    pivot_longer(-Name, names_to="Challenge", values_to="Interest") %>%
    mutate(Name      = factor(Name, levels=olo),
           Challenge = factor(Challenge, rev(ct)))

## Plot elements to remove (petr)
petr <- c("axis.text.x",
          "axis.ticks.x",
          "panel.grid.major",
          "panel.grid.minor") %>%
    set_names() %>% map(~element_blank())

## Overview plot
gg1 <- ggplot(Z, aes(x=Name, y=Challenge, fill=Interest)) +
    geom_tile() + theme_minimal() + xlab("Registrant") +
    scale_fill_brewer(palette="Reds", na.value="gray90", na.translate=FALSE) +
    exec(theme, !!!petr)

ggsave("overview.pdf", gg1, width=12, height=5)
