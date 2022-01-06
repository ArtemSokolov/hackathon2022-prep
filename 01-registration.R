library(tidyverse)

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

## Load all registrant information
X <- read_csv('registration.csv', col_types=cols()) %>% rename(!!!sn)

## Plot elements to remove (petr)
petr <- c("axis.text.x",
          "axis.ticks.x",
          "panel.grid.major",
          "panel.grid.minor") %>%
    set_names() %>% map(~element_blank())

## Overall view of registrant interest in the challenges
Z <- X %>% select(Name, all_of(ct)) %>%
    pivot_longer(-Name, names_to="Challenge", values_to="Interest")
gg1 <- ggplot(Z, aes(x=Name, y=Challenge, fill=Interest)) +
    geom_tile() + theme_minimal() + xlab("Registrant") +
    scale_fill_brewer(palette="Reds", na.value="gray90", na.translate=FALSE) +
    exec(theme, !!!petr)

ggsave("overview.pdf", gg1, width=12, height=5)
