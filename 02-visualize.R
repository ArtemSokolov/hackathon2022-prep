library(tidyverse)
library(seriation)

source("common.R")

## Removes the specified set of plot elements
theme_remove <- function(elems) {
    set_names(elems) %>%
        map(~element_blank()) %>%
        exec(theme, !!!.)
}

## Shorthand for bold element_text of specific size
etxt <- function(s, ...) element_text(size = s, face = "bold", ...)

## Load participant interest (intr) + team assignments (asmt)
intr <- load_registration("data/tidy.csv") %>%
    select(Name, `01-artifacts`:`11-cosmetic`) %>%
    mutate(across(-Name, replace_na,  1))
asmt <- read_csv("data/assignments.csv", col_types = cols()) %>%
    inner_join(intr, by = "Name")
ct <- colnames(intr) %>% setdiff("Name")

## Compute the distance matrix (dmat) based on registrant interest
dmat <- as.data.frame(intr) %>% column_to_rownames("Name") %>% dist()

## Compute optimal leaf ordering (olo) from hierarchical clustering
olo <- hclust(dmat) %>%
    reorder(dmat) %>%
    dendextend::order.hclust() %>%
    labels(dmat)[.]

## Overall view of registrant interest in the challenges
## Abusing Assignment column to store legend labels
il <- c("Not interested", "Somewhat interested", "Very interested")
z <- asmt %>%
    pivot_longer(all_of(ct), names_to = "Challenge", values_to = "Interest") %>%
    mutate(Assignment = ifelse(Challenge == Assignment, "Experience", "No"),
           Name       = factor(Name, levels = olo),
           Interest   = factor(il[Interest], il),
           Challenge  = factor(Challenge, rev(ct)))
za <- filter(z, Assignment == "Experience")

## Plot interest by itself
ggi <- ggplot(z, aes(x = Name, y = Challenge, fill = Interest)) +
    geom_tile() + theme_minimal() + xlab("Registrant") +
    scale_fill_brewer(palette = "Blues") +
    theme(legend.position = "bottom",
          legend.title = etxt(12), legend.text = etxt(10),
          axis.title = etxt(12), axis.text = etxt(10)) +
    theme_remove(c("axis.text.x", "axis.ticks.x",
                   "panel.grid.major", "panel.grid.minor"))

ggsave("plots/interest.pdf", ggi, width = 14, height = 5)
ggsave("plots/interest.png", ggi, width = 14, height = 5)

## Overlay assignments
pal <- c("Experience" = "black", "Has GPU" = "black")
gg <- ggi +
    geom_tile(data = za, aes(color = Assignment), size = 0.5) +
    geom_text(data = za, aes(color = Assignment, label = Experience)) +
    geom_text(data = filter(za, GPU == "Yes"), label = "_") +
    scale_color_manual(values = pal) +
    guides(
        fill  = guide_legend(override.aes = list(color = NA)),
        color = guide_legend(override.aes = list(fill = "white",
            label = c("3", "_")))
    )

ggsave("plots/assignments.pdf", gg, width = 14, height = 5)
ggsave("plots/assignments.png", gg, width = 14, height = 5)
