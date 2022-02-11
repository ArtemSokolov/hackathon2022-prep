library(tidyverse)
library(seriation)

## Load cleaned registrant data + team assignments
X <- read_csv("data/assignment.csv", col_types=cols())
ct <- X %>% select(`01-artifacts`:`11-cosmetic`) %>% colnames()

## Compute the distance matrix (DM) based on registrant interest
DM <- X %>% select(Name, all_of(ct)) %>%
    mutate_at(ct, recode, !!!g_il) %>% as.data.frame() %>%
    column_to_rownames("Name") %>% dist()

## Compute optimal leaf ordering (olo) from hierarchical clustering
olo <- hclust(DM) %>% reorder(DM) %>%
    dendextend::order.hclust() %>% labels(DM)[.]

## Overall view of registrant interest in the challenges
## Abusing Assignment column to store legend labels
Z <- X %>% select(Assignment, Name, Experience, GPU, all_of(ct)) %>%
    pivot_longer(all_of(ct), names_to="Challenge", values_to="Interest") %>%
    mutate(Assignment = ifelse(Challenge == Assignment, "Experience", "No"),
           Name       = factor(Name, levels=olo),
           Challenge  = factor(Challenge, rev(ct)))
ZA <- filter(Z, Assignment == "Experience")

## Removes the specified set of plot elements
theme_remove <- function(elems) {
    set_names(elems) %>%
        map(~element_blank()) %>%
        exec(theme, !!!.)
}

## Shorthand for bold element_text of specific size
etxt <- function(s, ...) element_text(size = s, face='bold', ...)

## Plot interest by itself
ggi <- ggplot(Z, aes(x=Name, y=Challenge, fill=Interest)) +
    geom_tile() + theme_minimal() + xlab("Registrant") +
    scale_fill_brewer(palette="Blues") +
    theme(legend.position="bottom",
          legend.title = etxt(12), legend.text = etxt(10),
          axis.title = etxt(12), axis.text = etxt(10)) +
    theme_remove(c("axis.text.x", "axis.ticks.x",
                   "panel.grid.major", "panel.grid.minor"))

ggsave("plots/interest.pdf", ggi, width=14, height=5)
ggsave("plots/interest.png", ggi, width=14, height=5)

## Overlay assignments
gg <- ggi +
    geom_tile(data=ZA, aes(color=Assignment), size=0.5) +
    geom_text(data=ZA, aes(color=Assignment, label=Experience)) +
    geom_text(data=filter(ZA, GPU=="Yes"), label="_" ) +
    scale_color_manual(values=c("Experience"="black", "Has GPU"="black")) +
    guides(
        fill  = guide_legend(override.aes = list(color=NA)),
        color = guide_legend(override.aes = list(fill="white", label=c("3","_"))))

ggsave("plots/assignments.pdf", gg, width=14, height=5)
ggsave("plots/assignments.png", gg, width=14, height=5)

