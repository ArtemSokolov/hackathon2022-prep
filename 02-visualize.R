library(tidyverse)
library(seriation)

## Plot elements to remove (petr)
petr <- c("axis.text.x",
          "axis.ticks.x",
          "panel.grid.major",
          "panel.grid.minor") %>%
    set_names() %>% map(~element_blank())

## Intereset level (il)
il <- c("Very interested"     = 3,
        "Somewhat interested" = 2,
        "Not interested"      = 1)

## Load cleaned registrant data + team assignments
X <- read_csv("data/assignment.csv", col_types=cols())
ct <- X %>% select(`01-artifacts`:`12-cosmetic`) %>% colnames()
   
## Compute the distance matrix (DM) based on registrant interest
DM <- X %>% select(Name, all_of(ct)) %>%
    mutate_at(ct, recode, !!!il) %>% as.data.frame() %>%
    column_to_rownames("Name") %>% dist()

## Compute optimal leaf ordering (olo) from hierarchical clustering
olo <- hclust(DM) %>% reorder(DM) %>%
    dendextend::order.hclust() %>% labels(DM)[.]

## Overall view of registrant interest in the challenges
Z <- X %>% select(Assignment, Name, all_of(ct)) %>%
    pivot_longer(all_of(ct), names_to="Challenge", values_to="Interest") %>%
    mutate(Assigned = ifelse(Challenge == Assignment, "Yes", "No"),
           Name      = factor(Name, levels=olo),
           Challenge = factor(Challenge, rev(ct)))

## Plot everything
gg <- ggplot(Z, aes(x=Name, y=Challenge, fill=Interest)) +
    geom_tile() + theme_minimal() + xlab("Registrant") +
    geom_tile(data=filter(Z, Assigned=="Yes"), color="black", size=1) +
    scale_fill_brewer(palette="Reds", na.value="gray90", na.translate=FALSE) +
    exec(theme, !!!petr)

ggsave("plots/assignments.pdf", gg, width=14, height=5)
