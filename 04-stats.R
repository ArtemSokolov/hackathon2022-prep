library(tidyverse)
source("common.R")

rgst <- load_registration("data/tidy.csv") %>%
    mutate(across(Stage, factor, levels = unique(Stage)[c(2, 6, 5, 4, 1, 3)]))

## Shorthand notations
etxt <- function(s, ...) element_text(size = s, face = "bold", ...)
eblk <- element_blank

## Career Stage
gg1 <- ggplot(rgst, aes(x = Stage, fill = GPU)) +
    geom_bar() + ggtitle(str_c(nrow(rgst), " registrants")) +
    theme_minimal() + coord_flip() +
    scale_fill_manual(values = c(Yes = "tomato", No = "steelblue")) +
    theme(axis.title = eblk(), axis.text = etxt(12), title = etxt(14),
          legend.title = etxt(14), legend.text = etxt(12),
          panel.grid.major.y = eblk(), panel.grid.minor.y = eblk(),
          plot.background = element_rect(fill = "white", color = NA))

ggsave("plots/04-stage.png", gg1, width = 10, height = 4)

## Experience Level
gg2 <- ggplot(rgst, aes(x = Experience, fill = GPU)) +
    geom_bar() + ggtitle(str_c(nrow(rgst), " registrants")) +
    theme_minimal() + coord_flip() +
    xlab("Experience Level") + ylab("") +
    scale_fill_manual(values = c(Yes = "tomato", No = "steelblue")) +
    theme(axis.title.y = etxt(14), axis.title.x = eblk(), axis.text = etxt(12),
          legend.title = etxt(14), legend.text = etxt(12), title = etxt(14),
          panel.grid.major.y = eblk(), panel.grid.minor.y = eblk(),
          plot.background = element_rect(fill = "white", color = NA))
    
ggsave("plots/04-experience.png", gg2, width = 7, height = 4)

## Language coverage
library(UpSetR)
langs <- mutate(rgst, across(Languages, str_replace_all, ", ", "&")) %>%
    count(Languages) %>%
    with(set_names(n, Languages))

pdf("plots/04-languages.pdf", width = 7, height = 3)
upset(fromExpression(langs), order.by = "freq", mb.ratio = c(0.70, 0.30))
dev.off()

## Most-represented Institutions
inst <- count(rgst, Institution) %>%
    filter(n > 1) %>%
    arrange(n) %>%
    mutate(across(Institution, factor, Institution, Institution))
gg3 <- ggplot(inst, aes(x = Institution, y = n)) +
    geom_bar(stat = "identity") + ylab("# Registrants") +
    theme_minimal() + coord_flip() +
    scale_y_continuous(breaks = seq(2, 10, by = 2)) +
    theme(axis.title.y = eblk(), axis.text = etxt(12),
          panel.grid.major.y = eblk(), panel.grid.minor.y = eblk(),
          panel.grid.minor.x = eblk(),
          plot.background = element_rect(fill = "white", color = NA))

ggsave("plots/04-inst.png", gg3, width = 7, height = 4)
