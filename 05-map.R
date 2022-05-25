library(tidyverse)
library(scatterpie)

source("common.R")

## Shorthand notations
etxt <- function(s, ...) element_text(size = s, face = "bold", ...)
eblk <- element_blank

## Okabe Ito palette
pal <- c("#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00",
    "#CC79A7", "#999999", "black")

## Map coordinates
us <- map_data("state")

## Helper function for arranging institutions by the number of attendees
f <- function(.df)
    count(.df, Institution) %>%
        arrange(n) %>%
        pull(Institution)

## Load all data
rgst <- load_registration("data/tidy.csv") %>% select(Name, Stage)
atnd <- read_csv("data/attendees.csv", col_types = cols()) %>%
    inner_join(rgst, by = "Name") %>%
    mutate(
        State = ifelse(Country != "United States", "International",
            str_split(City, " ", simplify = TRUE)[, 2]),
        State = factor(State, levels = c(
            setdiff(State, "International"),
            "International")
            ),
        Institution = ifelse(Country != "United States",
            str_c(Institution, " (", Country, ")"), Institution),
    ) %>%
    mutate(Institution = factor(Institution, levels=f(.)))

## Barplot of all institutions
ggall <- ggplot(atnd, aes(x = Institution, fill = State)) +
    geom_bar() + ggtitle(str_c(nrow(atnd), " attendees")) +
    theme_minimal() + coord_flip() + xlab("") + ylab("") +
    scale_fill_manual(values = pal[c(1:6,9)]) +
    theme(
        axis.text = etxt(12), title = etxt(14),
        legend.title = etxt(14), legend.text = etxt(12),
        panel.grid.major.y = eblk(), panel.grid.minor.y = eblk(),
        plot.background = element_rect(fill = "white", color = NA))

ggsave("plots/05-all.png", ggall, width=10, height=5)

## Tally up the U.S. attendees
atnd_us <- na.omit(atnd) %>%
    group_by(City) %>%
    count(Stage) %>%
    mutate(Stage = ifelse(
        grepl("Software", Stage),
        "Industry/non-academic", Stage
    )) %>%
    ungroup() %>%
    spread(Stage, n, fill = 0) %>%
    left_join(select(maps::us.cities, City = name, lat, long), by = "City")

## Draw the map
dty <- colnames(atnd_us)[c(7, 3, 5, 6, 2, 4)]
ggus <- ggplot(us, aes(long, lat)) + theme_void() +
    geom_map(map = us, aes(map_id = region),
        fill = "gray90", color = "gray80") +
    geom_scatterpie(aes(x = long, y = lat, group = City),
                    pie_scale = 0.8, data = atnd_us, cols = dty, color = NA) +
    scale_fill_manual(values = pal[1:6], name = "Career Stage", breaks = dty) +
    theme(
        legend.title = etxt(14), legend.text = etxt(12),
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "bottom")

ggsave("plots/05-us.png", ggus, width = 10, height = 5)
