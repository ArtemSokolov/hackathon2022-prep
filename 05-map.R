library(tidyverse)
library(scatterpie)

source("common.R")

us <- map_data("state")

rgst <- load_registration("data/tidy.csv") %>% select(Name, Stage)
atnd <- read_csv("data/attendees.csv", col_types = cols()) %>%
    inner_join(rgst, by = "Name")

## Tally up and map the U.S. attendees
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

## Okabe Ito palette
pal <- c("#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00",
    "#CC79A7", "#999999", "black")
dty <- colnames(atnd_us)[c(7, 3, 5, 6, 2, 4)]
ggus <- ggplot(us, aes(long, lat)) + theme_void() +
    geom_map(map = us, aes(map_id = region),
        fill = "gray90", color = "gray80") +
    geom_scatterpie(aes(x = long, y = lat, group = City),
                    pie_scale = 0.8, data = atnd_us, cols = dty, color = NA) +
    scale_fill_manual(values = pal[1:6], name = "Career Stage", breaks = dty) +
    theme(plot.background = element_rect(fill = "white", color = NA))

ggsave("plots/05-us.png", ggus, width = 10, height = 4)
