library(tidyverse)

library(maptools)
data(wrld_simpl)

X <- read_csv("data/countries.csv", col_types=cols()) %>%
    count(Country) %>% rename(id = Country, Count = n) %>%
    mutate(SqRCnt = sqrt(Count))

wrld_simpl@data$id <- wrld_simpl@data$NAME
wrld <- fortify(wrld_simpl, region="id")
wrld <- subset(wrld, id != "Antarctica")

pal <- rev(RColorBrewer::brewer.pal(n=7, name="RdBu"))[5:7]
vs <- c(1, 10, 20, 40)
gg <- ggplot() + theme_void() +
    geom_map(data=wrld, map=wrld, aes(long, lat, map_id=id), fill="white", color="darkgray", size=0.25) +
    geom_map(data=X, map=wrld, aes(map_id=id, fill=SqRCnt), color="darkgray", size=0.25) +
    scale_fill_gradientn(name = "# Registrants", colors=pal,
                         labels = vs, breaks=sqrt(vs))

ggsave("plots/05-map.png", gg, width=10, height=4)
