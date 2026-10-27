# je na dálnici benzín dražší?

library(RCzechia) # pro česká geodata
library(dplyr)    # manipulace s data frames

# protože licence na Carto / https://github.com/r-spatial/mapview/issues/520
options("mapviewBasemaps" = c("Esri.WorldGrayCanvas","Stadia.AlidadeSmoothDark","OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))

benzin <- readRDS("./data/czech-oil-gc.rds") 

mapview::mapview(benzin)

# Američanům nenaléváme...
benzin <- st_filter(benzin, republika())

mapview::mapview(benzin)

dalnice <- RCzechia::silnice() %>%
  filter(TRIDA %in% c("Dálnice I. tř.", "Dálnice II. tř.")) %>% 
  summarize()
  
# vizuální kontrola
mapview::mapview(dalnice)

# blízkost jako boolean / kategorická veličina

okoli_dalnic <- dalnice %>% 
  st_transform(5514) %>% # protočím přes Křováka kvůli jasnější definici vzdálenosti
  st_buffer(units::set_units(2.5, "km")) %>% 
  st_transform(4326)

# vizuální kontrola
mapview::mapview(okoli_dalnic)

# doplnění benzínu o info o dálnici
benzin$blizko <- st_intersects(benzin, okoli_dalnic, sparse = F)[, 1]

# vizuální kontrola
mapview::mapview(benzin, zcol = "blizko")

# mám data - dál už "jenom statistika" :)
model_kategoricky <- lm(data = benzin,
                        formula = cena ~ blizko)

summary(model_kategoricky)

# vzdálenost jako spojitá veličina

benzin$vzdalenost <- st_distance(benzin, dalnice)[, 1] %>% 
  units::drop_units()

model_spojity <- lm(data = benzin, 
                    formula = cena ~ vzdalenost)

summary(model_spojity)

# který z modelů mám radši?
summary(model_spojity)["r.squared"]
summary(model_kategoricky)["r.squared"]