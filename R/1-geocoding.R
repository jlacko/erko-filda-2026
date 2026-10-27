# ogeokódovat a zakreslit na mapě lokalitu workshopu

library(sf)      # pro prostorová data
library(dplyr)   # pro manipulaci s data frames
library(ggplot2) # malování obrázků :)

# protože licence na Carto / https://github.com/r-spatial/mapview/issues/520
options("mapviewBasemaps" = c("OpenStreetMap"))


# konkrétní data - SDS FF UK 2026 geokódovaný
filda <- tidygeocoder::geo("Celetná 13, Praha 1",
                            method = "osm") %>% 
   sf::st_as_sf(coords = c("long", "lat"), crs = 4326)

# obecná data - hranice ČR jako celku, low resolution pro zjednodušení
hranice <- RCzechia::republika(resolution = "low")

# rychlý náhled interaktivní
mapview::mapview(filda)

# statická mapa - SDS FF UK 2026 v kontextu republiky jako celku
ggplot(data = filda) +
   geom_sf(data = hranice, fill = NA, lwd = 2/3) +
   geom_sf(pch = 4, color = "red", stroke = 2) +
   geom_sf_text(label = "SDS FF UK 2026", hjust = -.15) +
   theme_void() 


# jiný pohled na statickou mapu
ggplot() +
   geom_sf(data = filda, pch = 4, color = "red", stroke = 2) +
   geom_sf(data = RCzechia::reky("Praha"), color = "steelblue", size = 1) +
   geom_sf(data = subset(RCzechia::kraje(), KOD_CZNUTS3 == "CZ010"), fill = NA, size = 1) +
   labs(title = "SDS FF UK 2026") +
   theme_void()