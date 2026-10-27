# úkol = zakreslit kartogram [CZE] / choropleth map [ENG]
# ceny vánočního kapra v roce 2018 (poslední známý) po krajích

library(tidyverse) # protože dplyr, ggplot2 a spol.
library(RCzechia)  # česká geodata
library(ggspatial) # pro měřítka a směrové růžice 
library(leaflet)   # pro fancy dynamické overview

# protože licence na Carto / https://github.com/r-spatial/mapview/issues/520
options("mapviewBasemaps" = c("Esri.WorldGrayCanvas","Stadia.AlidadeSmoothDark","OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))

# podkladová data - ceny potravin (spotřební koš ČSÚ) v regionech a čase
kapr <- czso::czso_get_table("012052", dest_dir = "./data") %>%  # kešuj nebo zemři!
   filter(reprcen_txt %in% c("Kapr živý [1 kg]")   # relevantní cenový reprezentant,
          & uzemi_txt != "Česká republika"         # pouze regionální hodnoty (tj. ne ČR jako celek)
          & obdobiod >= "2018-12-01" 
          & obdobido <= "2019-01-01") 

# sloučení podkladových dat a podkladové geometrie
chrt_src <- RCzechia::kraje("low") %>% 
   inner_join(kapr, by = c("KOD_KRAJ" = "uzemi_kod"))

# co máme?
chrt_src

# statický obrázek basic
plot(chrt_src["hodnota"])

# statický obrázek fancy
ggplot(chrt_src) +
   geom_sf(aes(fill = hodnota)) +
   geom_sf_label(aes(label = hodnota), fill = "white") +
   scale_fill_viridis_c(labels = scales::label_number(suffix = " Kč")) +
   # měřítko mapy
   annotation_scale(location = "br", 
                    line_col = ("gray25"),
                    bar_cols = c("white", "gray25"),
                    height = unit(1/6, "cm"),
                    pad_x = unit(0, "cm"),
                    pad_y = unit(0, "cm"),
                    text_cex = .5) +
   # sever je když...
   annotation_north_arrow(width = unit(1, "cm"),
                          height = unit(1, "cm"),
                          pad_x = unit(1, "cm"),
                          pad_y = unit(0, "cm"),
                          which_north = "true",
                          style = north_arrow_fancy_orienteering(line_col = "gray25",
                                                                 fill = c("white", "gray25"),
                                                                 text_size = 8)) +
   
   theme_void() +
   labs(title = "Vánoční kapr",
        subtitle = "v posledním roce fyzického výběrového šetření (2018)",
        fill = "Cena v Kč",
        caption = "zdroj dat: ČSÚ")

# dynamické overview basic
mapview::mapview(chrt_src, zcol = "hodnota")

# dynamické overview fancy
barvy <- colorNumeric(palette = "RdYlGn",
                      reverse = T,
                      domain = chrt_src$hodnota)

leaflet(data = chrt_src) %>% 
   addProviderTiles("Esri.WorldGrayCanvas") %>%  # https://leaflet-extras.github.io/leaflet-providers/preview/
   addPolygons(fillColor = ~ barvy(hodnota),
               fillOpacity = 1/2,
               stroke = F,
               label = ~paste(hodnota, "Kč za kilo kapra"))

