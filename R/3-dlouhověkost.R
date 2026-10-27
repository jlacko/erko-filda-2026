# Máte dva censy: 1980 a 1930, padesát let od sebe
# posuďte závislost podílu obyvatel věkové kategorie 65+ v roce 1980
# - na podílu obyvatel 65+ v roce 1930 / hypotéza "Blue Zones" https://doi.org/10.1553/populationyearbook2013s87
# - na podílu obyvatel německé národnosti v roce 1930 / hypotéza "Postupim"

library(RCzechia) # česká geodata
library(dplyr)    # manipulace s data frames
library(ggplot2)  # malování obrázků

# census 1980
okresy_1980 <- RCzechia::historie("okresy_1980") %>%
   mutate(duchodci_80 = `obyvatelstvo celkem 65+` / `počet obyvatel přítomných`) %>% 
   select(duchodci_80) %>% 
   st_transform(5514)

ggplot() +
   geom_sf(data = st_transform(okresy_1980, 4326), color = NA, aes(fill = duchodci_80)) +
   scale_fill_viridis_c(limits = c(0, .2),
                        option = "magma",
                        direction = -1,
                        labels = scales::label_percent()) +
   labs(title = "Důchodci 1980")
   

# census 1930
okresy_1930 <- RCzechia::historie("okresy_1930") %>%
   mutate(nemci_30 = `národnost německá` / `počet obyvatel přítomných`,
          duchodci_30 = `obyvatelstvo celkem věk 65+` / `počet obyvatel přítomných` ) %>% 
   select(nemci_30, duchodci_30) %>% 
   st_transform(5514)

ggplot() +
   geom_sf(data = st_transform(okresy_1930, 4326), color = NA, aes(fill = duchodci_30)) +
   scale_fill_viridis_c(limits = c(0, .2),
                        option = "magma",
                        direction = -1,
                        labels = scales::label_percent()) +
   labs(title = "Důchodci 1930")

ggplot() +
   geom_sf(data = st_transform(okresy_1930, 4326), color = NA, aes(fill = nemci_30)) +
   scale_fill_viridis_c(limits = c(0, 1),
                        option = "magma",
                        direction = -1,
                        labels = scales::label_percent()) +
   labs(title = "Němci 1930")

# přenos metrik napříč nestejnými geometriemi = area weighted interpolation / https://www.scopus.com/pages/publications/0019095230 
okresy_1930$duchodci_80 <- st_interpolate_aw(okresy_1980["duchodci_80"],
                                             st_geometry(okresy_1930),
                                             extensive = F) %>% 
  pull(duchodci_80)


# modely technikou linární regrese
model_zdrave_ovzdusi <- lm(data = okresy_1930,
                           formula = duchodci_80 ~ duchodci_30)

model_postupim <- lm(data = okresy_1930,
                     formula = duchodci_80 ~ nemci_30)

# co mi říkají modely?
summary(model_zdrave_ovzdusi)
summary(model_postupim)

# ze summary R² pro srovnání...
summary(model_zdrave_ovzdusi)["r.squared"]
summary(model_postupim)["r.squared"]

