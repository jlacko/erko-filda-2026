# pokračování problému "kapr" technikami prostorové statistiky

library(tidyverse) # protože dplyr, ggplot2 a spol.
library(RCzechia) # česká geodata

# Naděje dožití v okresech a správních obvodech ORP při narození
raw_hope <- czso::czso_get_table("130140", dest_dir = "./data") %>%  # kešuj nebo zemři!
   filter(casref_do == as.Date("2021-12-31 UTC") & # poslední řez
             pohlavi_txt == "žena" &                # or whatever...
             vek_kod == "400000600001000" ) %>%    # při narození
   select(vuzemi_kod, doziti = hodnota)

chrt_src <- RCzechia::orp_polygony() %>% 
   rmapshaper::ms_simplify(keep = 1/75, keep_shapes = T) %>% 
   inner_join(raw_hope, by = c("KOD_ORP" = "vuzemi_kod"))

# pro připomenutí...
ggplot(data = chrt_src) +
   geom_sf(aes(fill = doziti)) +
   scale_fill_viridis_c(labels = scales::label_number(suffix = " years")) +
   theme_void() +
   labs(fill = "Life expectancy\nat birth") +
   theme(legend.text.align = 1,
         legend.title.align = 0.5)

# je naděje dožití rozmístěna náhodně?
library(spdep)

# pomocné objekty v rovinném CRS
orp_krovak <- chrt_src %>% 
   st_geometry() %>% 
   st_transform(5514)

# vlastní matice
matice <- orp_krovak %>% 
   poly2nb() %>%  # z polygonů na sousedy
   nb2listw()     # ze sousedů na matici

# Moranův test s přijetím předpokladů™
moran.test(chrt_src$doziti, matice, alternative = "two.sided")

# Moranův test technikou Monte Carlo / 9999 uvařených bramboraček ze factorial(14) teoreticky možných
bramboracka <- moran.mc(chrt_src$doziti, matice, nsim = 9999, alternative = "two.sided")

bramboracka

ggplot() +
   geom_histogram(aes(bramboracka$res), bins = 20, fill = "grey80", color = "grey25") +
   geom_vline(xintercept = bramboracka$statistic,
              color = "red",
              lwd = 1) +
   labs(x = "Moran's I", y = "Count of observations") +
   theme_minimal()


# líza!

lisa_src <- localmoran_perm(chrt_src$doziti, matice, nsim = 9999)

chrt_src$zscore <- lisa_src[,"Z.Ii"]
chrt_src$quadr <- attr(lisa_src, "quadr")$mean
chrt_src$pvalue <- p.adjust(lisa_src[,"Pr(z != E(Ii)) Sim"], "fdr")

chrt_src$cluster <- case_when(chrt_src$pvalue <= .05 ~ chrt_src$quadr,
                              T ~ NA)

# celkový přehled za ORPčka
ggplot(data = chrt_src) +
   geom_sf(aes(fill = cluster)) +
   scale_fill_viridis_d(na.value = "gray85",
                        labels = function(breaks) {breaks[is.na(breaks)] <- "Not Significant"; breaks}) +
   theme_void() +
   labs(fill = "Cluster") +
   theme(legend.text.align = 1,
         legend.title.align = 0.5)

