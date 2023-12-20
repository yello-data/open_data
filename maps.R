library(sf)
sp <- eurostat::eurostat_geodata_60_2016 %>%
  filter(LEVL_CODE == 2,
         CNTR_CODE == "ES")
#Peninsula y Canarias    
spp <- sp %>% filter(id!="ES70")
canarias<- sp %>% filter(id=="ES70")

ancha <- cvid29 %>%
  filter(fecha %in% c(Sys.Date() - 1, Sys.Date() - 15)) %>%
  group_by(id, ccaa) %>%
  transmute(fecha, cc = casos_cum_pop - lag(casos_cum_pop)) %>%
  filter(!is.na(cc))

sp %>%
  left_join(ancha) %>%
  ggplot() +
  scale_fill_gradient(low = "darkslategray1", high = "grey40") +
  geom_sf(aes(fill = cc))
