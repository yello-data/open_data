#ESP LA CURVA A ESPANYA
cvid29 %>%
  group_by(fecha) %>%
  summarize(contagios_esp = sum(casos),
            resueltos_esp = sum(rec + fall)) %>%
  gather(var, val, c(contagios_esp, resueltos_esp)) %>%
  ggplot(aes(x = fecha, y = val, col = var)) +
  geom_smooth(se = F) +
  geom_point() +
  xlim(c(Sys.Date() - 25, Sys.Date() - 1)) +
  theme_economist()


#FACET PER CCAA CUMULATS
cvc <- cvid29 %>%
  mutate(actius_pop_cum = casos_cum_pop - rec_cum_pop - fall_cum_pop,
         nom = fct_reorder(nom, actius_pop_cum, last)) %>%
  group_by(ccaa) %>%
  mutate(var = last(actius_pop_cum) < nth(actius_pop_cum, -6))

cvc %>%
  ggplot(aes(x = fecha, y = actius_pop_cum)) +
  geom_point(size = 0.5, alpha = 0.3, col = "antiquewhite4") +
  geom_smooth(se = F, span = 0.6, col = "firebrick4", data = filter(cvc, var == FALSE)) +
  geom_smooth(se = F, span = 0.8, col = "dodgerblue4", data = filter(cvc, var == TRUE)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~nom, nrow = 3) +
  scale_x_date(expand = c(0,0),
               limits = c(Sys.Date() - 30, Sys.Date())) +
  labs(title = "Casos actius en l'últim mes per CCAA per cada 100.000 hab.",
       subtitle = paste0("Ordenat per casos actius el darrer dia de dades: ", max(cvid29$fecha),  " | En blau, més de cinc dies en descens"),
       caption = "Font = Ministerio de Sanidad") +
  theme_hc() +
  theme(text = element_text(family = "Optima", size = 11),
        plot.title = element_text(size=14, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "gray50", face = "bold",
                                     size = 10),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank()
  )

#FACET MORTS
cvm <- cvid29 %>%
  mutate(nom = fct_reorder(nom, fall_pop, last)) %>%
  group_by(ccaa) %>%
  mutate(var = last(fall_pop) < nth(fall_pop, -6))
cvm %>%
  ggplot(aes(x = fecha, y = fall_pop)) +
  geom_point(size = 0.5, alpha = 0.3, col = "antiquewhite4") +
  geom_smooth(se = F, span = 0.6, col = "firebrick4", data = filter(cvm, var == FALSE)) +
  geom_smooth(se = F, span = 0.8, col = "dodgerblue4", data = filter(cvm, var == TRUE)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~nom, nrow = 3) +
  scale_x_date(expand = c(0,0),
               limits = c(Sys.Date() - 30, Sys.Date())) +
  labs(title = "Morts diaris els últims 30 dies per CCAA per cada 100.000 hab.",
       subtitle = paste0("Ordenat per morts el darrer dia de dades: ", max(cvid29$fecha),  " | En blau, més de cinc dies en descens"),
       caption = "Font = Ministerio de Sanidad") +
  theme_hc() +
  theme(text = element_text(family = "Optima", size = 11),
        plot.title = element_text(size=14, face = "bold"),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "gray50", face = "bold",
                                     size = 10),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank()
  )


#LA CURVA CASOS OBERTS VS. CASOS TANCATS
recs <- cvid29 %>%
  ungroup() %>%
  transmute(ccaa, fecha, casos_pop, rec_pop, fall_pop, resolts = rec_pop + fall_pop) %>%
  gather(var, val, c(casos_pop, rec_pop, fall_pop, resolts)) %>%
  filter(ccaa == c("CAT"),
         fecha > Sys.Date() - 25)

recs1 <- recs %>%
  filter(var %in% c("casos_pop", "resolts"))

recs2 <- recs %>%
  filter(var %in% c("fall_pop", "rec_pop"))

recs1 %>%
  ggplot(aes(x = fecha, y = val, col = var)) +
  geom_smooth(se = FALSE, span = 0.65) +
  geom_point(alpha = 0.5) +
  geom_smooth(data = recs2, aes(col = var), se = F,
              lty = 2, alpha = 0.2, size = 0.8, span = 0.65) +
  scale_color_pander(labels=c("Casos oberts", "Morts", 
                              "Recuperacions", "Casos tancats")) +
  xlim(c(Sys.Date() - 25, Sys.Date())) +
  scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 35)) +
  labs(title = "Casos oberts nous / casos tancats nous a Catalunya per cada 100.000 hab.",
       subtitle = paste0("Dades actualitzades: ", max(cvid28$fecha), " | Morts + Recuperacions = Casos Tancats "),
       col= NULL, caption = "Font = Ministerio de Sanidad") +
  guides(color = guide_legend(nrow = 2)) +
  theme_hc() +
  theme(text = element_text(family = "Optima", size = 11),
        plot.title = element_text(size=14, face = "bold",
                                  margin = margin(b = -40)),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "gray50", face = "bold",
                                     size = 10,
                                     vjust = -14.5),
        legend.position = c(0.20, 0.78),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(face = "bold", vjust = 4),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank()
  )
filter(cvid28, ccaa == "CAT", fecha == max(fecha))
