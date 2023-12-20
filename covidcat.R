#cvcat <- read_csv("cvidcat.csv", skip = 1,
#         col_names = c("data", "casos", "def", "altes", "casos_cum", "def_cum", "altes_cum"))

#Packages
library(httr)
library(tidyverse)
library(jsonlite)
library(lubridate)

#Download
cvcat <- GET("https://analisi.transparenciacatalunya.cat/resource/623z-r97q.json") %>%
  content(as = "text") %>%
  fromJSON() %>%
  as_tibble()


#Clean
cvcat$data <- ymd(ymd_hms(cvcat$data))
names(cvcat) <- c("data", "casos", "def", "altes", "casos_cum", "def_cum", "altes_cum")
cvcat <- mutate_if(cvcat, is.character, as.numeric)

cvcat <- cvcat %>%
  mutate(actius_cum = casos_cum - altes_cum - def_cum,
         actius = casos - altes - def)

ggplot(cvcat, aes(x = data, y = actius)) +
  geom_smooth(se = FALSE) +
  geom_point(alpha = 0.3)

#LA CURVA CASOS OBERTS VS. CASOS TANCATS
cvcat %>%
  mutate(resolts = altes + def) %>%
  select(data, casos, def, altes, resolts) %>%
  gather(vars, vals, casos:resolts) %>%
  ggplot(aes(x = data, y = vals, col = vars)) +
  geom_smooth(se = FALSE)

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

