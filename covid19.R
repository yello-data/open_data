library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(tidyr)
library(ggthemes)
library(forcats)

#("https://github.com/CSSEGISandData/COVID-19")

#DOWNLOAD AND TIDY
cvid <- read_csv("https://covid19.isciii.es/resources/serie_historica_acumulados.csv")
#cvid$FECHA[c(989, 990, 991)] <- c("11/4/2020", "11/4/2020", "11/4/2020")
names(cvid) <- tolower(names(cvid))
cvid$fecha <- dmy(cvid$fecha) #lubridate date
cvid <- cvid %>% mutate_at(3:9, funs(ifelse(is.na(.), 0, .))) %>%
  mutate_at(3:9, funs(as.numeric(.))) %>%
  filter(!is.na(fecha))
sum(is.na(cvid))

#CREATE POPULATION VARIABLE
library(readxl)
ccaa3 <- c("AND", "ARA", "AST", "BAL", "CNR", "CBR", "CLM", "CLE", "CAT", "CEU", 
           "VAL", "EXT", "GAL", "MAD", "MEL", "MUR", "NAV", "EUS", "RIJ")
pop <- read_xlsx("2853.xlsx")
cvid <- cvid %>%
  left_join(pop) %>%
  mutate(ccaa = str_replace(cvid$ccaa, paste0("^", unique(cvid$ccaa), "$"), ccaa3))
  
#CREATE TABLE VARIABLES
cvid29 <- cvid %>%
  arrange(ccaa, fecha) %>%
  group_by(ccaa) %>%
  rename(casos_cum = casos, hosp_cum = hospitalizados, 
         uci_cum = uci, fall_cum = fallecidos, rec_cum = recuperados) %>%
  mutate_at(vars(casos_cum:rec_cum), funs(n = . - lag(., default = 0))) %>%
  rename(casos = casos_cum_n, hosp = hosp_cum_n, uci = uci_cum_n, 
         fall = fall_cum_n, rec = rec_cum_n) %>%
  mutate_if(is.numeric, funs(pop = . / pop * 100000)) %>%
  select(-pop, -pop_pop) %>%
  ungroup()

names(cvid29) #variables
unique(cvid29$ccaa) #cases

#MOST AND LEAST CASES
most <- cvid29 %>%
  filter(fecha > Sys.Date() - 3) %>%
  group_by(ccaa) %>%
  mutate(c = casos_pop / sum(casos_pop)) %>%
  filter(fecha == max(fecha)) %>%
  ungroup() %>%
  top_n(6, c) %>%
  pull(ccaa)

least <- cvid29 %>%
  filter(fecha > Sys.Date() - 3) %>%
  group_by(ccaa) %>%
  mutate(c = casos_pop / sum(casos_pop)) %>%
  filter(fecha == max(fecha)) %>%
  ungroup() %>%
  top_n(-6, c) %>%
  pull(ccaa)

#TESTS
cvid29 %>%
  filter(ccaa %in% least, #cases
         fecha > Sys.Date() - 25) %>% #starting date
  ggplot(aes(x = fecha, y = casos_pop, col = ccaa)) + #change Y
  stat_smooth(aes(y = casos_pop, x = fecha), #mean of all CCAA, change Y
              inherit.aes = FALSE, col = "grey70", lty = 4, se = FALSE,
              span = .5, alpha = .2) +
  geom_smooth(se = F, span = .25, alpha = 0.8) +
  geom_point(alpha = .3, size = 2) +
  scale_color_pander() +
  guides(color = guide_legend(nrow = 2)) +
  xlim(c(Sys.Date() - 25, Sys.Date())) +
  labs(title = "Entrades a l'UCI a Espanya per cada 100.000 hab.",
       subtitle = paste0("Dades actualitzades: ", max(cvid29$fecha)),
         col= NULL, caption = "Font = Ministerio de Sanidad") +
  theme_hc() +
  theme(text = element_text(family = "Optima", size = 11),
        plot.title = element_text(size=14, face = "bold",
                                  margin = margin(b = -40)),
        plot.title.position = "plot",
        plot.subtitle = element_text(color = "gray50", face = "bold",
                                     size = 10,
                                     vjust = -14.5),
        legend.position = c(0.20, 0.62),
        legend.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(face = "bold", vjust = 4),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank()
)

#PROVES
cvid29 %>%
  mutate(ccaa = fct_reorder(ccaa, casos_pop, last)) %>%
  ggplot(aes(x = fecha, y = casos_pop)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = F) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ ccaa) +
  xlim(c(Sys.Date() - 25, Sys.Date()))

#TESTS
#wday(cvid$fecha, label = TRUE, abbr = FALSE)
cvid29 %>%
  filter(ccaa %in% c("CAT", "MAD", "EUS", "BAL", "CLM"), #cases
         fecha > Sys.Date() - 30) %>% #starting date
  ggplot(aes(x = fecha, y = casos_pop, col = ccaa)) + #change Y
  geom_rect(aes(xmin=Sys.Date() - 24, xmax=Sys.Date() - 22,
                ymin=0, ymax=45), fill="gray90", col = "gray90", alpha=0.1) +
  geom_rect(aes(xmin=as_date("2020-03-20"), xmax=as_date("2020-03-22"),
                ymin=0, ymax=45), fill="gray90", col = "gray90", alpha=0.1) +
  geom_rect(aes(xmin=as_date("2020-03-28"), xmax=as_date("2020-03-30"),
                ymin=0, ymax=45), fill="gray90", col = "gray90", alpha=0.1) +
  geom_rect(aes(xmin=as_date("2020-04-03"), xmax=as_date("2020-04-05"),
                ymin=0, ymax=45), fill="gray90", col = "gray90", alpha=0.1) +
  geom_rect(aes(xmin=as_date("2020-04-09"), xmax=as_date("2020-04-13"),
                ymin=0, ymax=45), fill="gray90", col = "gray90", alpha=0.1) +
  
  stat_smooth(aes(y = casos_pop, x = fecha), #mean of all CCAA, change Y
              inherit.aes = FALSE, col = "grey70", lty = 4, se = FALSE,
              span = .5, alpha = .2) +
  geom_smooth(se = F, span = .25, alpha = 0.8) +
  geom_point(alpha = .3, size = 2) +
  scale_color_pander(guide = guide_legend(nrow = 1)) +
  scale_y_continuous(breaks = seq(0, 45, 15), limits = c(0, 52)) +
  xlim(c(Sys.Date() - 25, Sys.Date())) +
  labs(title = "Contagis nous a España per cada 100.000 hab.",
       subtitle = paste0("Dades actualitzades: ", max(cvid29$fecha),
                         ". Festius en gris"),
       col= NULL, caption = "Font = Ministerio de Sanidad") +
  theme_hc() +
  theme(text = element_text(family = "Optima", size = 11),
        plot.title = element_text(size=14, face = "bold",
                                  margin = margin(b = -40)),
        plot.title.position = "plot",
        
        plot.subtitle = element_text(color = "gray50", face = "bold",
                                     size = 10,
                                     vjust = -14.5),
        legend.position = c(0.72, 0.88),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(face = "bold", vjust = 4),
        axis.title.y=element_blank(),
        axis.ticks.x = element_blank()
  )

