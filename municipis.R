area27 <- read_csv("https://github.com/ibesora/covid-19-data/raw/master/2020-03-27-areesBasiques.csv")
area30 <- read_csv("https://github.com/ibesora/covid-19-data/raw/master/2020-03-30-areesBasiques.csv")

area1 <- area27 %>%
  rename(c27 = cases, v27 = value) %>%
  left_join(area30, by = c("id" = "id")) %>%
  transmute(id, name = name.x, c27, c30 = cases, v27, v30 = value,
            cases = c30 - c27, value = v30 - v27)

area1 %>%
  ggplot(aes(x = c27, y = cases)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method= "lm", se = F)

area1[which(area1$cases > 20 & area1$c27 < 10), ]
area1[which(area1$cases > 10 & area1$c27 > 90), ]



library(tidyverse)
library(jsonlite)
library(lubridate)
mun <- jsonlite::fromJSON("https://analisi.transparenciacatalunya.cat/resource/jj6z-iyrp.json") %>%
  as_tibble()
mun$data <- dmy(mun$data)

mun <- read_csv("rsconnect/casosmun2.csv")
mun$TipusCasData <- dmy(mun$TipusCasData)
glimpse(mun)
summary(mun)
mun_cas <- mun %>%
  filter(MunicipiDescripcio == "Castellar del Vallès") %>%
  mutate(TipusCasDescripcio = fct_rev(TipusCasDescripcio),
         wday = wday(TipusCasData, week_start = 1) %in% c(6,7))
mun_cas %>% View

gtext <- c(y = 35, angle = 90, col = "red", hjust = -0.6)
  
mun_cas_line <- mun_cas %>%
  mutate(TipusCasDescripcio = if_else(TipusCasDescripcio == "Sospitós", "Sospitós", "Positiu")) %>%
  group_by(TipusCasData, TipusCasDescripcio) %>%
  summarize(NumCasos = sum(NumCasos))

mun_cas_line %>%
  ggplot(aes(x = TipusCasData)) +
  geom_area(aes(y = NumCasos, fill = TipusCasDescripcio), position= "identity",
            data = filter(mun_cas_line, TipusCasDescripcio == "Sospitós")) +
  geom_area(aes(y = NumCasos, fill = TipusCasDescripcio), position= "identity", alpha = 0.7,
            data = filter(mun_cas_line, TipusCasDescripcio == "Positiu")) +
  geom_vline(xintercept = as.Date("2020-03-15"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Estat d'Alarma I"), vjust = -0.6, hjust = "left",
            y = 20, angle = 90, col = "firebrick4",
            x = as.Date("2020-03-15")) +
  geom_vline(xintercept = as.Date("2020-03-29"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Estat d'Alarma II"), vjust = -0.6, hjust = "left",
            y = 20, angle = 90, col = "firebrick4",
            x = as.Date("2020-03-29")) +
  geom_vline(xintercept = as.Date("2020-05-04"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Inici Fase 0"), vjust = -0.6, hjust = "left",
            y = 20, angle = 90, col = "firebrick4",
            x = as.Date("2020-05-04")) +
  geom_vline(xintercept = as.Date("2020-05-18"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Inici Fase 0.5"), vjust = -0.6, hjust = "left",
            y = 20, angle = 90, col = "firebrick4",
            x = as.Date("2020-05-18")) +
  scale_fill_brewer(type = "qual", palette = 3, direction = -1) +
  scale_x_date(date_breaks = "15 days", date_labels = "%b %d") +
  labs(title = "Dades Covid19 a Castellar del Vallès",
       subtitle = paste0("Última actualització: ", Sys.Date()),
       y = "Número de casos", x = NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank())




mun_cas %>%
  mutate(TipusCasDescripcio = if_else(TipusCasDescripcio == "Sospitós", "Sospitós", "Positiu")) %>%
  ggplot(aes(x = TipusCasData)) +
  geom_col(aes(y = NumCasos, fill = TipusCasDescripcio), position= "identity",
            data = filter(mun_cas_line, TipusCasDescripcio == "Sospitós")) +
  geom_col(aes(y = NumCasos, fill = TipusCasDescripcio), position= "identity", alpha = 0.7,
            data = filter(mun_cas_line, TipusCasDescripcio == "Positiu")) +
  geom_vline(xintercept = as.Date("2020-03-15"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Estat d'Alarma I"), vjust = -0.6, hjust = "left",
            y = 20, angle = 90, col = "firebrick4", size = 12,
            x = as.Date("2020-03-15")) +
  geom_vline(xintercept = as.Date("2020-03-29"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Estat d'Alarma II"), vjust = -0.6, hjust = "left",
            y = 20, angle = 90, col = "firebrick4",size = 12,
            x = as.Date("2020-03-29")) +
  geom_vline(xintercept = as.Date("2020-05-04"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Inici Fase 0"), vjust = -0.6, hjust = "left",
            y = 20, angle = 90, col = "firebrick4", size = 12,
            x = as.Date("2020-05-04")) +
  geom_vline(xintercept = as.Date("2020-05-18"), lty = 2, col = "firebrick4") +
  geom_text(aes(label = "Inici Fase 0.5"), vjust = -0.6, hjust = "left",
             y = 20, angle = 90, col = "firebrick4", size = 12,
            x = as.Date("2020-05-18")) +
  scale_color_brewer(type = "qual", palette = 3, direction = -1) +
  scale_fill_brewer(type = "qual", palette = 3, direction = -1) +
  scale_x_date(date_breaks = "15 days", date_labels = "%b %d") +
  labs(title = "Dades Covid19 a Castellar del Vallès",
       subtitle = paste0("Última actualització: ", Sys.Date()),
       y = "Número de casos", x = NULL) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_blank())

mun_cas %>%
  mutate(TipusCasDescripcio = fct_rev(TipusCasDescripcio),
         TipusCasDescripcio = if_else(TipusCasDescripcio == "Sospitós", "Sospitós", "Positiu"),
         TipusCasData = week(TipusCasData)) %>%
  ggplot(aes(x = TipusCasData, y = NumCasos, fill = TipusCasDescripcio)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(title = "Dades Covid19 a Castellar del Vallès") +
  theme(legend.position = "bottom")

mun_cas %>%
  mutate(TipusCasDescripcio = fct_rev(TipusCasDescripcio),
         TipusCasDescripcio = if_else(TipusCasDescripcio == "Sospitós", "Sospitós", "Positiu"),
         TipusCasData = week(TipusCasData)) %>%
  ggplot(aes(x = TipusCasData, y = NumCasos, fill = TipusCasDescripcio)) +
  geom_col(position = "dodge") +
  facet_wrap(~ SexeDescripcio) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(title = "Dades Covid19 a Castellar del Vallès") +
  theme(legend.position = "bottom")



mun_cas %>%
  mutate(TipusCasDescripcio = fct_rev(TipusCasDescripcio),
         TipusCasDescripcio = if_else(TipusCasDescripcio == "Sospitós", "Sospitós", "Positiu"),
         TipusCasData = week(TipusCasData)) %>%
  group_by(TipusCasData, TipusCasDescripcio) %>%
  summarize(NumCasos = sum(NumCasos)) %>%
  ggplot(aes(x = TipusCasData, y = NumCasos, col = TipusCasDescripcio)) +
  geom_line() +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(title = "Dades Covid19 a Castellar del Vallès") +
  theme(legend.position = "bottom")

mun_cas %>%
  mutate(TipusCasDescripcio = fct_rev(TipusCasDescripcio),
         TipusCasDescripcio = if_else(TipusCasDescripcio == "Sospitós", "Sospitós", "Positiu"),
         TipusCasData = week(TipusCasData)) %>%
  group_by(TipusCasData, TipusCasDescripcio, SexeDescripcio) %>%
  summarize(NumCasos = sum(NumCasos)) %>%
  ggplot(aes(x = TipusCasData, y = NumCasos, col = TipusCasDescripcio)) +
  geom_line() +
  facet_wrap(~ SexeDescripcio) +
  scale_fill_brewer(type = "qual", palette = 3) +
  labs(title = "Dades Covid19 a Castellar del Vallès") +
  theme(legend.position = "bottom")

