library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(ggrepel)

# Stelle den mitgegebenen Plot nach. Alle Daten dafür habt Ihr ja vorliegen!

# load df with all palamentarians  in the German BT
# bundestag_2019 <- …

# load their Nebentätigkeiten
# nebeneinkuenfte <- …

#data

bundestag_2019 <- readRDS("data/bundestag_2019.rds")
abgeordnetenwatch <- read_csv("data/data-ROsix.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    name = str_split(name, ", ") %>%
      map_chr(~rev(.) %>% paste(collapse = " ")),
    #name_ = map_chr(str_split(name, ", "), function(x) paste(rev(x), collapse = " ")) #andere Schreibweise
    nebentaetigkeiten = nebentatigkeiten == "ja"
  )%>% 
  select(-nebentatigkeiten, -partei)

Abgeordneten_full <- bundestag_2019 %>% 
  left_join(abgeordnetenwatch)

partei_farben <- list(
  "CDU" = "black",
  "SPD" = "red",
  "CSU" = "black",
  "FDP" = "yellow",
  "Grüne" = "green",
  "Linke" = "violet",
  "AfD" = "blue",
  "fraktionslos" = "grey"
)




#plot
options(scipen = 999) # e zu ganzen Zahlen

Abgeordneten_full %>% 
  ggplot(
    aes(
      x = lebensdaten,
      y = land,
      label = name
    )
  )+
  geom_jitter(aes(
    size = mindest_einkunfte_in_euro, 
    color = fraktion
  ))+
  scale_color_manual(values = partei_farben)+
  theme_minimal()+
  labs(
    x = "Jahrgang",
    y = "Bundesland",
    size = "Einkünfte",
    color = "Partei"
  )+
  geom_label_repel(data = subset(Abgeordneten_full, mindest_einkunfte_in_euro > 469999)
  )

Abgeordneten_full
# data <- …

# ggplot(data) + …
  