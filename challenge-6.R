library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(ggrepel)

# Stelle den mitgegebenen Plot nach. Alle Daten dafür habt Ihr ja vorliegen!

# load df with all palamentarians  in the German BT

# load their Nebentätigkeiten

#data load

bundestag_2019 <- readRDS("data/bundestag_2019.rds")
nebeneinkuenfte <- read_csv("data/data-ROsix.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    name = str_split(name, ", ") %>%
      map_chr(~rev(.) %>% paste(collapse = " ")),
    #name_ = map_chr(str_split(name, ", "), function(x) paste(rev(x), collapse = " ")) #andere Schreibweise
    nebentaetigkeiten = nebentatigkeiten == "ja"
  )%>% 
  select(-nebentatigkeiten, -partei)

data <-Abgeordneten_full <- bundestag_2019 %>% 
  left_join(nebeneinkuenfte)

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

data %>% 
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
  geom_label_repel(data = subset(data, mindest_einkunfte_in_euro > 469999)
  )

data


  