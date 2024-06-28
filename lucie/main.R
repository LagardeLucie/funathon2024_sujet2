# Import des packages nécessaires ----------------------------------------------

library(yaml)
library(readr)
library(dplyr)
library(stringr)
library(sf)
library(leaflet)
library(ggplot2)
library(plotly)
library(lubridate)
library(gt)


# Import des fichiers de fonctions ---------------------------------------------

source("lucie/create_data_list.R")
source("lucie/clean_data_frame.R")
source("lucie/import_data.R")
source("lucie/figures.R")
source("lucie/divers_functions.R")
source("lucie/tables.R")


# Paramètres -------------------------------------------------------------------

YEARS_LIST  <- as.character(2018:2022)
MONTHS_LIST <- 1:12
year <- YEARS_LIST[1]
month <- MONTHS_LIST[1]


# Création de tous les urls des sources ----------------------------------------

urls <- create_data_list("sources.yml")


# Import des données -----------------------------------------------------------

# Données sur les aéroports ====================================================

aeroports <- import_airport_data(urls)

aeroports %>% glimpse()

# Données sur les compagnies ===================================================

compagnies <- import_compagnies_data(urls)

compagnies %>% glimpse()

# Données sur les liaisons== ===================================================

liaisons <- import_liaisons_data(urls)

liaisons %>% glimpse()


# Localisation des données des aéroports ---------------------------------------

localisations_aeroports <- st_read(urls$geojson$airport)

# Carte minimaliste pour vérifier la bonne localisation des données
leaflet(localisations_aeroports) %>% 
  addTiles() %>% 
  addMarkers(label = ~Nom)


# Exploration des données ------------------------------------------------------

# Valorisation 1 ===============================================================

liste_aeroports <- unique(aeroports$apt)
aeroport_par_defaut <- liste_aeroports[1]

# Figure dynamique
plot_airport_line(aeroports, aeroport_par_defaut)

# Valorisation 2 ===============================================================

stats_aeroports <- summary_stat_airport(
  create_data_frame_from_input(aeroports, year, month)
)

create_table_airports(stats_aeroports)

# Valorisation 3 ===============================================================

map_leaflet_airport(aeroports, localisations_aeroports, month, year)
