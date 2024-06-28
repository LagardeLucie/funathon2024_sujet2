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
library(bslib)


# Import des fichiers de fonctions ---------------------------------------------

source("lucie/create_data_list.R")
source("lucie/clean_data_frame.R")
source("lucie/import_data.R")
source("lucie/figures.R")
source("lucie/divers_functions.R")
source("lucie/tables.R")


# Variables globales -----------------------------------------------------------

YEARS_LIST  <- as.character(2018:2022)
MONTHS_LIST <- 1:12


# Import des données -----------------------------------------------------------

urls <- create_data_list("sources.yml")

aeroports <- import_airport_data(urls)
compagnies <- import_compagnies_data(urls)
liaisons <- import_liaisons_data(urls)

localisations_aeroports <- st_read(urls$geojson$airport)


# Objets nécessaires à l'application -------------------------------------------

liste_aeroports <- unique(aeroports$apt)
aeroport_par_defaut <- liste_aeroports[1]

trafic_aeroports <- aeroports %>%
  mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
  filter(apt %in% aeroport_par_defaut) %>%
  mutate(date = as.Date(make_date(year = an, month = mois), "%B %Y"))

