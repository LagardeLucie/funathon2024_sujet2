# A executer one shot
# renv::restore()
library(readr)
library(dplyr)
library(lubridate)
library(stringr)
library(sf)
library(ggplot2)
library(plotly)
library(gt)
library(leaflet)
library(bslib)

source("christophe/fonctions.R")

# import des premières bases
urls <- creer_liste_donnees("sources.yml")

MONTHS_LIST = 1:12

pax_apt_all <- import_donnees_aeroport(unlist(urls$airports))
pax_cie_all <- import_donnees_compagnies(unlist(urls$compagnies))
pax_lsn_all <- import_donnees_liaisons(unlist(urls$liaisons))

airports_location <- st_read(urls$geojson$airport)

# première carte

# Pour faire des petits navions ;°)
AvionIcon <- makeIcon(
  iconUrl = "christophe/aéroport-icone.png",
  iconWidth = 25, 
  iconHeight = 25
)


leaflet(airports_location) %>% 
  addTiles() %>% 
  addMarkers(popup = ~Nom, icon = AvionIcon)
