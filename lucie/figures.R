plot_airport_line <- function(data, aeroport) {
  data_aeroport <- data %>% 
    mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>% 
    filter(apt == aeroport) %>% 
    mutate(date = as.Date(make_date(year = an, month = mois), "%B %Y"))
  
  plot_ly(data_aeroport, 
          x = ~date, 
          y = ~trafic, 
          text = ~apt_nom,
          hovertemplate = paste("<i>Aéroport:</i> %{text}<br>%{y} passagers"),
          type = "scatter", 
          mode = "lines+markers")
  
}


map_leaflet_airport <- function(df, airports_location, month, year) {
  palette <- c("green", "blue", "red")
  
  trafic_date <- df %>% 
    mutate(date = as.Date(make_date(year = an, month = mois), "%B %Y")) %>% 
    filter(an == year,
           mois == month)
  
  trafic_aeroports <- airports_location %>% 
    select(Code.OACI, geometry) %>% 
    left_join(trafic_date, by = c("Code.OACI" = "apt")) %>% 
    mutate(frequentation = apt_pax_arr + apt_pax_dep + apt_pax_tr,
           volume = ntile(frequentation, 3),
           color = palette[volume])
  
  icons <- awesomeIcons(
    icon = "plane",
    iconColor = 'black',
    library = "fa",
    markerColor = trafic_aeroports$color
  )
  
  leaflet(trafic_aeroports) %>% 
    addTiles() %>% 
    addAwesomeMarkers(label = ~paste0(apt_nom, " (", Code.OACI, ") : ", frequentation, " voyageurs"),
                      icon = icons[])
  
}
