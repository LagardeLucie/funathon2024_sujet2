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
