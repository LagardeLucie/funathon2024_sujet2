create_data_frame_from_input <- function(data, annee, mois) {
  data <- data %>% 
    filter(an == annee,
           mois == mois)
  
  return(data)
}

summary_stat_airport <- function(data) {
  stat_aeroports <- data %>% 
    group_by(apt, apt_nom) %>% 
    summarise(total_pax_dep = sum(apt_pax_dep),
              total_pax_arr = sum(apt_pax_arr),
              total_pax_tr = sum(apt_pax_tr),
              total_pax = total_pax_dep + total_pax_arr + total_pax_tr) %>% 
    arrange(desc(total_pax)) %>% 
    ungroup()
  
  return(stat_aeroports)
}
