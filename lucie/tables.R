create_table_airports <- function(stats_aeroports){
  
  stats_aeroports_table <- stats_aeroports %>%
    mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")) %>%
    select(name_clean, everything())
  
  table <- stats_aeroports_table %>% 
    gt() %>% 
    cols_hide(columns = starts_with("apt")) %>%
    fmt_number(columns = starts_with("total"), suffixing = TRUE) %>% 
    fmt_markdown(columns = "name_clean") %>% 
    cols_label(name_clean = md("**Aéroport**"), 
               total_pax_arr = md("**Arrivées**"), 
               total_pax_dep = md("**Départs**"), 
               total_pax_tr = md("**Transits**"), 
               total_pax = md("**Total**")) %>% 
    tab_header(title = md("**Statistiques de fréquentation**"),
               subtitle = md("Classement des aéroports")) %>%
    # tab_style(
    #   style = cell_fill(color = "powderblue"),
    #   locations = cells_title()) %>%
    tab_source_note(source_note = md("*Source : DGAC, à partir des données sur data.gouv.fr*")) %>% 
    opt_interactive()
  
  return(table)
}
