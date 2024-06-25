# Permet de transformer un fichier YAML en liste imbriquée

#' @param source_file yaml file containing data urls 
#' @return list (level 1 = concepts, level 2 = year).
#'
#' @examples
#'  create_data_list("sources.yml")
#'  
create_data_list <- function(source_file){
  catalogue <- yaml::read_yaml(source_file)
  return(catalogue)
}

# nettoyer data frame
clean_dataframe <- function(df){
  
  # Create an et mois columns
  df <- df %>% 
    mutate(
      an = str_sub(ANMOIS,1,4),
      mois = str_sub(ANMOIS,5,6)
    ) %>%
    mutate(
      mois = str_remove(mois, "^0+")
    )
  
  # lower case for variable names
  colnames(df) <- tolower(colnames(df))
  
  return(df)
  
}


# importer les donnees
import_airport_data <- function(list_files){
  
  pax_apt_all <- readr::read_csv2(
    list_files, 
    col_types = cols(
      ANMOIS = col_character(),
      APT = col_character(),
      APT_NOM = col_character(),
      APT_ZON = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(pax_apt_all)
  
}


import_compagnies_data <- function(list_files){
  
  pax_cie_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      CIE = col_character(),
      CIE_NOM = col_character(),
      CIE_NAT = col_character(),
      CIE_PAYS = col_character(),
      .default = col_double()
    )
  ) %>% 
    clean_dataframe()
  
  return(pax_cie_all)
  
  
}


import_liaisons_data <- function(list_files){
  
  pax_lsn_all <- readr::read_csv2(
    file = list_files,
    col_types = cols(
      ANMOIS = col_character(),
      LSN = col_character(),
      LSN_DEP_NOM = col_character(),
      LSN_ARR_NOM = col_character(),
      LSN_SCT = col_character(),
      LSN_FSC = col_character(),
      .default = col_double()
    ) 
  ) %>% 
    clean_dataframe()
  
  return(pax_lsn_all)
  
  
}

### Fonctions graphiques
plot_airport_line <- function(df, selected_airport){
  trafic_aeroports <- df %>%
    mutate(trafic = apt_pax_dep + apt_pax_tr + apt_pax_arr) %>%
    filter(apt %in% selected_airport) %>%
    mutate(
      date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
    )
  
  figure_plotly <- trafic_aeroports %>%
    plot_ly(
      x = ~date, y = ~trafic,
      text = ~apt_nom,
      hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}") ,
      type = 'scatter', mode = 'lines+markers')
  
  return(figure_plotly)
}


map_leaflet_airport <- function(df, airports_location, months, years){
  
  palette <- c("green", "blue", "red")
  
  trafic_date <- df %>%
    mutate(
      date = as.Date(paste(anmois, "01", sep=""), format = "%Y%m%d")
    ) #%>%
    #filter(mois %in% month, an %in% year)
  
  trafic_aeroports <- airports_location %>%
    inner_join(trafic_date, by = c("Code.OACI" = "apt"))
  
  
  trafic_aeroports <- trafic_aeroports %>%
    mutate(
      volume = ntile(trafic, 3)
    ) %>%
    mutate(
      color = palette[volume]
    )  
  
  icons <- awesomeIcons(
    icon = 'plane',
    iconColor = 'black',
    library = 'fa',
    markerColor = trafic_aeroports$color
  )
  
  carte_interactive <- leaflet(trafic_aeroports) %>% addTiles() %>%
    addAwesomeMarkers(
      icon=icons[],
      label=~paste0(Nom, "", " (",Code.OACI, ") : ", trafic, " voyageurs")
    )
  
  return(carte_interactive)
}


### Creer tables
create_table_airports <- function(stats_aeroports){
  
  stats_aeroports_table <- stats_aeroports %>%
    mutate(name_clean = paste0(str_to_sentence(apt_nom), " _(", apt, ")_")
    ) %>%
    select(name_clean, everything())
  
  table_aeroports <- gt(stats_aeroports_table)
  
  table_aeroports <- table_aeroports %>%
    cols_hide(columns = starts_with("apt"))
  
  table_aeroports <- table_aeroports %>%
    fmt_number(columns = starts_with("pax"), suffixing = TRUE)
  
  table_aeroports <- table_aeroports %>%
    fmt_markdown(columns = "name_clean")
  
  table_aeroports <- table_aeroports %>%
    cols_label(
      name_clean = md("**Aéroport**"),
      paxdep = md("**Départs**"),
      paxarr = md("**Arrivée**"),
      paxtra = md("**Transit**")
    ) %>%
    tab_header(
      title = md("**Statistiques de fréquentation**"),
      subtitle = md("Classement des aéroports")
    ) %>%
    tab_style(
      style = cell_fill(color = "powderblue"),
      locations = cells_title()
    ) %>%
    tab_source_note(source_note = md("_Source: DGAC, à partir des données sur data.gouv.fr_"))
  
  table_aeroports <- table_aeroports %>%
    opt_interactive()
  
  return(table_aeroports)
  
}

### Simplifier le texte
simplify_text <- function(text){
  text <- gsub('[:punct:]',' ', text)
  text <- gsub(' ','', text)
  return(
    tolower(stri_trans_general(text, "Latin-ASCII"))
  )
}

### Fonctions diverses

create_data_from_input <- function(data, year, month){
  data <- data %>%
    filter(mois %in% month, an %in% year)
  return(data)
}

summary_stat_airport <- function(data){
  table2 <- data %>%
    group_by(apt, apt_nom) %>%
    summarise(
      paxdep = round(sum(apt_pax_dep, na.rm = T),3),
      paxarr = round(sum(apt_pax_arr, na.rm = T),3),
      paxtra = round(sum(apt_pax_tr, na.rm = T),3)) %>%
    arrange(desc(paxdep)) %>%
    ungroup()
  
  return(table2)
}

summary_stat_liaisons <- function(data){
  agg_data <- data %>%
    group_by(lsn_fsc) %>%
    summarise(
      paxloc = round(sum(lsn_pax_loc, na.rm = TRUE)*1e-6,3)
    ) %>%
    ungroup()
  return(agg_data)
}



