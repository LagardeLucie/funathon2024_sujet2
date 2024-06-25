# Permet de transformer un fichier YAML en liste imbriquée

#' @param source_file yaml file containing data urls 
#' @return list (level 1 = concepts, level 2 = year).
#'
#' @examples
#'  creer_liste_donnees("sources.yml")
#'  
creer_liste_donnees <- function(source_file){
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


# importer les donnees sur les compagnies
import_donnees_compagnies <- function(list_files){
  
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

# importer les donnees sur les liaisons
import_donnees_liaisons <- function(list_files){
  
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

# importer les donnees sur les aeroports
import_donnees_aeroport <- function(list_files){
  
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
