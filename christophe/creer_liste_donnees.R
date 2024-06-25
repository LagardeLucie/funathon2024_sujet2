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