box::use(
  magrittr[`%>%`],
  #DT = data.table,
  data.table,
  data.table[`:=`],
  here[here]
)

#'##########################
#'##   Sub-list on names
#'##########################

#' Simplify "not in"'s writing
#'
#' @export
sublist <- function(liste, liste_names){
  liste[names(liste) %in% liste_names]
}



#'##########################
#'## Not in as `%!in%`
#'##########################

#' Simplify "not in"'s writing
#'
#' @export
`%!in%` <- Negate(`%in%`)


#'############################
#'## Remove vars from tibble
#'############################

#' Remove vars from tibble
#' 
#' @param .tbl A tibble
#' @param ... Many column names
#' 
#' @return A tibble with ... removed
#' 
#' @export
rm_vars <- function(.tbl, ...) {
  .vars <- rlang::qquos(...)
  .tbl %>% 
    select(-(!!!.vars))
}


#'##################################
#'## Recode variables to one value
#'##################################

#' Recode many variables of a tibble to one value
#' 
#' @param .tbl A tibble
#' @param values A vector of values
#' @param to_value A value
#'
#' @return A tibble with recoded values to one value
#'
#' @examples
#' rec_values(mtcars, 0, NA)
#'
#' @export
rec_values <- function(.tbl, values, to_value){
  .tbl %>% 
    dplyr::mutate_all(~ replace(., . %in% values, to_value))
  }


#'#####################
#'## Recode NAs to NA
#'#####################

#' Recode many types of NAs of a tibble to NA
#'
#' @param .tbl A tibble
#' @param values A vector of values
#' @param to_value A value
#'
#' @return A tibble with one type of NAs
#'
#' @examples
#' rec_na(mtcars)
#' 
#' @export
rec_na <- function(.tbl){
  nas <- c(NULL, "NULL", "N/A", "n/a", 999, 998, 888, " ", 
           9999, "(vide)", "d/m", "", "NA", "na", "", " ")
  .tbl %>% 
    rec_values(nas, NA)
  }


  
#' CSV initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param encoding Encoding for import (default to UTF-8)
#' @param NA.recode Shall we recode different types of NAs?
#' @param type.convert Use readr::type_convert()?
#' 
#' @export
csv_import <- function(path, encoding = "UTF-8"){
  data.table::fread(here::here(path), encoding = encoding) %>%
  data.table::setDT() %>%
  janitor::clean_names()
  }

#' XLSX initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param NA_recode Shall we recode different types of NAs?
#' @param type_convert Use readr::type_convert()?
#'
#' @export
xlsx_import <- function(path, sheet = NULL){
  df <- readxl::read_xlsx(here::here(path), sheet = sheet, col_types = "text")
  df %>%
  data.table::setDT() %>%
  janitor::clean_names() %>%
  readr::type_convert()
}



#' All sheets from a XLSX initial import with clean names (not an option!)
#'
#' @param path The project relative path to the csv file
#' @param NA_recode Shall we recode different types of NAs?
#' @param type_convert Use readr::type_convert()?
#'
#' @export
all_xlsx_import <- function(path, sheet = NULL){
  sheet_names <- readxl::excel_sheets(here::here(path))
  liste <- lapply(sheet_names, xlsx_import, path = path)
  names(liste) <- sheet_names
  return(liste)
}
