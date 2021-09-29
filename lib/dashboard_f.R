box::use(
  dplyr,
  rlang,
  data.table,
  purrr,
  data.table[`:=`],
  magrittr[`%>%`],
  tidyr[pivot_wider],
  scales,
  uti = lib / utils
)

rep_ad2 <- uti$csv_import("data/bfa_msna_21_hrp_admin2_names.csv")

# Goes with 0 everywhere
vec_base <- c("id_analysis", 
              "sub_rq",
              "subset",
              "type",
              "group_pop_name",
              "indicator",
              "label_choices")



vec_region_na <- c("id_analysis",
                   "sub_rq",
                   "subset",
                   "type",
                   "group_pop_name",
                   "indicator",
                   "label_choices",
                   "Centre-Sud",
                   "Sud-Ouest", 
                   "Centre")

#' Pivot analysis for tables
#' 
#' @export
pivot_ana <- function(analysis, lev) {
  
  analysis <- analysis %>% 
    dplyr::filter(level_name == lev)
  
  if (lev %in% c("National", "HRP - National")) {
    analysis <- analysis %>% 
      dplyr::select(id_analysis,
                    sub_rq, 
                    subset, 
                    type, 
                    indicator,
                    label_choices,
                    stat_num_round,
                    group_pop_name) %>% 
      dplyr::rename(Statistique = stat_num_round)
    
  } else if (lev %in% c("Région", "Region")) { #, "HRP - Region")) {
    analysis <- analysis %>% 
      dplyr::select(id_analysis,
                    sub_rq, 
                    subset, 
                    type,
                    admin1_name,
                    indicator, 
                    label_choices, 
                    stat_num_round,
                    group_pop_name) %>%
      tidyr:::pivot_wider(names_from = "admin1_name",
                          values_from = "stat_num_round") |>
    dplyr::mutate(
      dplyr::across(
        !dplyr::any_of(vec_base),
        (\(x) dplyr::if_else(
          group_pop_name != "PDI",
          tidyr::replace_na(x, replace = 0), x))
      ),
      dplyr::across(
        !dplyr::any_of(vec_region_na),
        (\(x) tidyr::replace_na(x, replace = 0))
      )
    )

            
  } else if (lev %in% c("Province", "HRP - Province")) {
    analysis <- analysis %>% 
      dplyr::select(
        id_analysis, 
        sub_rq,
        subset, 
        type, 
        admin2_name,
        indicator,
        label_choices, 
        stat_num_round,
        group_pop_name) %>%
      tidyr::pivot_wider(names_from = "admin2_name",
                         values_from = "stat_num_round") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(rep_ad2[repres_ad2 == "pdi", admin2_name]),
        (\(x) dplyr::if_else(
          group_pop_name == "PDI",
          tidyr::replace_na(x, replace = 0), x))
      ),
      dplyr::across(
        dplyr::any_of(rep_ad2[repres_ad2 == "hote",admin2_name]),
        (\(x) dplyr::if_else(
          group_pop_name != "PDI",
          tidyr::replace_na(x, replace = 0), x))
      ),
      dplyr::across(
        dplyr::any_of(rep_ad2[repres_ad2 == "ens",admin2_name]),
        (\(x) tidyr::replace_na(x, replace = 0))
      )
    )

  } else {
    stop("Either the level was not calculated or you choose a wrong one!")
  }
  
  analysis <- analysis %>% 
    dplyr::rename(
      `Sous-secteur` = sub_rq,
      `Sous-ensemble` = subset,
      Indicateur = indicator,
      Type = type,
      Choix = label_choices,
    ) %>% 
    dplyr::select(-c("id_analysis", "group_pop_name"))
   
  
  
  
  return(analysis)
}
# which(!(names(dt) %in% c("Sous-question",
#                          "Sous-ensemble",
#                          "Type",
#                          "Groupe",
#                          "Indicateur",
#                          "Choix")
