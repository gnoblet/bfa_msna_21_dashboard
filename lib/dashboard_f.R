box::use(
  dplyr,
  rlang,
  data.table,
  purrr,
  data.table[`:=`],
  magrittr[`%>%`],
  tidyr[pivot_wider],
  scales,
  shinyWidgets[...],
  shiny[...],
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
      `Ménage` = group_pop_name,
      Indicateur = indicator,
      Type = type,
      Choix = label_choices,
    ) %>% 
    dplyr::select(-"id_analysis")
   
  
  
  
  return(analysis)
}
# which(!(names(dt) %in% c("Sous-question",
#                          "Sous-ensemble",
#                          "Type",
#                          "Groupe",
#                          "Indicateur",
#                          "Choix")


#' @export
`%inT%` <- function(x, table) {
  if (!is.null(table) && ! "" %in% table) {
    x %in% table
  } else {
    rep_len(TRUE, length(x))
  }
}

#' @export
toggleDisplayServer <- function (session, id, display = c("none", "block", "inline-block", 
                                   "table-cell")) 
{
  display <- match.arg(display)
  session$sendCustomMessage(type = "toggleDisplay", message = list(id = id, 
                                                                   display = display))
}


#' @export
my_selectize_group_server <- function (input, output, session, data, vars) 
{
  ns <- session$ns
  toggleDisplayServer(session = session, id = ns("reset_all"), 
                      display = "none")
  rv <- shiny::reactiveValues(data = NULL, vars = NULL)
  observe({
    if (is.reactive(data)) {
      rv$data <- data()
    }
    else {
      rv$data <- as.data.frame(data)
    }
    if (is.reactive(vars)) {
      rv$vars <- vars()
    }
    else {
      rv$vars <- vars
    }
    for (var in names(rv$data)) {
      if (var %in% rv$vars) {
        toggleDisplayServer(session = session, id = ns(paste0("container-", 
                                                              var)), display = "table-cell")
      }
      else {
        toggleDisplayServer(session = session, id = ns(paste0("container-", 
                                                              var)), display = "none")
      }
    }
  })
  # reactive({lapply(X = rv$vars, FUN = function(x) {
  #   vals <- sort(unique(rv$data[[x]]))
  #   selectizeInput(session = session, inputId = x, selected = vals$x, 
  #                        choices = vals, server = TRUE)
  # })
  # })
  observe({
    lapply(X = rv$vars, FUN = function(x) {
      vals <- sort(unique(rv$data[[x]]))
      updateSelectizeInput(session = session, inputId = x, 
                           choices = vals, server = TRUE)
    })
  })
  observeEvent(input$reset_all, {
    lapply(X = rv$vars, FUN = function(x) {
      vals <- sort(unique(rv$data[[x]]))
      updateSelectizeInput(session = session, inputId = x, 
                           choices = vals, server = TRUE)
    })
  })
  observe({
    vars <- rv$vars
    lapply(X = vars, FUN = function(x) {
      ovars <- vars[vars != x]
      observeEvent(input[[x]], {
        data <- rv$data
        indicator <- lapply(X = vars, FUN = function(x) {
          data[[x]] %inT% input[[x]]
        })
        indicator <- Reduce(f = `&`, x = indicator)
        data <- data[indicator, ]
        if (all(indicator)) {
          toggleDisplayServer(session = session, id = ns("reset_all"), 
                              display = "none")
        }
        else {
          toggleDisplayServer(session = session, id = ns("reset_all"), 
                              display = "block")
        }
        for (i in ovars) {
          if (is.null(input[[i]])) {
            updateSelectizeInput(session = session, 
                                 inputId = i, choices = sort(unique(data[[i]])), 
                                 server = TRUE)
          }
        }
        if (is.null(input[[x]])) {
          updateSelectizeInput(session = session, inputId = x, 
                               choices = sort(unique(data[[x]])), server = TRUE)
        }
      }, ignoreNULL = FALSE, ignoreInit = TRUE)
    })
  })
  observe({
    updateSelectInput(inputId = "rq", choices = unique(rv$data$rq), selected = "Abris")
  })
  observe({
    updateSelectInput(inputId = "indicator", choices = unique(rv$data$indicator), selected = "Type d'abri")
  })
  
  observe({
    updateSelectInput(inputId = "group_pop_name", choices = unique(rv$data$group_pop_name), selected = "Ensemble")
  })
  
  
  return(reactive({
    data <- rv$data
    vars <- rv$vars
    indicator <- lapply(X = vars, FUN = function(x) {
      data[[x]] %inT% input[[x]]
    })
    indicator <- Reduce(f = `&`, x = indicator)
    data <- data[indicator, ]
    return(data)
  }))
}

