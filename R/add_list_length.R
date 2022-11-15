

#' Add a context-specific list-length
#'
#' Optionally creating a single column `list` with the full context.
#'
#' @param df Data frame with taxa and context columns.
#' @param taxa_col Character column name in `df` with taxa.
#' @param context Character vector of columns names defining the context.
#' @param create_list_col Create a single column named `list` with the full
#' context separated by `sep`.
#' @param sep Separator between contexts if `create_list_col`
#'
#' @return Data frame with extra column(s) list length, named `sr`, and, optionally,
#' list name, `list`.
#' @export
#'
#' @examples
  add_list_length <- function(df
                              , taxa_col = "taxa"
                              , context
                              , create_list_col = FALSE
                              , sep = "_"
                              ) {

    df %>%
      dplyr::left_join(df %>%
                         dplyr::filter(success > 0) %>%
                         dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col))
                                         , dplyr::across(tidyselect::any_of(context))
                                         ) %>%
                         dplyr::count(dplyr::across(tidyselect::any_of(context))
                                      , name = "list_length"
                                      )
                       ) %>%
      {if(create_list_col) (.) %>%
          tidyr::unite(col = "list"
                       , tidyselect::any_of(context)
                       , sep = sep
                       , remove = FALSE
                       ) else (.)
        }

  }
