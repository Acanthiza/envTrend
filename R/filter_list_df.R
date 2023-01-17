

#' Filter occurrence data to a set of specifications
#'
#' Works on results from `make_list_df`. Probably works on other dfs.
#'
#' @param df Cleaned biological data.
#' @param var_cols Character name of column(s) in `df` that will be variables
#' in the model (or will define the model; e.g. `taxa_col`)
#' @param time_col Character name of column in `df` with time variable. Usually
#' `year`.
#' @param shortest_max Minimum allowable maximum length.
#' @param min_occurrences Minimum allowable occurrence.
#' @param min_years Minimum allowable years with occurrence.
#' @param min_year Must be records less than `min_year`.
#' @param max_year Much be records greater than `max_year`.
#'
#' @return Filtered data frame.
#' @export
#'
#' @examples
filter_list_df <- function(df
                           , var_cols = "taxa"
                           , time_col = "year"
                           , shortest_max = 3
                           , min_occurrences = 5
                           , min_years = 3
                           , min_span = 10
                           , max_year = as.numeric(format(Sys.Date(),"%Y")) - 1
                           ) {


  # df <- bio_lists

  df_filt <- df %>%
    dplyr::filter(success > 0)


  # Apply shortest_max
  shortest_max_remove <- df_filt %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::filter(list_length == max(list_length)) %>%
    dplyr::filter(list_length < shortest_max) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))

  # Apply min_occurrences
  min_occurences_remove <- df_filt %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::summarise(occurences = n()) %>%
    dplyr::filter(occurences < min_occurrences) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))

  # Apply min_years
  min_years_remove <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols))
                    , dplyr::across(tidyselect::any_of(time_col))
                    )  %>%
    dplyr::count(dplyr::across(tidyselect::any_of(var_cols))
                 , name = "years"
                 ) %>%
    dplyr::filter(years < min_years) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))

  # Apply min_span
  span_remove <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols))
                    , dplyr::across(tidyselect::any_of(time_col))
                    ) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::mutate(max_min = dplyr::case_when(year == max(year) ~ "max"
                                             , year == min(year) ~ "min"
                                             , TRUE ~ "not"
                                             )
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(max_min != "not") %>%
    tidyr::pivot_wider(names_from = "max_min"
                       , values_from = "year"
                       ) %>%
    dplyr::mutate(span = max - min) %>%
    dplyr::filter(!is.na(span)) %>%
    dplyr::filter(span < min_span) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))


  # Apply max_year
  max_year_remove <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols))
                    , dplyr::across(tidyselect::any_of(time_col))
                    ) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::filter(!!rlang::ensym(time_col) == max(!!rlang::ensym(time_col))) %>%
    dplyr::filter(!!rlang::ensym(time_col) < max_year) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))


  # summarise removes

  removes <- dplyr::bind_rows(mget(ls(pattern = "_remove$"))) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))


  # Return

  df <- df %>%
    dplyr::anti_join(removes)

  return(df)

}
