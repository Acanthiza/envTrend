

#' Filter occurrence data to a set of specifications
#'
#' Works on results from `make_list_df`. Reworked to focus on only removing taxa
#'  (i.e. not visits or other contexts).
#'
#' @param df Cleaned biological data.
#' @param taxa_col Character name of column in `df` with taxa.
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
                           , taxa_col = "taxa"
                           , time_col = "year"
                           , shortest_max = 3
                           , min_occurrences = 5
                           , min_years = 3
                           , min_year = as.numeric(format(Sys.Date(),"%Y")) - 11
                           , max_year = as.numeric(format(Sys.Date(),"%Y")) - 1
                           ) {


  # df <- bio_lists

  df_filt <- df %>%
    dplyr::filter(success > 0)


  # Apply shortest_max
  shortest_max_remove <- df_filt %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(taxa_col))) %>%
    dplyr::filter(list_length == max(list_length)) %>%
    dplyr::filter(list_length < shortest_max) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col)))

  # Apply min_occurrences
  min_occurences_remove <- df_filt %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(taxa_col))) %>%
    dplyr::summarise(occurences = n()) %>%
    dplyr::filter(occurences < min_occurrences) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col)))

  # Apply min_years
  min_years_remove <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col))
                    , dplyr::across(tidyselect::any_of(time_col))
                    )  %>%
    dplyr::count(dplyr::across(tidyselect::any_of(taxa_col))
                 , name = "years"
                 ) %>%
    dplyr::filter(years < min_years) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col)))

  # Find min_year
  temp <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col))
                    , dplyr::across(tidyselect::any_of(time_col))
                    )

  min_year_remove <- temp %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(taxa_col))) %>%
    dplyr::filter(!!rlang::ensym(time_col) == min(!!rlang::ensym(time_col))) %>%
    dplyr::filter(!!rlang::ensym(time_col) > min_year) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col)))

  # Find max_year
  max_year_remove <- temp %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(taxa_col))) %>%
    dplyr::filter(!!rlang::ensym(time_col) == max(!!rlang::ensym(time_col))) %>%
    dplyr::filter(!!rlang::ensym(time_col) < max_year) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col)))


  # summarise removes

  removes <- purrr::reduce(mget(ls(pattern = "_remove$"))
                           , dplyr::full_join
                           , by = "taxa"
                           ) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col)))


  # Return

  df <- df %>%
    dplyr::anti_join(removes)

  return(df)

}
