

#' Filter occurrence data to a set of specifications
#'
#' Works on results from `make_list_df`. Probably works on other dfs. All
#' thresholds are inclusive (i.e. if `max_year` = 2015, taxa with records in
#' 2015 will not be filtered).
#'
#' @param df Cleaned biological data.
#' @param var_cols Character name of column(s) in `df` that will be variables
#' in the model (or will define the model; e.g. `taxa_col`)
#' @param time_col Character name of column in `df` with time variable. Usually
#' `year`.
#' @param remove_0 Logical. Should success == 0 records be removed prior to
#' filtering? Default (`TRUE`) assumes that any success == 0 are implied rather
#' than representing true absences.
#' @param shortest_max Minimum allowable maximum length.
#' @param min_occurrences Minimum allowable occurrence.
#' @param min_years Minimum allowable years with occurrence.
#' @param max_year Must be records greater than `max_year` (this is the minimum
#' allowable maximum `time_col`).
#' @param min_year Must be records less than `min_year` (this is the maximum
#' allowable minimum `time_col`).
#' @param max_gap Maximum gap in `time_col`.
#'
#' @return Filtered data frame.
#' @export
#'
#' @examples
filter_list_df <- function(df
                           , var_cols = "taxa"
                           , time_col = "year"
                           , remove_0 = TRUE
                           , shortest_max = 3
                           , min_occurrences = 5
                           , min_years = 3
                           , max_year = as.numeric(format(Sys.Date(),"%Y")) - 5
                           , min_year = max_year - 10
                           , max_gap = 10
                           ) {


  # df <- bio_lists # helps with interactive testing

  # Remove zeroes?
  df_filt <- if(remove_0) {

    df %>%
      dplyr::filter(success > 0)

  } else df


  # Apply shortest_max
  shortest_max_remove <- df_filt %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::filter(list_length == max(list_length)) %>%
    dplyr::filter(list_length <= shortest_max) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))

  # Apply min_occurrences
  min_occurences_remove <- df_filt %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::summarise(occurences = n()) %>%
    dplyr::filter(occurences <= min_occurrences) %>%
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
    dplyr::filter(years <= min_years) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))


  # Apply max_year
  max_year_remove <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols))
                    , dplyr::across(tidyselect::any_of(time_col))
                    ) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::filter(!!rlang::ensym(time_col) == max(!!rlang::ensym(time_col))) %>%
    dplyr::filter(!!rlang::ensym(time_col) <= max_year) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))


  # Apply min_year
  min_year_remove <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols))
                    , dplyr::across(tidyselect::any_of(time_col))
                    ) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::filter(!!rlang::ensym(time_col) == min(!!rlang::ensym(time_col))) %>%
    dplyr::filter(!!rlang::ensym(time_col) >= min_year) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols)))

  # Apply max_gap
  max_gap_remove <- df_filt %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(var_cols))
                    , dplyr::across(tidyselect::any_of(time_col))
                    ) %>%
    dplyr::arrange(dplyr::across(tidyselect::any_of(c(var_cols, time_col)))) %>%
    dplyr::mutate(grouping = purrr::pmap_chr(dplyr::across(!! var_cols)
                                             , paste0
                                             )
                  ) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(var_cols))) %>%
    dplyr::mutate(time = !!rlang::ensym(time_col)
                  , min_time = min(time)
                  , gap = time - dplyr::lag(time
                                            , default = min(time)
                                            , order_by = grouping
                                            )
                  ) %>%
    dplyr::filter(gap >= max_gap) %>%
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
