

#' Filter occurrence data to a set of specifications
#'
#' Currently this is clunky, much room for improvement. Best run after
#' envTrend::add_list_length.
#'
#' @param df Cleaned biological data.
#' @param taxa_col Character name of column in `df` with taxa.
#' @param geo_levels Character name of column in `df` with geographic context(s)
#' of interest.
#' @param tax_levels Character name of column(s) in `df` with taxonomic
#' context(s) of interest.
#' @param time_levels Character name of column(s) in `df` with time context(s)
#' of interest.
#' @param min_length Minimum allowable list length.
#' @param shortest_max Minimum allowable maximum length.
#' @param min_occurrences Minimum allowable occurrence.
#' @param min_years Minimum allowable years with occurrence.
#' @param min_lengths Minimum allowable unique list lengths.
#' @param min_lists Minimum number of lists within a context defined by
#' `geo_levels`, `tax_levels` and `time_levels`.
#' @param min_year Must be records less than `min_year`.
#' @param max_year Much be records greater than `max_year`.
#'
#' @return Filtered data frame.
#' @export
#'
#' @examples
filter_list_df <- function(df
                           , taxa_col = "taxa"
                           , geo_levels
                           , tax_levels
                           , time_levels
                           , min_length = 3
                           , shortest_max = 3
                           , min_occurrences = 5
                           , min_years = 3
                           , min_lengths = 2
                           , min_lists = 3
                           , min_year = as.numeric(format(Sys.Date(),"%Y")) - 11
                           , max_year = as.numeric(format(Sys.Date(),"%Y")) - 1
                           ) {


  # df <- bio_lists

  # Apply min_length
  df <- df %>%
    dplyr::filter(list_length >= min_length)

  # Apply shortest_max
  temp <- df %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(tax_levels))) %>%
    dplyr::filter(list_length == max(list_length)) %>%
    dplyr::filter(list_length < shortest_max) %>%
    dplyr::ungroup()

  df <- df %>%
    dplyr::anti_join(temp)

  # Apply min_occurrences
  temp <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col))
                    , list
                    )  %>%
    dplyr::count(dplyr::across(tidyselect::any_of(taxa_col))
                 , name = "occurences"
                 ) %>%
    dplyr::filter(occurences >= min_occurrences)

  df <- df %>%
    dplyr::inner_join(temp)

  # Apply min_years
  temp <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col))
                    , dplyr::across(tidyselect::any_of(time_levels))
                    )  %>%
    dplyr::count(dplyr::across(tidyselect::any_of(taxa_col))
                 , name = "years"
                 ) %>%
    dplyr::filter(years >= min_years)

  df <- df %>%
    dplyr::inner_join(temp)

  # Apply min_lengths
  temp <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col))
                    , list_length
                    )  %>%
    dplyr::count(dplyr::across(tidyselect::any_of(taxa_col))
                 , name = "list_lengths"
                 ) %>%
    dplyr::filter(list_lengths >= min_lengths)

  df <- df %>%
    dplyr::inner_join(temp)

  # Find min_year
  temp <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(taxa_col))
                    , dplyr::across(tidyselect::any_of(tax_levels))
                    , dplyr::across(tidyselect::any_of(time_levels))
                    , dplyr::across(tidyselect::any_of(unname(geo_levels)))
                    )

  keep_low <- temp %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(taxa_col))
                    , dplyr::across(tidyselect::any_of(tax_levels))
                    , dplyr::across(tidyselect::any_of(unname(geo_levels)))
                    ) %>%
    dplyr::filter(year == min(year)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year < min_year) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(tax_levels))
                    , dplyr::across(tidyselect::any_of(unname(geo_levels)))
                    )

  # Find max_year
  keep_high <- temp %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(taxa_col))
                    , dplyr::across(tidyselect::any_of(tax_levels))
                    , dplyr::across(tidyselect::any_of(unname(geo_levels)))
                    ) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year > max_year) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(tax_levels))
                    , dplyr::across(tidyselect::any_of(unname(geo_levels)))
                    )

  df <- df %>%
    dplyr::inner_join(keep_low) %>%
    dplyr::inner_join(keep_high)

  return(df)

}
