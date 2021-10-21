

#' Filter occurrence data to a set of specifications
#'
#' Currently this is clunky, much room for improvement.
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
#' @param min_occurrence Minimum allowable occurrence.
#' @param min_years Minimum allowable years with occurrence.
#' @param min_lengths Minimum allowable unique list lengths.
#' @param min_span Minimum allowable span of years.
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
                           , min_occurrence = 5
                           , min_years = 3
                           , min_lengths = 2
                           , min_span = 20
                           ) {

  full_context <- c(taxa_col, geo_level, tax_level, time_levels)

  filt_min_length <- function(df
                              , ret = c("num","df")
                              ) {

    if(ret == "num") {

      min(df$list_length)

    } else if(ret == "df") {

      df %>%
        dplyr::filter(list_length > min_length)

    }

  }

  filt_shortest_max <- function(df
                                , ret = c("num","df")
                                ) {

    temp <- df %>%
      dplyr::group_by(across(tidyselect::any_of(full_context))) %>%
      dplyr::filter(list_length == max(list_length)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(list_length < min(shortest_max))

    if(ret == "num") {

      temp %>%
        dplyr::pull(list_length) %>%
        min()

    } else {

      df %>%
        dplyr::anti_join(temp)

    }

  }

  filt_min_occurrence <- function(df
                                  , ret = c("num","df")
                                  ) {

    temp <- df %>%
      dplyr::count(across(any_of(full_context))
                   , name = "lists"
                   ) %>%
      dplyr::filter(lists < min_occurrence)

    if(ret == "num") {

      temp %>%
        dplyr::pull(lists) %>%
        min()

    } else {

      df %>%
        dplyr::anti_join(temp)

    }

  }

  filt_min_years <- function(df
                             , ret = c("num","df")
                             ) {

    temp <- df %>%
      dplyr::count(across(tidyselect::any_of(full_context))
                   , name = "blah"
                   ) %>%
      dplyr::count(across(tidyselect::any_of(full_context[!full_context %in% time_cols]))
                   , name = "years"
                   ) %>%
      dplyr::filter(years < min_years)

    if(ret == "num") {

      temp %>%
        dplyr::pull(years) %>%
        min()

    } else {

      df %>%
        dplyr::anti_join(temp)

    }

  }

  filt_min_lengths <- function(df
                               , ret = c("num","df")
                               ) {

    temp <- df %>%
      dplyr::count(across(tidyselect::any_of(full_context))
                   , list_length
                   , name = "blah"
                   ) %>%
      dplyr::count(across(tidyselect::any_of(full_context))
                   , name = "lengths"
                   ) %>%
      dplyr::filter(lengths < min_lengths)

    if(ret == "num") {

      temp %>%
        dplyr::pull(lengths) %>%
        min()

    } else {

      df %>%
        dplyr::anti_join(temp)

    }

  }

  filt_min_span <-function(df
                           , ret = c("num","df")
                           ) {

    temp <- df %>%
      dplyr::group_by(across(tidyselect::any_of(full_context[!full_context %in% time_cols]))) %>%
      dplyr::summarise(min_year = min(year)
                       , max_year = max(year)
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(span = max_year - min_year) %>%
      dplyr::filter(span < min_span)

    if(ret == "num") {

      temp %>%
        dplyr::pull(span) %>%
        min()

    } else {

      df %>%
        dplyr::anti_join(temp)

    }

  }

  min_list_length <- filt_min_length(df, ret = "num")
  min_list_max_length <- filt_shortest_max(df, ret = "num")
  min_list_occurrence <- filt_min_occurrence(df, ret = "num")
  min_list_years <- filt_min_years(df, ret = "num")
  min_list_lengths <- filt_min_lengths(df, ret = "num")
  min_list_span <- filt_min_span(df, ret = "num")

  while(min_list_length < min_length |
        min_list_max_length < shortest_max |
        min_list_occurrence < min_occurrence |
        min_list_years < min_years |
        min_list_lengths < min_lengths |
        min_list_span < min_span
        ) {

    df <- df %>%
      filt_min_length(ret = "df") %>%
      filt_shortest_max(ret = "df") %>%
      filt_min_occurrence(ret = "df") %>%
      filt_min_years(ret = "df") %>%
      filt_min_lengths(ret = "df") %>%
      filt_min_span(ret = "df") %>%
      dplyr::add_count(list, name = "list_length")

    min_list_length <- filt_min_length(df, ret = "num")
    min_list_max_length <- filt_shortest_max(df, ret = "num")
    min_list_occurrence <- filt_min_occurrence(df, ret = "num")
    min_list_years <- filt_min_years(df, ret = "num")
    min_list_lengths <- filt_min_lengths(df, ret = "num")
    min_list_span <- filt_min_span(df, ret = "num")

    temp <- paste0("Minimum length = ",min_list_length
                  ,"\nMinimum of maximum length = ",min_list_max_length
                  ,"\nMinimum occurrences = ",min_list_occurrence
                  ,"\nMinimum years = ",min_list_years
                  ,"\nMinimum unique lengths = ",min_list_lengths
                  ,"\nMinimum year span = ",min_list_span
                  ,"\nTotal records = ",nrow(df)
                  ,"\n"
                  )

    cat(temp)

  }

  return(df)

}
