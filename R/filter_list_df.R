

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
                           , min_occurrence = 5
                           , min_years = 3
                           , min_lengths = 2
                           , min_year = as.numeric(format(Sys.Date(),"%Y")) - 11
                           , max_year = as.numeric(format(Sys.Date(),"%Y")) - 1
                           ) {

  temp <- df %>%
    dplyr::group_by(across(any_of(c(taxa_col, tax_levels, geo_levels)))) %>%
    dplyr::summarise(min_list_length = min(list_length)
                     , max_list_length = max(list_length)
                     , list_occ = n_distinct(list)
                     , min_list_year = min(year)
                     , max_list_year = max(year)
                     , years = n_distinct(year)
                     , lengths = n_distinct(list_length)
                     , records = n()
                     ) %>%
    dplyr::ungroup()

  keep <- temp %>%
    dplyr::filter(min_list_length >= min_length
                  , max_list_length >= shortest_max
                  , list_occ >= min_occurrence
                  , years >= min_years
                  , lengths >= min_lengths
                  , min_list_year <= min_year
                  , max_list_year >= max_year
                  )

  res <- df %>%
    dplyr::inner_join(keep) %>%
    dplyr::select(names(df))

}
