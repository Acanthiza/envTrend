

#' Filter data to a set of specifications
#'
#' Currently this feels messy, much room for improvement.
#'
#' @param df Cleaned biological data.
#' @param taxa_col Character name of column in `df` with taxa.
#' @param geo_level Character name of column in `df` with geographic scale(s) of
#' interest
#' @param tax_level Character name of column(s) in `df` with taxonomic scale(s)
#' of interest
#' @param minListLengthThresh Minimum list length allowed
#' @param maxListLengthOccurenceThresh Maximum
#' @param minlistOccurenceThresh
#' @param minYearsThresh
#' @param minListLengthsThresh
#' @param minYearSpanThresh
#'
#' @return
#' @export
#'
#' @examples
create_list_df <- function(df
                           , taxa_col = "taxa"
                           , geo_level
                           , tax_level
                           , minListLengthThresh = 3
                           , maxListLengthOccurenceThresh = 3
                           , minlistOccurenceThresh = 5
                           , minYearsThresh = 3
                           , minListLengthsThresh = 2
                           , minYearSpanThresh = 20
                           ) {

  find_min_list_length <- function(df) {

    min(df$list_length)

  }

  find_max_list_length_occurence <- function(df) {

    df %>%
      dplyr::count(dplyr::across(tidyselect::any_of(geo_level))
                   , taxa
                   , list_length
                   , name = "lists"
                   ) %>%
      dplyr::group_by(across(tidyselect::any_of(taxa_col))) %>%
      dplyr::filter(list_length == max(list_length)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(list_length == min(list_length)) %>%
      dplyr::pull(list_length) %>%
      unique()

  }

  find_min_list_occurence <- function(df) {

    df %>%
      dplyr::count(across(tidyselect::any_of(geo_level))
                   , across(tidyselect::any_of(tax_level))
                   , taxa,name = "lists"
                   ) %>%
      dplyr::filter(lists == min(lists)) %>%
      dplyr::pull(lists) %>%
      unique()

  }

  find_min_years <- function(df) {

    df %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , tax_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , year
                   , name = "blah"
                   ) %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , tax_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , name = "years"
                   ) %>%
      dplyr::filter(years == min(years)) %>%
      dplyr::pull(years) %>%
      unique()

  }

  find_min_list_lengths <- function(df) {

    df %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , list_length
                   , name = "blah"
                   ) %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , name = "lengths"
                   ) %>%
      dplyr::filter(lengths == min(lengths)) %>%
      dplyr::pull(lengths) %>%
      unique()

  }

  find_min_year_span <-function(df) {

    df %>%
      dplyr::group_by(across(tidyselect::any_of(c(tax_level
                                                  , geo_level
                                                  , taxa_col
                                                  )
                                                )
                             )
                      ) %>%
      dplyr::summarise(minYear = min(year)
                       , maxYear = max(year)
                       , diffYear = maxYear - minYear
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::pull(diffYear) %>%
      min() %>%
      unique()

  }

  minListLength <- find_min_list_length(df)
  maxListLengthOccurence <- find_max_list_length_occurence(df)
  minlistOccurence <- find_min_list_occurence(df)
  minYears <- find_min_years(df)
  minLengths <- find_min_list_lengths(df)
  minYearSpan <- find_min_year_span(df)

  while(minListLength < minListLengthThresh |
        maxListLengthOccurence < maxListLengthOccurenceThresh |
        minlistOccurence < minlistOccurenceThresh |
        minYears < minYearsThresh |
        minLengths < minListLengthsThresh |
        minYearSpan < minYearSpanThresh
        ) {

    df <- df %>%
      dplyr::filter(list_length > minListLengthThresh)

    removeTaxaOnShortLists <- df %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , tax_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , list_length
                   , name = "lists"
                   ) %>%
      dplyr::group_by(across(tidyselect::any_of(taxa_col))) %>%
      dplyr::filter(list_length == max(list_length)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(list_length < maxListLengthOccurenceThresh) %>%
      dplyr::distinct(across(tidyselect::any_of(c(geo_level
                                                  , tax_level
                                                  , taxa_col
                                                  )
                                                )
                             )
                      )

    removeTaxaWithFewOccurrences <- df %>%
      dplyr::anti_join(removeTaxaOnShortLists) %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , tax_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , name = "lists"
                   ) %>%
      dplyr::filter(lists < minlistOccurenceThresh) %>%
      dplyr::distinct(across(tidyselect::any_of(c(geo_level
                                                  , tax_level
                                                  , taxa_col
                                                  )
                                                )
                             )
                      )

    removeTaxaWithFewYears <- df %>%
      dplyr::anti_join(removeTaxaOnShortLists) %>%
      dplyr::anti_join(removeTaxaWithFewOccurrences) %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , tax_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , year
                   , name = "blah"
                   ) %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , tax_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , name = "years"
                   ) %>%
      dplyr::filter(years < minYearsThresh) %>%
      dplyr::distinct(across(tidyselect::any_of(c(geo_level
                                                  , tax_level
                                                  , taxa_col
                                                  )
                                                )
                             )
                      )

    removeTaxaWithFewLengths <- df %>%
      dplyr::anti_join(removeTaxaOnShortLists) %>%
      dplyr::anti_join(removeTaxaWithFewOccurrences) %>%
      dplyr::anti_join(removeTaxaWithFewYears) %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , list_length
                   , name = "blah"
                   ) %>%
      dplyr::count(across(tidyselect::any_of(c(geo_level
                                               , taxa_col
                                               )
                                             )
                          )
                   , name = "lengths"
                   ) %>%
      dplyr::filter(lengths < minListLengthsThresh) %>%
      dplyr::distinct(across(tidyselect::any_of(c(geo_level
                                                  , taxa_col
                                                  )
                                                )
                             )
                      )

    removeTooFewYears <- df %>%
      dplyr::anti_join(removeTaxaOnShortLists) %>%
      dplyr::anti_join(removeTaxaWithFewOccurrences) %>%
      dplyr::anti_join(removeTaxaWithFewYears) %>%
      dplyr::anti_join(removeTaxaWithFewLengths) %>%
      dplyr::group_by(across(tidyselect::any_of(c(tax_level
                                                  , geo_level
                                                  , taxa_col
                                                  )
                                                )
                             )
                      ) %>%
      dplyr::summarise(minYear = min(year)
                       , maxYear = max(year)
                       , diffYear = maxYear - minYear
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(diffYear < minYearSpanThresh) %>%
      dplyr::distinct(across(tidyselect::any_of(c(tax_level
                                                  , taxa_col
                                                  , geo_level
                                                  )
                                                )
                             )
                      )


    df <- df %>%
      dplyr::anti_join(removeTaxaOnShortLists) %>%
      dplyr::anti_join(removeTaxaWithFewOccurrences) %>%
      dplyr::anti_join(removeTaxaWithFewYears) %>%
      dplyr::anti_join(removeTaxaWithFewLengths) %>%
      dplyr::anti_join(removeTooFewYears) %>%
      dplyr::add_count(list, name = "list_length")

    minListLength <- find_min_list_length(df)
    maxListLengthOccurence <- find_max_list_length_occurence(df)
    minlistOccurence <- find_min_list_occurence(df)
    minYears <- find_min_years(df)
    minLengths <- find_min_list_lengths(df)
    minYearSpan <- find_min_year_span(df)

    res <- paste0("Minimum list length = ",minListLength
                  ,"\nMaximum list taxa occurs on = ",maxListLengthOccurence
                  ,"\nMinimum list occurence = ",minlistOccurence
                  ,"\nMinimum years = ",minYears
                  ,"\nMinimum list lengths = ",minLengths
                  ,"\nMinimum year span = ",minYearSpan
                  ,"\nTotal records = ",nrow(df)
                  ,"\n"
                  )

    cat(res)

  }

  return(df)

}
