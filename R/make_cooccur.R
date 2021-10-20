
#' Find taxa pairwise co-occurrence patterns
#'
#' @param pres_df Dataframe with presence records, along with `taxa_col` and
#' `context`(s)
#' @param taxa_col Character name of columns in `pres_df` with taxa.
#' @param context Character name of column(s) in `pres_df` that define the
#' context.'
#' @param ... Passed to `cooccur::cooccur()`
#'
#' @return a list of class `cooccur`
#' @export
#'
#' @examples
make_cooccur <- function(pres_df
                         , taxa_col = "taxa"
                         , context
                         , ...
                         ) {

  pres_df %>%
    dplyr::add_count(dplyr::across(tidyselect::any_of(taxa_col))) %>%
    dplyr::filter(n > min_taxa_count) %>%
    dplyr::distinct(across(all_of(taxa_col))
                    , across(any_of(context))
                    ) %>%
    dplyr::mutate(p = 1) %>%
    tidyr::pivot_wider(names_from = any_of(context)
                       , values_from = p
                       , values_fill = 0
                       ) %>%
    tibble::column_to_rownames(taxa_col) %>%
    as.matrix() %>%
    cooccur::cooccur(...)

}
