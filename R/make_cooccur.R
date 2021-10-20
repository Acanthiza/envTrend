
make_cooccur <- function(presences
                         , taxa_col = "taxa"
                         , num_col = "p"
                         , context
                         , min_taxa_count = 1
                         ) {

  presences %>%
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
    cooccur::cooccur(spp_names = TRUE
                     , thresh = FALSE
                     )

}
