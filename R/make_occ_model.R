


#' Make, and save to disk, an occupancy model.
#'
#' @param taxa Character name of taxa for which model is being run. Used to name
#' output file.
#' @param df Cleaned, filtered data frame.
#' @param out_file Path to file where model results are/will be saved.
#' @param geo_cols Character name of columns in `df` containing geographic
#' context. Last index in `geo_cols` is used as primary analysis level.
#' @param random_col Character name of column in `df` containing random factor
#' for model. This is usually the larger of two (probably raster) grid cell
#' sizes.
#' @param ... Passed to rstanarm::stan_gamm4 (e.g. chains, iter)
#'
#' @return `out_file`
#' @export
#'
#' @examples
make_occ_model <- function(taxa
                , df
                , out_file
                , geo_cols
                , random_col = "grid_l"
                , ...
                ) {

  print(taxa)

  dat_occ_prep <- df %>%
    dplyr::select(c(year
                    , any_of(geo_cols)
                    , any_of(random_col)
                    , success
                    )
                  ) %>%
    tidyr::nest(data = -c(year
                          , any_of(geo_cols)
                          )
                ) %>%
    dplyr::mutate(data = map(data, . %>%
                               dplyr::group_by(across(any_of(random_col))) %>%
                               dplyr::mutate(vis = row_number()) %>%
                               dplyr::ungroup() %>%
                               tidyr::pivot_wider(names_from = "vis"
                                                  , values_from = "success"
                                                  #, values_fill = 0
                                                  )
                             )
                  , trials = map_dbl(data
                                     , nrow
                                     )
                  ) %>%
    dplyr::filter(trials > 2) %>%
    dplyr::group_by(across(any_of(geo_cols))) %>%
    dplyr::mutate(years = n_distinct(year)) %>%
    dplyr::filter(years > 2)

  if(nrow(dat_occ_prep) > 2) {

    dat_occ <- dat_occ_prep %>%
      dplyr::mutate(umf = purrr::map(data
                                     , function(x) unmarked::unmarkedFrameOccu(y = x %>% dplyr::select(-1))
                                     )
                    , mod_year = purrr::map(umf
                                            , function(x) unmarked::occu(~1 ~1
                                                                         , data = x
                                                                         )
                                            )
                    , occ = purrr::map_dbl(mod_year
                                           , ~unmarked::backTransform(.
                                                                     , type = "state"
                                                                     )@estimate
                                           )
                    , occ = if_else(occ == 0, 0.00000001, occ)
                    , occ = if_else(occ == 1, 0.99999999, occ)
                    , det = purrr::map_dbl(mod_year
                                           , ~unmarked::backTransform(.
                                                                      , type = "det"
                                                                      )@estimate
                                           )
                    ) %>%
      dplyr::select(where(Negate(is.list))) %>%
      dplyr::group_by(across(any_of(geo_cols))) %>%
      dplyr::mutate(years = n_distinct(year)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(years > 3) %>%
      dplyr::mutate(across(where(is.character),factor))

    rio::export(dat_occ
                , gsub("mod", "dat", out_file)
                )

    geos <- dat_occ %>%
      dplyr::distinct(across(any_of(geo_cols))) %>%
      nrow()

    grid_cells <- dat_occ %>%
      dplyr::distinct(across(any_of(random_col))) %>%
      nrow()

    if(geos > 1) {

        mod <- rstanarm::stan_gamm4(as.formula(paste0("occ ~ "
                                           , "s(year, k = 4, bs = 'ts') +"
                                           , geo2
                                           , " + s(year, k = 4, by = "
                                           , geo2
                                           , ", bs = 'ts')"
                                           )
                                     )
                          , data = dat_occ
                          , family = mgcv::betar()
                          , random = if(grid_cells > 1) {

                            as.formula(paste0("~(1|"
                                              , random_col
                                              , ")"
                                              )
                                       )
                          } else NULL
                          , ...
                          )

      } else {

        mod <- rstanarm::stan_gamm4(as.formula(paste0("occ ~ "
                                            , "s(year, k = 4, bs = 'ts')"
                                            )
                                     )
                          , data = dat_occ
                          , family = mgcv::betar()
                          , random = if(grid_cells > 1) {

                            as.formula(paste0("~(1|"
                                              , random_col
                                              , ")"
                                              )
                                       )
                            } else NULL
                          , ...
                          )

      }

    rio::export(mod
                , out_file
                )

  } else print("not enough data")

}
