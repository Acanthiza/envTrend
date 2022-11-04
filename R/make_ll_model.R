

#' Make, and save to disk, a list-length corrected reporting rate model.
#'
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
make_ll_model <- function(df
                          , out_file
                          , geo_cols
                          , random_col = "grid_l"
                          , ...
                          ) {

  print(basename(out_file))

  geos <- df %>%
    dplyr::distinct(across(any_of(geo_cols))) %>%
    nrow()

  grid_cells <- df %>%
    dplyr::distinct(across(any_of(random_col))) %>%
    nrow()

  geo2 <- geo_cols[length(geo_cols)]

  # GAM
  if(geos > 1) {

    mod <- rstanarm::stan_gamm4(as.formula(paste0("cbind(success,trials - success) ~ "
                                                  , "s(year, k = 4, bs = 'ts') +"
                                                  , "s(year, k = 4, by = "
                                                  , geo2
                                                  , ", bs = 'ts') + "
                                                  , geo2
                                                  , "+"
                                                  , "log_list_length +"
                                                  , geo2
                                                  , "*log_list_length"
                                                  )
                                           )

                                , data = df
                                , family = stats::binomial()
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

    mod <- rstanarm::stan_gamm4(cbind(success,trials-success) ~ s(year, k = 4, bs = "ts") +
                                  log_list_length
                                , data = df
                                , random = if(grid_cells > 1) {

                                  as.formula(paste0("~(1|"
                                          , random_col
                                          , ")"
                                          )
                                   )

                        } else NULL
                      , family = stats::binomial()
                      , ...
                      )

  }

  rio::export(mod,out_file)

  rm(mod)

  gc()

}
