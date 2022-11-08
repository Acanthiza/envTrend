

#' Make, and save to disk, a list-length corrected reporting rate model.
#'
#' @param df Cleaned, filtered data frame.
#' @param out_file Path to file where model results are/will be saved.
#' @param geo_col Character name of columns in `df` containing geographic
#' context.
#' @param random_col Character name of column in `df` containing random factor
#' for model. This is usually the larger of two (probably raster) grid cell
#' sizes. Default is NULL, so no random factor in model.
#' @param ... Passed to rstanarm::stan_gamm4 (e.g. chains, iter)
#'
#' @return `out_file`
#' @export
#'
#' @examples
make_ll_model <- function(df
                          , out_file
                          , geo_col
                          , random_col = NULL
                          , ...
                          ) {

  print(basename(out_file))

  geos <- df %>%
    #dplyr::select(!tidyselect::any_of(unname(random_col))) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(unname(geo_col)))) %>%
    nrow()

  randoms <- df %>%
    dplyr::distinct(across(any_of(random_col))) %>%
    nrow()

  # GAM
  if(geos > 1) {

    mod <- rstanarm::stan_gamm4(as.formula(paste0("cbind(success,trials - success) ~ "
                                                  , "s(year, k = 4, bs = 'ts') +"
                                                  , "s(year, k = 4, by = "
                                                  , geo_col
                                                  , ", bs = 'ts') + "
                                                  , geo_col
                                                  , "+"
                                                  , "log_list_length +"
                                                  , geo_col
                                                  , "*log_list_length"
                                                  )
                                           )

                                , data = df
                                , family = stats::binomial()
                                , random = if(randoms > 1) {

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
                                , random = if(randoms > 1) {

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
