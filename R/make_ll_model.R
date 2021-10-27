

#' Make, and save to disk, a list-length corrected reporting rate model.
#'
#' @param taxa Character name of taxa for which model is being run. Used to name
#' output file.
#' @param df Cleaned, filtered data frame.
#' @param geo_cols Character name of columns in `df` containing geographic
#' context. Last index in `geo_cols` is used as primary analysis level.
#' @param random_col Character name of column in `df` containing random factor
#' for model. This is usually the larger of two (probably raster) grid cell
#' sizes.
#' @param out_path Directory into which model results are saved.
#' @param ... Passed to rstanarm::stan_gamm4 (e.g. chains, iter)
#'
#' @return `fs::path(out_path,paste0("list-length_mod_",taxa,".rds"))`
#' @export
#'
#' @examples
make_ll_model <- function(taxa
                          , df
                          , geo_cols
                          , random_col = "grid_l"
                          , out_path
                          , ...
                          ) {

  print(taxa)

  out_file <- fs::path(out_path,paste0("list-length_mod_",taxa,".rds"))

  geos <- df %>%
    dplyr::distinct(across(any_of(geo_levels))) %>%
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
                                                  , ", bs = 'ts') +"
                                                  , "s(year, k = 4, by = log_list_length, bs = 'ts') + "
                                                  , geo2
                                                  , "+"
                                                  , geo2
                                                  , "*log_list_length"
                                                  )
                                           )
                                , data = df
                                , family = stats::binomial()
                                , random = as.formula(paste0("~(1|"
                                                             , random_col
                                                             , ")"
                                                             )
                                                      )
                                , ...
                                )

  } else {

    mod <- stan_gamm4(cbind(success,trials-success) ~ s(year, k = 4, bs = "ts") +
                        s(year, k = 4, by = log_list_length, bs = "ts") +
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

  write_rds(mod,out_file)

}