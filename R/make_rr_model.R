

#' Make, and save to disk, a reporting rate model.
#'
#' @param taxa Character name of taxa for which model is being run. Used to name
#' output file.
#' @param df Cleaned, filtered data frame.
#' @param geo_cols Character name of columns in `df` containing geographic
#' context. Last index in `geo_cols` is used as primary analysis level.
#' @param cell_cols Character name of columns in `df` containing two scales of
#' (probably raster) grid cells. The first index is the larger cell, second
#' index is smaller.  The larger cell is used as a random factor in the model,
#' if there is more than one level.
#' @param out_path Path into which model results are saved.
#' @param ... Passed to rstanarm::stan_gamm4 (e.g. chains, iter)
#'
#' @return `fs::path(out_path,paste0("reporting-rate_mod_",taxa,".rds"))`
#' @export
#'
#' @examples
make_rr_model <- function(taxa
               , df
               , geo_cols
               , cell_cols = c("grid_l", "grid_s")
               , out_path
               , ...
               ) {

  print(taxa)

  out_file <- fs::path(out_path,paste0("reporting-rate_mod_",taxa,".rds"))

  geos <- df %>%
    dplyr::distinct(across(any_of(geo_levels))) %>%
    nrow()

  grid_cells <- df %>%
    dplyr::distinct(across(cell_cols)) %>%
    nrow()

  geo2 <- geo_cols[2]

  grid_l <- cell_cols[1]

  # GAM
  if(geos > 1) {

    mod <- rstanarm::stan_gamm4(as.formula(paste0("cbind(success,trials - success) ~ "
                                        , "s(year, k = 4, bs = 'ts') +"
                                        , geo2
                                        , " + s(year, k = 4, by = "
                                        , geo2
                                        , ", bs = 'ts')"
                                        )
                                 )
                      , data = df
                      , family = stats::binomial()
                      , random = ~(1|grid_l)
                      , ...
                      )

  } else {

    mod <- stan_gamm4(cbind(success,trials-success) ~ s(year, k = 4, bs = "ts")
                      , data = df
                      , random = if(grid_cells > 1) formula(~ (1|grid_l)) else NULL
                      , family = stats::binomial()
                      , ...
                      )

    }

  rio::export(mod
              , out_file
              )

}
