

#' Make, and save to disk, a list-length corrected reporting rate model.
#'
#' @param df Cleaned, filtered data frame.
#' @param out_file Path to file where model results are/will be saved.
#' @param geo_col Character name of columns in `df` containing geographic
#' context.
#' @param random_col Character name of column in `df` containing random factor
#' for model. This is usually the larger of two (probably raster) grid cell
#' sizes. Default is NULL, so no random factor in model.
#' @param mod_type Character. Currently "glm" or "gam".
#' @param k Integer used as `k` argument of `mgcv::s` for use in gam.
#' @param ... Passed to `rstanarm::stan_gamm4` (e.g. chains, iter)
#'
#' @return `out_file`
#' @export
#'
#' @examples
make_ll_model <- function(df
                          , out_file
                          , geo_col
                          , random_col = NULL
                          , mod_type = "glm"
                          , k = if(mod_type == "gam") 6 else NULL
                          , ...
                          ) {

  geos <- df %>%
    #dplyr::select(!tidyselect::any_of(unname(random_col))) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(unname(geo_col)))) %>%
    nrow()

  randoms <- df %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(random_col))) %>%
    nrow()

  taxa_name <- gsub("\\..*$"
                    , ""
                    , basename(out_file)
                    )

  message(paste0("Trying to run "
                 , mod_type
                 , " for "
                 , taxa_name
                 )
          )

  mod_func <- if(mod_type == "gam") {

    rstanarm::stan_gamm4

  } else if(mod_type == "glm") {

    if(geos > 1) {

      rstanarm::stan_glmer

    } else {

      rstanarm::stan_glm

    }

  }

  mod_spec <- if(mod_type == "gam") {

    if(geos > 1) {

      as.formula(paste0("cbind(success,trials - success) ~ "
                        , "s(year, k = "
                        , k
                        , ", bs = 'ts') +"
                        , "s(year, k = "
                        , k
                        , ", by = "
                        , geo_col
                        , ", bs = 'ts') + "
                        , geo_col
                        , "+"
                        , "log_list_length +"
                        , geo_col
                        , "*log_list_length"
                        )
                 )

      } else {

        as.formula(paste0("cbind(success,trials - success) ~ "
                          , "s(year, k = 4, bs = 'ts') +"
                          , "log_list_length"
                          )
                   )

      }

    } else {

      if(mod_type == "glm") {

        if(geos > 1) {

          as.formula(paste0("cbind(success, trials - success) ~ year * "
                            , geo_col
                            , " * log_list_length"
                            # random intercept and slope (gam does intercept only)
                            , " + (year | "
                            , random_col
                            , ")"
                            )
                     )

          } else {

            as.formula(paste0("cbind(success, trials - success) ~ year * "
                              , geo_col
                              , " * log_list_length"
                              )
                       )

            }

        }

      }


  mod <- tryCatch(

    {

      mod_func(formula = mod_spec
               , data = df
               , family = stats::binomial()
               , if(mod_type == "gam") {

                 random = if(randoms > 1) {

                   # random intercept not slope (glm does random slope too)
                   as.formula(paste0("~(1 | "
                                     , random_col
                                     , ")"
                                     )
                              )

                   } else NULL

               }
               , ...
               )

    }

    , error = function(cond) {

      paste0(mod_type
             , " for "
             , taxa_name
             , " gave error:"
             , message(cond)
             )

    }

  )

  if(exists("mod")) {

    missing_names <- setdiff(names(df), names(mod$data))

    if(length(missing_names) > 0) {

      mod$data <- mod$data %>%
        dplyr::bind_cols(df %>%
                          dplyr::select(tidyselect::any_of(missing_names))
                        )

    }

    rio::export(mod
                , out_file
                )

  }

  rm(list = ls())

  gc()

}
