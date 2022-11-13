

#' Make, and save to disk, a list-length corrected reporting rate model.
#'
#' @param df Cleaned, filtered data frame.
#' @param out_file Path to file where model results are/will be saved.
#' @param geo_col Character name of columns in `df` containing geographic
#' context.
#' @param random_col Character name of column in `df` containing random factor
#' for model. This is usually the larger of two (probably raster) grid cell
#' sizes. Default is NULL, so no random factor in model.
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
                          , ...
                          ) {

  geos <- df %>%
    #dplyr::select(!tidyselect::any_of(unname(random_col))) %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(unname(geo_col)))) %>%
    nrow()

  randoms <- df %>%
    dplyr::distinct(across(any_of(random_col))) %>%
    nrow()

  taxa_name <- gsub("\\..*$", "", basename(out_file))

  message(paste0("Trying to run rstanarm::stan_gamm4 for "
                 , taxa_name
                 )
          )

  mod <- tryCatch(

    {

      # GAM
      if(geos > 1) {

        rstanarm::stan_gamm4(as.formula(paste0("cbind(success,trials - success) ~ "
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

        rstanarm::stan_gamm4(as.formula(paste0("cbind(success,trials - success) ~ "
                                               , "s(year, k = 4, bs = 'ts') +"
                                               , "log_list_length "
                                               )
                                        )
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

    }

    , error = function(cond) {

      message(paste0("rstanarm::stan_gamm4 for "
                     , taxa_name
                     , " gave error:"
                     )
              )

      message(cond)

    }

    , finally = {

      "Hopefully the other code will keep running...."

    }

  )

  if(exists("mod")) {

    rio::export(mod,out_file)

    message("successfully saved rstanarm::stan_gamm4 model to "
            , out_file
            )

  }

  rm(list = ls())

  gc()

}
