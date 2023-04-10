

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
#' @return `out_file`. Named list containing inputs and model results as `mod`.
#' @export
#'
#' @examples
make_ll_model <- function(df
                          , out_file
                          , time_col = "year"
                          , geo_col = "geo"
                          , random_col = NULL
                          , mod_type = "glm"
                          , k = if(mod_type == "gam") 6 else NULL
                          , ...
                          ) {

  res <- list()

  if(!is.null(time_col)) df["time"] <- df[time_col]
  if(!is.null(geo_col)) df["geo"] <- df[geo_col]
  if(!is.null(random_col)) df["rand"] <- df[random_col]

  geos <- df %>%
    dplyr::distinct(geo) %>%
    nrow()

  if(!is.null(random_col)) {

    randoms <- df %>%
      dplyr::distinct(rand) %>%
      nrow()

  } else randoms <- 0


  message(paste0("Trying to run "
                 , mod_type
                 , " for "
                 , gsub("\\.rds", "", basename(out_file))
                 )
          )

  mod_func <- if(mod_type == "gam") {

    rstanarm::stan_gamm4

  } else if(mod_type == "glm") {

    if(randoms > 1) {

      rstanarm::stan_glmer

    } else {

      rstanarm::stan_glm

    }

  }

  mod_spec <- if(mod_type == "gam") {

    if(geos > 1) {

      as.formula(paste0("cbind(success,trials - success) ~ "
                        , "s(time, k = "
                        , k
                        , ", bs = 'ts') + s(time, k = "
                        , k
                        , ", by = geo, bs = 'ts') + geo + log_list_length + geo * log_list_length"
                        )
                 )

      } else {

        as.formula(paste0("cbind(success,trials - success) ~ "
                          , "s(time, k = "
                          , k
                          , ", bs = 'ts') + log_list_length"
                          )
                   )

      }

    } else if (mod_type == "glm") {

      if(geos > 1) {

        if(randoms > 1) {

          cbind(success, trials - success) ~ year * geo * log_list_length + (time | rand)

        } else {

          cbind(success, trials - success) ~ year * geo * log_list_length

        }

      } else {

        if(randoms > 1) {

          cbind(success, trials - success) ~ year * log_list_length + (time | rand)

        } else {

          cbind(success, trials - success) ~ year * log_list_length

        }

      }

    } else "cbind(success, trials - success) ~ year"


  mod <- tryCatch(

    {

      if(mod_type == "gam") {

        if(randoms > 1) {

          mod_func(formula = mod_spec
                   , data = df
                   , family = stats::binomial()
                   , random = ~ (time | rand)
                   , ...
                   )

        } else {

          mod_func(formula = mod_spec
                   , data = df
                   , family = stats::binomial()
                   , ...
                   )

        }

      } else if(mod_type == "glm") {

        mod_func(formula = mod_spec
                 , data = df
                 , family = stats::binomial()
                 , ...
                 )

      }

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

    res$random_col <- random_col
    res$randoms <- randoms

    res$geo_col <- geo_col
    res$geos <- geos

    res$time_col <- time_col

    res$mod <- mod

    rio::export(res
                , out_file
                )

  }

  rm(list = ls())

  gc()

}
