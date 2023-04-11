

#' Make, and save to disk, a list-length corrected reporting rate model.
#'
#' @param df Cleaned, filtered data frame.
#' @param out_file Path to file where model results are/will be saved.
#' @param var_col Chraacter name of column in `df` containing main `x` variable.
#' @param cat_col Character name of column in `df` containing categorical data.
#' @param cov_col Character name of column in `df` containing secondary `x`
#' variable.
#' @param random_col Character name of column in `df` containing random factor
#' for model. Default is NULL, so no random factor in model.
#' @param log_cov Logical. Use `cov_col` (`T`) or the log of `cov_col` (`F`) in
#' model specification.
#' @param rand_slope Logical. Should a random slope effect be included in the
#' model as well as any random intercept?
#' @param mod_type Character. Currently "glm" or "gam".
#' @param k Integer used as `k` argument of `mgcv::s` for use in gam.
#' @param ... Passed to `rstanarm::stan_gamm4` (e.g. chains, iter)
#'
#' @return `out_file`. Named list containing inputs and model results as `mod`.
#' @export
#'
#' @examples
make_ll_model <- function(df
                          , mod_file
                          , var_col = "year"
                          , cat_col = "geo"
                          , cov_col = "list_length"
                          , random_col = NULL
                          , log_cov = TRUE
                          , rand_slope = TRUE
                          , mod_type = "glm"
                          , k = if(mod_type == "gam") 6 else NULL
                          , ...
                          ) {

  res <- c(as.list(environment()), list(...))

  res$data <- df
  res$df <- NULL

  if(!is.null(var_col)) df["var"] <- df[var_col]
  if(!is.null(cat_col)) df["cat"] <- df[cat_col]
  if(!is.null(random_col)) df["rand"] <- df[random_col]

  if(!is.null(cov_col)) {

    df["cov"] <- df[cov_col]
    df["log_cov"] <- log(df$cov)

  }

  df["use_cov"] <- if(log_cov) df$log_cov else df$cov

  cats <- df %>%
    dplyr::distinct(cat) %>%
    nrow()

  randoms <- if(!is.null(random_col)) {

    df %>%
      dplyr::distinct(rand) %>%
      nrow()

  } else 0


  message(paste0("Trying to run "
                 , mod_type
                 , " for "
                 , gsub("\\.rds", "", basename(mod_file))
                 )
          )

  # mod func------

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

    # gam spec-------

    if(cats > 1) {

      as.formula(paste0("cbind(success, trials - success) ~ "
                        , "s(var, k = "
                        , k
                        , ", bs = 'ts') + s(var, k = "
                        , k
                        , ", by = cat, bs = 'ts') + cat + use_cov + cat * use_cov"
                        )
                 )

      } else {

        as.formula(paste0("cbind(success, trials - success) ~ "
                          , "s(var, k = "
                          , k
                          , ", bs = 'ts') + use_cov"
                          )
                   )

      }

    # glm spec-------

    } else if (mod_type == "glm") {

      if(cats > 1) {

        if(randoms > 1) {

          if(rand_slope) {

            cbind(success, trials - success) ~ var * cat * use_cov + (var | rand)

          } else {

            cbind(success, trials - success) ~ var * cat * use_cov + (1 | rand)

          }

        } else {

          cbind(success, trials - success) ~ var * cat * use_cov

        }

      } else {

        if(randoms > 1) {

          if(rand_slope) {

            cbind(success, trials - success) ~ var * use_cov + (var | rand)

          } else {

            cbind(success, trials - success) ~ var * use_cov + (1 | rand)

          }

        } else {

          cbind(success, trials - success) ~ var * use_cov

        }

      }

    } else cbind(success, trials - success) ~ var


  # mod-----

  mod <- tryCatch(

    {

      if(mod_type == "gam") {

        if(randoms > 1) {

          if(rand_slope) {

            mod_func(formula = mod_spec
                     , data = df
                     , family = stats::binomial()
                     , random = ~ (var | rand)
                     , ...
                     )

          } else {

            mod_func(formula = mod_spec
                     , data = df
                     , family = stats::binomial()
                     , random = ~ (1 | rand)
                     , ...
                     )

          }

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
             , gsub("\\.rds", "", mod_file)
             , " gave error:"
             , message(cond)
             )

    }

  )


  # capture missing variables ------

  if(exists("mod")) {

    missing_names <- setdiff(names(df), names(mod$data))

    if(length(missing_names) > 0) {

      mod$data <- mod$data %>%
        dplyr::bind_cols(df %>%
                          dplyr::select(tidyselect::any_of(missing_names))
                        )

    }

    # finalise res-------

    res$randoms <- randoms
    res$cats <- cats
    res$mod <- mod

    rio::export(res
                , mod_file
                )

  }

  # clean up -------

  rm(list = ls())

  gc()

}
