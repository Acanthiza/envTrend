

#' Run, and save to disk, a list-length corrected reporting rate model.
#'
#' Code is now generic enough to take a continuous variable (usually `year`)
#' plus any combination of: another continuous variable (usually `list_length`),
#' a categorical variable (often a geographic context) and/or a random variable
#' (often a taxonomic or geographic context).
#'
#' @param df Cleaned, filtered data frame.
#' @param mod_file Path to file where model results are/will be saved.
#' @param force_new Logical. If `mod_file` exists, should the model be re-run
#' (and the previous results overwritten)?
#' @param y Character name of the column in `df` containing the `y` variable.
#' @param y_total For binomial models only. Character name of the column in `df`
#' containing success + failure.
#' @param var_col Character name of column in `df` containing main `x` variable.
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
#' @param k For `mod_type = "gam"` only. Integer used as `k` argument of
#' `mgcv::s()`.
#' @param ... Passed to `rstanarm::stan_gamm4` (e.g. chains, iter)
#'
#' @return `out_file`. `.rds` file containing named list of inputs and model
#' results as `mod`.
#' @export
#'
#' @examples
make_ll_model <- function(df
                          , mod_file
                          , force_new = FALSE
                          , y
                          , y_total = NULL
                          , var_col = "var"
                          , cat_col = "cat"
                          , cov_col = "cov"
                          , random_col = NULL
                          , log_cov = TRUE
                          , rand_slope = TRUE
                          , mod_type = "glm"
                          , k = if(mod_type == "gam") 6 else NULL
                          , ...
                          ) {

  run_model <- if(!file.exists(mod_file)) TRUE else force_new

  if(run_model) {

    res <- c(as.list(environment())
             , list(...)
             )

    res$df <- NULL

    df["y"] <- df[y]
    if(!is.null(y_total)) df["y_total"] <- df[y_total]
    if(!is.null(var_col)) df["var"] <- df[var_col] - median(df[var_col][[1]])
    if(!is.null(cat_col)) df["cat"] <- df[cat_col]
    if(!is.null(random_col)) df["rand"] <- df[random_col]

    if(!is.null(cov_col)) {

      df["cov"] <- df[cov_col]
      df["log_cov"] <- log(df["cov"])

      df["use_cov"] <- if(log_cov) df["log_cov"] else df["cov"]

    }

    covs <- if(!is.null(cov_col)) {

      df %>%
        dplyr::distinct(use_cov) %>%
        nrow()

    } else 0

    cats <- if(!is.null(cat_col)) {

      df %>%
        dplyr::distinct(cat) %>%
        nrow()

    } else 0

    rands <- if(!is.null(random_col)) {

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

      if(rands > 1) {

        rstanarm::stan_glmer

      } else {

        rstanarm::stan_glm

      }

    }

    # y-------

    y_spec <- if(!is.null(y_total)) {

      paste0("cbind(y, y_total - y)")

    } else "y"

    if(exists("family", res)) {

      if(grepl("Beta", res$family$family)) {

        df <- df %>%
          dplyr::mutate(y = dplyr::case_when(y == 0 ~ 0.000001
                                             , y == 1 ~ 0.999999
                                             , TRUE ~ y
                                             )
                        )

      }

    }

    mod_spec <- if(mod_type == "gam") {

      # gam spec-------

      paste0(y_spec
             , " ~ "
             , "s(var, k = "
             , k
             , ", bs = 'ts')"
             , if(covs > 1) " + use_cov"
             , if(cats > 1) paste0(" + s(var, k = "
                                   , k
                                   , ", by = cat, bs = 'ts') + cat"
                                   )
             )

      # glm spec-------

      } else if (mod_type == "glm") {

        paste0(y_spec
               , " ~ "
               , "var"
               , if(covs > 1) " + use_cov"
               , if(cats > 1) " + cat + var * cat"
               , if(rands > 1) paste0(" + ("
                                        , if(rand_slope) "var" else 1
                                        , " | rand)"
                                        )
               )

      } else paste0(y_spec, " ~ var")

    mod_spec <- as.formula(mod_spec)


    # mod-----

    mod <- tryCatch(

      {

        if(mod_type == "gam") {

          if(rands > 1) {

            if(rand_slope) {

              mod_func(formula = mod_spec
                       , data = df
                       , random = ~ (var | rand)
                       , ...
                       )

            } else {

              mod_func(formula = mod_spec
                       , data = df
                       , random = ~ (1 | rand)
                       , ...
                       )

            }

          } else {

            mod_func(formula = mod_spec
                     , data = df
                     , ...
                     )

          }

        } else if(mod_type == "glm") {

          mod_func(formula = mod_spec
                   , data = df
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


    if(exists("mod")) {

      # capture missing variables ------
      missing_names <- setdiff(names(df), names(mod$data))

      if(length(missing_names) > 0) {

        mod$data <- mod$data %>%
          dplyr::bind_cols(df %>%
                            dplyr::select(tidyselect::any_of(missing_names))
                          )

      }

      # finalise res-------

      res$covs <- covs
      res$rands <- rands
      res$cats <- cats

      res$df <- df
      res$mod <- mod


      # export ------
      rio::export(res
                  , mod_file
                  )

    }

    # clean up -------

    rm(list = ls())

    gc()

  }

  return(invisible(NULL))

}
