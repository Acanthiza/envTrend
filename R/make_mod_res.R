

#' Summarise results from `make_ll_model`
#'
#' @param mod_file Character. Path to saved results from `make_ll_model`
#' @param ref Numeric. Single value of `var_col` from `make_ll_model` to use as
#' a reference value or a negative integer to compare all values of `var_col`
#' against `var_col + ref`.
#' @param pred_step Numeric. What step (in units of `var_col`) to predict at?
#' @param cov_q Numeric. What list lengths quantiles to predict at?
#' @param res_q Numeric. What quantiles to summarise predictions at?
#' @param do_gc Logical. Run `base::gc` after predict? On a server with shared
#' resources, can be necessary when summarising many, many models to prevent
#' RAM issues.
#' @param ... Passed to `tidybayes::add_epred_draws()`
#'
#' @return A list with components
#' \describe{
#'   \item{list_length}{data frame of list length quantiles in original data}
#'   \item{data}{data frame of original data retrieved from `mod$data`}
#'   \item{pred}{data frame of predictions (via
#'   `tidybayes::add_epred_draws()`), including a `diff` column of the
#'   difference between `ref`erence and each `pred_step` from the minimum of
#'   `var_col` to the maximum of `var_col`}
#'   \item{res}{data frame of predictions summarised, including columns for
#'   each of `res_q` (applied to the differences between `ref` and `rec`)}
#'   \item{n_data}{`nrow(mod$data)`}
#'   \item{n_fixed_coefs}{`length(mod$coefficients[!grepl("b\\[", names(mod$coefficients))])`}
#'   \item{n_random_coefs}{`length(mod$coefficients[grepl("b\\[", names(mod$coefficients))])`}
#' }
#' @export
#'
#' @examples
make_mod_res <- function(mod_file
                         , ref = -20
                         , pred_step = 2
                         , cov_q = c(0.5)
                         , res_q = c(0.1, 0.5, 0.9)
                         , do_gc = TRUE
                         , ...
                         ) {

  model_results <- rio::import(mod_file)

  res <- c(model_results
           , as.list(environment())
           , list(...)
           )

  purrr::walk2(names(res)
               , res
               , assign
               , pos = 1
               )

  if("stanreg" %in% class(res$mod)) {

    if(!exists("ndraws")) ndraws <- res$mod$stanfit@sim$iter

    # Deal with covariate, if needed
    if(!is.null(cov_col)) {

      res$cov <- envFunc::quibble(res$mod$data$cov, cov_q) %>%
        tidyr::pivot_longer(everything()
                            , names_to = "cov_q"
                            , values_to = "cov"
                            ) %>%
        dplyr::mutate(log_cov = log(cov)
                      , cov_q = forcats::fct_reorder(cov_q
                                                     , cov
                                                     )
                      )

    }

    if(stats::family(res$mod)$family == "binomial") {

      res$data$success <- res$mod$y[,1]
      res$data$trials <- res$mod$y[,1] + res$mod$y[,2]

    }

    pred_var <- sort(unique(c(seq(min(res$mod$data[[var_col]], na.rm = TRUE)
                                    , max(res$mod$data[[var_col]], na.rm = TRUE)
                                    , pred_step
                                    )
                                , ref
                                )
                              )
                       )

    pred_var <- pred_var[pred_var > 0]

    pred_at <- mod$data %>%
      dplyr::filter(success > 0) %>%
      dplyr::distinct(dplyr::across(any_of(c(cat_col, var_col)))) %>%
      dplyr::group_by(dplyr::across(any_of(cat_col))) %>%
      dplyr::filter(!!rlang::ensym(var_col) == min(!!rlang::ensym(var_col)) |
                      !!rlang::ensym(var_col) == max(!!rlang::ensym(var_col))
                    ) %>%
      dplyr::mutate(minmax = dplyr::case_when(!!rlang::ensym(var_col) == min(!!rlang::ensym(var_col)) ~ "min"
                                              , !!rlang::ensym(var_col) == max(!!rlang::ensym(var_col)) ~ "max"
                                              , TRUE ~ "neither"
                                              )
                    ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(values_from = !!rlang::ensym(var_col)
                         , names_from = "minmax"
                         ) %>%
      na.omit() %>%
      dplyr::left_join(tibble::tibble(!!rlang::ensym(var_col) := pred_var)
                       , by = character()
                       ) %>%
      dplyr::group_by(!!rlang::ensym(cat_col)) %>%
      dplyr::filter(!!rlang::ensym(var_col) <= max
                    , !!rlang::ensym(var_col) >= min
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::select(-c(min, max)) %>%
      {if(!is.null(random_col)) (.) %>%
          dplyr::mutate(!!rlang::ensym(random_col) := factor(paste0(random_col
                                                                   , paste0("_"
                                                                            , sample_new_levels_type
                                                                            )
                                                                   )
                                                            )
                        ) else (.)
      } %>%
      {if(!is.null(cov_col)) (.) %>%
          dplyr::left_join(res$cov
                           , by = character()
          ) else (.)
        }

    if(!is.null(var_col)) pred_at["var"] <- pred_at[var_col]

    if(!is.null(cat_col)) pred_at["cat"] <- pred_at[cat_col]

    if(!is.null(cov_col)) {

      pred_at[cov_col] <- pred_at$cov
      pred_at["use_cov"] <- if(log_cov) pred_at$log_cov else pred_at$cov

    }

    if(!is.null(random_col)) pred_at["rand"] <- pred_at[random_col]


    if(nrow(pred_at) > 0) {

      pred <- res$mod$data %>%
        dplyr::distinct(dplyr::across(any_of(cat_col))) %>%
        dplyr::mutate(success = 0
                      , trials = 100
                      ) %>%
        dplyr::left_join(tibble::tibble(year := pred_var)
                         , by = character()
                         ) %>%
        dplyr::inner_join(pred_at) %>%
        tidybayes::add_epred_draws(res$mod
                                   , ...
                                   , value = "pred"
                                   ) %>%
        dplyr::ungroup()

      if(ref < 0) {

        ref_draw <- pred %>%
          dplyr::mutate(!!rlang::ensym(var_col) := !!rlang::ensym(var_col) - ref) %>%
          dplyr::rename(ref = pred) %>%
          dplyr::select(tidyselect::any_of(c(cat_col, cov_col, var_col, random_col)) # do include var col here
                        , .draw
                        , ref
                        )

      } else {

        ref_draw <- pred %>%
          dplyr::filter(var == ref) %>%
          dplyr::select(tidyselect::any_of(c(cat_col, cov_col, random_col)) # don't include var_col here
                        , tidyselect::matches(cov_name)
                        , .draw
                        , ref = pred
                        )

      }

    } else pred <- tibble::tibble()

    if(nrow(pred) > 0) {

      res$pred <- pred %>%
        dplyr::left_join(ref_draw) %>%
        dplyr::mutate(diff = -1 * (ref - pred)
                      , diff_prop = pred / ref
                      )

      res$res_pred <- res$pred %>%
        dplyr::group_by(dplyr::across(any_of(c(cat_col
                                               , random_col
                                               , var_col
                                               )
                                             )
                                      )
                        , dplyr::across(contains(cov_name))
                        ) %>%
        dplyr::summarise(check = dplyr::n() - ndraws
                         , n_draws = dplyr::n()
                         , pred = envFunc::quibble(pred, res_q, na.rm = TRUE)
                         ) %>%
        dplyr::ungroup() %>%
        tidyr::unnest(cols = c(pred))

      res$res_diff <- res$pred %>%
        dplyr::group_by(dplyr::across(any_of(c(cat_col
                                               , random_col
                                               , var_col
                                               )
                                             )
                                      )
                        , dplyr::across(contains(cov_name))
                        ) %>%
        dplyr::summarise(check = dplyr::n() - ndraws
                         , pred = median(pred)
                         , n_draws = dplyr::n()
                         , lower = sum(diff < 0) / n_draws
                         , diff = envFunc::quibble(diff, res_q, na.rm = TRUE)
                         ) %>%
        dplyr::ungroup() %>%
        tidyr::unnest(cols = c(diff)) %>%
        add_likelihood(lower)

    }

    res$n_data <- nrow(res$mod$data)
    res$n_fixed_coefs <- length(res$mod$coefficients[!grepl(paste0("b\\[|", random_col), names(res$mod$coefficients))])
    res$n_random_coefs <- length(res$mod$coefficients[grepl(paste0("b\\[|", random_col), names(res$mod$coefficients))])


    if(do_gc) {

      stuff <- grep("res", ls(), value = TRUE, invert = TRUE)

      rm(list = stuff)

      gc()

    }

  } else res <- list("Not 'stanreg'")

  return(res)

}
