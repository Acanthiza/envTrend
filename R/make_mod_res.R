

#' Summarise results from `make_ll_model`
#'
#' Should now be generic enough to
#'
#' @param mod_file Character. Path to saved results from `make_ll_model`
#' @param ref Numeric. Either a single value of `var_col` from `make_ll_model`
#' to use as a reference value or a negative integer on the scale of `var_col`
#' to compare all values of `var_col` against `var_col + ref`.
#' @param pred_step Numeric. What step (in units of `var_col`) to predict at?
#' @param cov_q Numeric. What list lengths quantiles to predict at?
#' @param res_q Numeric. What quantiles to summarise predictions at?
#' @param keep_mod Logical. Return `mod` component of results (imported from
#' `mod_file`). Saves memory if this is not returned.
#' @param keep_pred Logical. Return `pred` component of results? Saves memory
#' if this is not returned.
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
                         , include_random = FALSE
                         , cov_q = c(0.5)
                         , res_q = c(0.1, 0.5, 0.9)
                         , keep_mod = TRUE
                         , keep_pred = TRUE
                         , do_gc = TRUE
                         , ...
                         ) {

  results <- c(rio::import(mod_file)
               , as.list(environment())
               , list(...)
               )

  results$res <- NULL

  purrr::walk2(names(results[grepl("col", names(results))])
               , results[grepl("col", names(results))]
               , assign
               , envir = environment()
               )

  if("stanreg" %in% class(results$mod)) {

    # diagnostics--------

    results$num_divergent <- rstan::get_num_divergent(results$mod$stanfit)

    results$prop_divergent <- results$num_divergent / nrow(data.frame(results$mod))

    if(results$num_divergent > 0) {

      results$divergent <- rstan::get_divergent_iterations(results$mod$stanfit)

    }

    results$summary <- results$mod$stanfit %>%
      rstan::summary(probs = c(0.1, 0.5, 0.9)) %>%
      `[[`(1) %>%
      tibble::as_tibble(rownames = "parameter")

    # values of 1.05 and 100 (used below) taken from printout associated with rstan::monitor

    results$check_summary <- results$summary %>%
      dplyr::select(Rhat, n_eff) %>%
      tidyr::pivot_longer(everything()
                          , names_to = "diagnostic"
                          ) %>%
      dplyr::mutate(fail = dplyr::case_when(diagnostic == "Rhat" ~ value >= 1.05
                                            , diagnostic == "n_eff" ~ value < 100
                                            , TRUE ~ NA
                                            )
                    ) %>%
      dplyr::group_by(diagnostic) %>%
      dplyr::summarise(parameters = n()
                       , failed = sum(fail)
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pass = failed == 0)

    # deal with some of ... that are required elsewhere
    if(is.null(results[["ndraws"]])) results$ndraws <- nrow(tibble::as_tibble(results$mod))
    if(is.null(results[["sample_new_levels"]])) results$sample_new_levels <- "uncertainty" # default via tidybayes::add_epred_draws

    # Deal with covariate, if needed
    if(!is.null(results$cov_col)) {

      results$cov <- envFunc::quibble(results$mod$data$cov, cov_q) %>%
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

    # Deal with response variable if family is binomial (probably doesn't work where weights argument is used instead of cbind?)
    if(stats::family(results$mod)$family == "binomial") {

      results$mod$data$y <- results$mod$y[,1]
      results$mod$data$y_total <- results$mod$y[,1] + results$mod$y[,2]

    }

    # Create new_data for pred
    pred_var <- sort(unique(c(seq(min(results$mod$data[[results$var_col]], na.rm = TRUE)
                                    , max(results$mod$data[[results$var_col]], na.rm = TRUE)
                                    , pred_step
                                    )
                                , ref
                                )
                              )
                       )

    pred_var <- pred_var[pred_var > 0]

    pred_at <- results$mod$data %>%
      dplyr::filter(y_total > 0) %>%
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
                       )

    if(!is.null(cat_col)) {

      pred_at <- pred_at %>%
        dplyr::group_by(!!rlang::ensym(cat_col)) %>%
        dplyr::filter(!!rlang::ensym(var_col) <= max
                      , !!rlang::ensym(var_col) >= min
                      ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(min, max))

    } else {

      pred_at <- pred_at %>%
        dplyr::filter(!!rlang::ensym(var_col) <= max
                      , !!rlang::ensym(var_col) >= min
                      ) %>%
        dplyr::select(-c(min, max))

    }

    if(!is.null(random_col)) {

      if(include_random) {

        random_pred <- dplyr::distinct(results$mod$data[random_col])

      } else {

        random_pred <- tibble::tibble(!!rlang::ensym(random_col) := factor(paste0(random_col
                                                                                 , paste0("_"
                                                                                          , results$sample_new_levels
                                                                                          )
                                                                                 )
                                                                           )
                                      )

      }

      pred_at <- pred_at %>%
        dplyr::left_join(random_pred
                         , by = character()
                         )

    }

    if(!is.null(cov_col)) {

      pred_at <- pred_at %>%
        dplyr::left_join(results$cov
                         , by = character()
                         )

    }


    # translate to generic terms in pred_at

    if(!is.null(var_col)) pred_at["var"] <- pred_at[var_col]

    if(!is.null(cat_col)) pred_at["cat"] <- pred_at[cat_col]

    if(!is.null(cov_col)) {

      pred_at[cov_col] <- pred_at$cov
      pred_at["use_cov"] <- if(results$log_cov) pred_at$log_cov else pred_at$cov

    }

    if(!is.null(random_col)) pred_at["rand"] <- pred_at[random_col]


    if(nrow(pred_at) > 0) {

      pred <- results$mod$data %>%
        dplyr::distinct(dplyr::across(any_of(cat_col))) %>%
        dplyr::mutate(success = 0
                      , trials = 100
                      ) %>%
        dplyr::left_join(tibble::tibble(!!rlang::ensym(var_col) := pred_var)
                         , by = character()
                         ) %>%
        dplyr::inner_join(pred_at) %>%
        tidybayes::add_epred_draws(results$mod
                                   , ...
                                   , value = "pred"

                                   # used in testing
                                   # , re_formula = NULL
                                   # , allow.new.levels = TRUE
                                   # , sample_new_levels = "uncertainty"

                                   ) %>%
        {if(results$num_divergent > 0) (.) %>% dplyr::mutate(divergent = results$divergent) else (.)} %>%
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
                        , tidyselect::matches(cov_col)
                        , .draw
                        , ref = pred
                        )

      }

    } else pred <- tibble::tibble()

    if(nrow(pred) > 0) {

      results$pred <- pred %>%
        dplyr::left_join(ref_draw) %>%
        dplyr::mutate(diff = -1 * (ref - pred)
                      , diff_prop = pred / ref
                      )

      results$res <- results$pred %>%
        dplyr::group_by(dplyr::across(any_of(c(cat_col
                                               , random_col
                                               , var_col
                                               , cov_col
                                               )
                                             )
                                      )
                        ) %>%
        dplyr::summarise(check = dplyr::n() - results$ndraws
                         , n_draws = dplyr::n()
                         , lower = sum(diff < 0) / n_draws
                         , higher = sum(diff > 0) / n_draws
                         , pred = envFunc::quibble(pred, res_q, na.rm = TRUE)
                         , diff = envFunc::quibble(diff, res_q, na.rm = TRUE) %>%
                           setNames(paste0("diff_", names(.)))
                         ) %>%
        dplyr::ungroup() %>%
        tidyr::unnest(cols = c(pred, diff))

      if(results$num_divergent > 0) {

        results$res_divergent <- results$pred %>%
          dplyr::group_by(dplyr::across(any_of(c(cat_col
                                                 , random_col
                                                 , var_col
                                                 , cov_col
                                                 )
                                               )
                                        )
                          , divergent
                          ) %>%
          dplyr::summarise(n_draws = dplyr::n()
                           , lower = sum(diff < 0) / n_draws
                           , higher = sum(diff > 0) / n_draws
                           , pred = envFunc::quibble(pred, res_q, na.rm = TRUE)
                           , diff = envFunc::quibble(diff, res_q, na.rm = TRUE) %>%
                             setNames(paste0("diff_", names(.)))
                           ) %>%
          dplyr::ungroup() %>%
          tidyr::unnest(cols = c(pred, diff))

      }

    }

    if(!keep_pred) results$pred <- NULL
    if(!keep_mod) results$mod <- NULL

    if(do_gc) {

      stuff <- grep("results", ls(), value = TRUE, invert = TRUE)

      rm(list = stuff)

      gc()

    }

  } else results$mod <- "Not 'stanreg'"

  return(results)

}
