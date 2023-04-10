

#' Summarise results from `make_ll_model`
#'
#' @param path_to_model_file Character. Path to saved results from
#' `make_ll_model`
#' @param geo_col Character. Name of column in original data with spatial
#' data.
#' @param time_col Character. Name of column in original data with time data.
#' @param ref Numeric. Either a reference year (or other time point) or a
#' negative integer. If the former, all years will be compared to that
#' `ref`erence. If the later, all years will be compared to themselves + `ref`.
#' @param pred_step Numeric. What step (in units of `time_col`) to predict at?
#' @param draws Passed to `ndraws` argument of `tidybayes::add_epred_draws()`.
#' Numeric or "max". Max will use all possible draws.
#' @param list_length_q Numeric. What list lengths quantiles to predict at?
#' @param res_q Numeric. What quantiles to summarise predictions at?
#' @param do_gc Logical. Run `base::gc` after predict? On a server with shared
#' resources, can be necessary when summarising many, many models to prevent
#' filling RAM.
#' @param sample_new_levels_type Character. Value of the `sample_new_levels`
#' argument to `tidybayes::add_epred_draws()`.
#'
#' @return A list with components
#' \describe{
#'   \item{list_length}{data frame of list length quantiles in original data}
#'   \item{data}{data frame of original data retrieved from `mod$data`}
#'   \item{pred}{data frame of predictions (via
#'   `tidybayes::add_epred_draws()`), including a `diff` column of the
#'   difference between `ref`erence and each `pred_step` from the minimum of
#'   `time_col` to the maximum of `time_col`}
#'   \item{res}{data frame of predictions summarised, including columns for
#'   each of `res_q` (applied to the differences between `ref` and `rec`)}
#'   \item{n_data}{`nrow(mod$data)`}
#'   \item{n_fixed_coefs}{`length(mod$coefficients[!grepl("b\\[", names(mod$coefficients))])`}
#'   \item{n_random_coefs}{`length(mod$coefficients[grepl("b\\[", names(mod$coefficients))])`}
#' }
#' @export
#'
#' @examples
make_mod_res <- function(path_to_model_file
                         , geo_col
                         , time_col = "year"
                         , random_col = NULL
                         , ref = 2000
                         , pred_step = 2
                         , draws = 200
                         , list_length_q = c(0.25, 0.5, 0.75)
                         , res_q = c(0.1, 0.5, 0.9)
                         , do_gc = TRUE
                         , sample_new_levels_type = "uncertainty"
                         ) {


  model_results <- rio::import(path_to_model_file)

  purrr::walk2(names(model_results)
               , model_results
               , assign
               , pos = 1
               )

  if("stanreg" %in% class(mod)) {

    if(!is.numeric(draws)) draws <- mod$stanfit@sim$iter

    res <- list(data = mod$data)
    res$draws = draws

    # Deal with list length, if needed
    if(any(grepl("list_length", names(mod$data)))) {

      res$list_length <- if("list_length" %in% names(mod$data)) {

        envFunc::quibble(mod$data$list_length, list_length_q)

      } else {

        envFunc::quibble(exp(mod$data$log_list_length), list_length_q)

      }

      res$list_length <- res$list_length %>%
        tidyr::pivot_longer(everything()
                            , names_to = "list_length_q"
                            , values_to = "list_length"
                            ) %>%
        dplyr::mutate(log_list_length = log(list_length)
                      , list_length_q = forcats::fct_reorder(list_length_q
                                                             , list_length
                                                             )
                      )

      if(!"list_length" %in% names(res$data)) {

        res$data$list_length <- exp(res$data$log_list_length)

      }

    }

    if(stats::family(mod)$family == "binomial") {

      res$data$success <- mod$y[,1]
      res$data$trials <- mod$y[,1] + mod$y[,2]

    }

    pred_times <- sort(unique(c(seq(min(mod$data[[time_col]], na.rm = TRUE)
                                    , max(mod$data[[time_col]], na.rm = TRUE)
                                    , pred_step
                                    )
                                , ref
                                )
                              )
                       )

    pred_times <- pred_times[pred_times > 0]

    pred_at <- res$data %>%
      dplyr::filter(success > 0) %>%
      dplyr::distinct(dplyr::across(any_of(geo_col))
                      , dplyr::across(any_of(time_col))
                      ) %>%
      dplyr::group_by(dplyr::across(any_of(geo_col))) %>%
      dplyr::filter(!!rlang::ensym(time_col) == min(!!rlang::ensym(time_col)) |
                      !!rlang::ensym(time_col) == max(!!rlang::ensym(time_col))
                    ) %>%
      dplyr::mutate(minmax = dplyr::case_when(!!rlang::ensym(time_col) == min(!!rlang::ensym(time_col)) ~ "min"
                                              , !!rlang::ensym(time_col) == max(!!rlang::ensym(time_col)) ~ "max"
                                              , TRUE ~ "neither"
                                              )
                    ) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(values_from = !!rlang::ensym(time_col)
                         , names_from = "minmax"
                         ) %>%
      na.omit() %>%
      dplyr::left_join(tibble::tibble(!!rlang::ensym(time_col) := pred_times)
                       , by = character()
                       ) %>%
      dplyr::group_by(!!rlang::ensym(geo_col)) %>%
      dplyr::filter(!!rlang::ensym(time_col) <= max
                    , !!rlang::ensym(time_col) >= min
                    ) %>%
      dplyr::select(-c(min, max)) %>%
      {if(!is.null(random_col)) (.) %>%
          dplyr::mutate(!!rlang::ensym(random_col) := factor(paste0(random_col
                                                                   , paste0("_"
                                                                            , sample_new_levels_type
                                                                            )
                                                                   )
                                                            )
                        ) else (.)
      }


    if(!is.null(time_col)) pred_at["time"] <- pred_at[time_col]
    if(!is.null(geo_col)) pred_at["geo"] <- pred_at[geo_col]
    if(!is.null(random_col)) pred_at["rand"] <- pred_at[random_col]


    if(nrow(pred_at) > 0) {

      pred <- mod$data %>%
        dplyr::distinct(dplyr::across(any_of(geo_col))) %>%
        dplyr::mutate(success = 0
                      , trials = 100
                      ) %>%
        dplyr::left_join(tibble::tibble(year := pred_times)
                         , by = character()
                         ) %>%
        dplyr::inner_join(pred_at) %>%
        dplyr::full_join(res$list_length
                         , by = character()
                         ) %>%
        tidybayes::add_epred_draws(mod
                                   , re_formula = NULL
                                   , allow.new.levels = TRUE
                                   , ndraws = draws
                                   , sample_new_levels = sample_new_levels_type
                                   , value = "pred"
                                   ) %>%
        dplyr::ungroup()

      if(ref < 0) {

        ref_draw <- pred %>%
          dplyr::mutate(time = time - ref) %>%
          dplyr::rename(ref = pred) %>%
          dplyr::select(tidyselect::any_of(c(geo_col, random_col))
                        , .draw
                        , ref
                        )

      } else {

        ref_draw <- pred %>%
          dplyr::filter(time == ref) %>%
          dplyr::select(tidyselect::any_of(c(geo_col, random_col))
                        , tidyselect::matches("list_length")
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

      res$res <- res$pred %>%
        dplyr::group_by(dplyr::across(any_of(c(geo_col
                                               , random_col
                                               , time_col
                                               )
                                             )
                                      )
                        , dplyr::across(contains("list_length"))
                        ) %>%
        dplyr::summarise(check = dplyr::n() - draws
                         , pred = median(pred)
                         , n_draws = dplyr::n()
                         , lower = sum(diff < 0) / n_draws
                         , diff = envFunc::quibble(diff, res_q, na.rm = TRUE)
                         ) %>%
        dplyr::ungroup() %>%
        tidyr::unnest(cols = c(diff)) %>%
        add_likelihood(lower)

    }

    res$n_data <- nrow(mod$data)
    res$n_fixed_coefs <- length(mod$coefficients[!grepl(paste0("b\\[|", random_col), names(mod$coefficients))])
    res$n_random_coefs <- length(mod$coefficients[grepl(paste0("b\\[|", random_col), names(mod$coefficients))])


    if(do_gc) {

      stuff <- grep("res", ls(), value = TRUE, invert = TRUE)

      rm(list = stuff)

      gc()

    }

  } else res <- list("Not 'stanreg'")

  return(res)

}
