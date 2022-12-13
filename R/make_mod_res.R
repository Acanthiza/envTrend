

#' Summarise results from `make_ll_model`
#'
#' @param path_to_model_file Character. Path to saved results from
#' `make_ll_model`
#' @param scale_name Character. Name of column in original data with spatial
#' data.
#' @param time_col Character. Name of column in original data with time data.
#' @param ref Numeric. Either a reference year (or other time point) or a
#' negative integer. If the former, all years will be compared to that
#' `ref`erence. If the later, all years will be compared to themselves + `ref`.
#' @param pred_step Numeric. What step (in units of `time_col`) to predict at?
#' @param draws Passed to `ndraws` argument of `tidybayes::add_predicted_draws`
#' @param list_length_q Numeric. What list lengths quantiles to predict at?
#' @param res_q Numeric. What quantiles to summarise predictions at?
#' @param do_gc Logical. Run `base::gc` after predict? On a server with shared
#' resources, can be necessary when summarising many, many models to prevent
#' filling RAM.
#'
#' @return A list with components
#' \describe{
#'   \item{list_length}{data frame of list length quantiles in original data}
#'   \item{data}{data frame of original data retrieved from `mod$data`}
#'   \item{pred}{data frame of predictions (via
#'   `tidybayes::add_predicted_draws`), including a `diff` column of the
#'   difference between `ref`erence and each `rec`ent.}
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
                         , scale_name
                         , time_col = "year"
                         , ref = 2000
                         , pred_step = 2
                         , draws = 200
                         , list_length_q = c(0.25, 0.5, 0.75)
                         , res_q = c(0.1, 0.5, 0.9)
                         , do_gc = TRUE
                         ) {


  mod <- rio::import(path_to_model_file)

  if("stanreg" %in% class(mod)) {

    res <- list()

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

    res$data <- mod$data

    if(!"list_length" %in% names(res$data)) {

      res$data$list_length <- exp(res$data$log_list_length)

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

    pred <- mod$data %>%
      dplyr::distinct(dplyr::across(any_of(scale_name))) %>%
      dplyr::mutate(success = 0
                    , trials = 100
                    )  %>%
      dplyr::left_join(tibble::tibble(!!rlang::ensym(time_col) := pred_times)
                       , by = character()
                       ) %>%
      dplyr::full_join(res$list_length
                       , by = character()
                       ) %>%
      tidybayes::add_predicted_draws(mod
                                     , ndraws = draws
                                     , re_formula = NA
                                     , value = "pred"
                                     ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(pred = pred / trials)

    if(ref < 0) {

      ref_draw <- pred %>%
        dplyr::mutate(year = year - ref_time) %>%
        dplyr::rename(ref = pred) %>%
        dplyr::select(tidyselect::any_of(scale_name)
                      , tidyselect::any_of(time_col)
                      , .draw
                      , ref
                      )

    } else {

      ref_draw <- pred %>%
        dplyr::filter(!!rlang::ensym(time_col) == ref) %>%
        dplyr::select(tidyselect::any_of(scale_name)
                      , tidyselect::matches("list_length")
                      , .draw
                      , ref = pred
                      )

    }

    res$pred <- pred %>%
      dplyr::left_join(ref_draw) %>%
      dplyr::mutate(diff = -1 * (ref - pred)
                    , diff_prop = pred / ref
                    )

    res$res <- res$pred %>%
      dplyr::group_by(dplyr::across(any_of(c(scale_name
                                             , time_col
                                             )
                                           )
                                    )
                      , dplyr::across(contains("list_length"))
                      ) %>%
      dplyr::summarise(check = dplyr::n() - draws
                       , pred = median(pred)
                       , lower = sum(diff < 0, na.rm = TRUE) / dplyr::n()
                       , diff = envFunc::quibble(diff, res_q, na.rm = TRUE)
                       ) %>%
      dplyr::ungroup() %>%
      tidyr::unnest(cols = c(diff)) %>%
      envFunc::add_likelihood(lower)

    res$n_data <- nrow(mod$data)
    res$n_fixed_coefs <- length(mod$coefficients[!grepl("b\\[", names(mod$coefficients))])
    res$n_random_coefs <- length(mod$coefficients[grepl("b\\[", names(mod$coefficients))])


    if(do_gc) {

      stuff <- grep("res", ls(), value = TRUE, invert = TRUE)

      rm(list = stuff)

      gc()

    }

  } else res <- list("Not 'stanreg'")

  return(res)

}
