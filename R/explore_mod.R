
#' Explore (pre- and post-) model results.
#'
#'
#' @param taxa Scientific name of taxa (for labelling things).
#' @param common Common name of taxa (also used in labels).
#' @param df Data used for model saved at `mod_path`.
#' @param mod_path Path to saved model.
#' @param out_file Path to save exploration results back to.
#' @param mod_type Type of model (e.g. 'reporting rate', or 'occupancy')
#' @param resp_var What is the response variable?
#' @param geo_var Categorical variable in the model (usually biogeographic)
#' @param time_var Continuous time variable (usually `year`)
#' @param max_levels Maximum number of classes to include in categorical plots.
#' @param draws Number of draws from posterior distribution to display in plots.
#' @param reference_year Reference time at which to predict (and compare change)
#' @param recent_year Time at which to predict to compare against reference_year
#' @param re_run Logical. If file `out_file` already exists, should it be
#' re-run?
#' @param quant_probs Quantiles for summarising.
#'
#' @return A list with components
#' \itemize{
#'   \item{count_char}{ggplot object. Count of levels within character variables.}
#'   \item{y_vs_char}{ggplot object. Response variable vs character variables.}
#'   \item{count_num}{ggplot object. Histogram of each numeric variable.}
#'   \item{y_vs_num}{ggplot object. Response variable vs numeric variables with `geom_smooth()`.}
#'   \item{pairs}{`GGally::ggpairs` object from `df`.}
#'   \item{pred}{Dataframe of `posterior_predict` for levels of interest.}
#'   \item{res}{`pred` summarised by `c(resp_var, time_var)`.}
#' }
#' @export
#'
#' @examples
  explore_mod <- function(taxa
                          , common
                          , df
                          , mod_path
                          , out_file = gsub("\\.rds", "_summary.rds", mod_path)
                          , mod_type
                          , resp_var = "prop"
                          , geo_var = "IBRA_SUB_N"
                          , time_var = "year"
                          , max_levels = 30
                          , draws = 200
                          , reference_year = 2000
                          , recent_year = 2010
                          , re_run = FALSE
                          , quant_probs = c(0.05, 0.5, 0.95)
                          ) {

    `:=` <- rlang::`:=`

    where <- tidyselect:::where

    taxa <- as.character(taxa)
    common <- as.character(common)

    print(taxa)

    #-------do_run--------

    do_run <- if(file.exists(out_file)) {

      if(re_run) TRUE else FALSE

    } else TRUE


    if(do_run) {

      tests <- tibble::tribble(~type, ~year
                               , "reference", reference_year
                               , "recent", recent_year
                               ) %>%
        tidyr::unnest(cols = c(.data$year))

      reference <- tests$year[tests$type == "reference"]

      recent <- tests$year[tests$type == "recent"]

      context <- c(geo_var, time_var, mod_type)

      mod <- rio::import(mod_path)

      #-------setup explore-------

      res <- list()

      plot_titles <- bquote(~italic(.(taxa))*":" ~ .(common))

      has_ll <- sum(grepl("list_length"
                          , names(mod$coefficients))
                    ) > 0

      if(!resp_var %in% names(df)) df <- df %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
        dplyr::summarise({{ resp_var }} := sum(.data$success) / n()) %>%
        dplyr::ungroup()


      # variables to explore
      var_exp <- c(resp_var
                   , colnames(df)
                   ) %>%
        unique()

      dat_exp <- df %>%
        dplyr::select(tidyselect::any_of(var_exp))

      has_numeric <- dat_exp %>%
        dplyr::select(-1) %>%
        dplyr::select(where(is.numeric)) %>%
        ncol() %>%
        `>` (0)

      has_character <- dat_exp %>%
        dplyr::select(-1) %>%
        dplyr::mutate(dplyr::across(where(is.factor),as.character)) %>%
        dplyr::select(where(is.character)) %>%
        ncol() %>%
        `>` (0)

      # Character variables
      if(has_character) {

        plot_data <- dat_exp %>%
          dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
          dplyr::select_if(is.character) %>%
          tidyr::gather(variable, value, 1:ncol(.)) %>%
          dplyr::group_by(.data$variable) %>%
          dplyr::mutate(levels = dplyr::n_distinct(.data$value)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(levels < max_levels)

        # count character
        res$count_char <- ggplot2::ggplot(data = plot_data) +
          ggplot2::geom_histogram(ggplot2::aes(.data$value)
                                  , stat = "count"
                                  ) +
          ggplot2::facet_wrap(~ .data$variable
                              , scales = "free"
                              ) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                             , vjust = 0.5
                                                             , hjust = 1
                                                             )
                         ) +
          ggplot2::labs(title = plot_titles
                        , subtitle = "Count of levels within character variables"
                        )

        # resp_var vs character
        plot_data <- dat_exp %>%
          dplyr::mutate({{ resp_var }} := as.factor(.data[[resp_var]])) %>%
          dplyr::mutate_if(is.factor,as.character) %>%
          dplyr::select_if(is.character) %>%
          dplyr::mutate({{resp_var}} := as.numeric(.data[[resp_var]])) %>%
          tidyr::gather(variable, value, 2:ncol(.)) %>%
          dplyr::group_by(.data$variable) %>%
          dplyr::mutate(levels = dplyr::n_distinct(.data$value)) %>%
          dplyr::ungroup() %>%
          dplyr::filter(levels < max_levels)

        res$y_vs_char <- ggplot2::ggplot(plot_data) +
          ggplot2::geom_boxplot(ggplot2::aes(x = .data$value
                                             , y = .data[[resp_var]]
                                             )
                                ) +
          ggplot2::facet_wrap(~ .data$variable
                              , scales = "free"
                              ) +
          ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90
                                                  , vjust = 0.5
                                                  )
                         ) +
          ggplot2::labs(title = plot_titles
                        , subtitle = paste0("Boxplots of response variable ("
                                            , resp_var
                                            , ") against character variables"
                                            )
                        )

      }

      # Numeric variables
      if(has_numeric) {

        plot_data <- dat_exp %>%
          dplyr::select(where(is.numeric)) %>%
          tidyr::gather(variable, value, 1:ncol(.))

        # Count numeric
        res$count_num <- ggplot2::ggplot(data = plot_data
                                         , ggplot2::aes(.data$value)
                                         ) +
          ggplot2::geom_histogram() +
          ggplot2::facet_wrap(~ .data$variable
                              , scales = "free"
                              ) +
          ggplot2::labs(title = plot_titles
                        , subtitle = "Histograms of numeric variables"
                        )

        # resp_var vs. Numeric
        plot_data <- dat_exp %>%
          dplyr::select(tidyselect::any_of(var_exp)) %>%
          dplyr::select(where(is.numeric)) %>%
          tidyr::gather(variable, value, 2:ncol(.)) %>%
          dplyr::arrange({{ resp_var }})

        res$y_vs_num <- ggplot2::ggplot(data = plot_data
                                        , ggplot2::aes(x = .data$value
                                                       , y = .data[[resp_var]]
                                                       )
                                        ) +
          ggplot2::geom_point(alpha = 0.5) +
          ggplot2::geom_smooth() +
          ggplot2::facet_wrap(~.data$variable, scales = "free") +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                             , vjust = 0.5
                                                             )
                         ) +
          ggplot2::labs(title = plot_titles
                        , subtitle = paste0("Numeric variables plotted against response variable ("
                                            , resp_var
                                            , ")"
                                            )
                        )

      }

      plot_data <- dat_exp %>%
        dplyr::mutate(dplyr::across(where(is.character),factor)) %>%
        dplyr::select(where(~is.numeric(.x)|is.factor(.x) & dplyr::n_distinct(.x) < 15)) %>%
        dplyr::mutate(dplyr::across(where(is.factor),factor))

      res$pairs <- GGally::ggpairs(plot_data) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                           , vjust = 0.5
                                                           )
                       )


      #-------residuals-------

      if(length(residuals(mod)) == nrow(df)) {

        res$resid <- tibble::tibble(residual = residuals(mod)
                                    , fitted = fitted(mod)
                                    ) %>%
          dplyr::bind_cols(df)


        res$resid_plot <- ggplot2::ggplot(data = res$resid
                                          , ggplot2::aes(x = .data$fitted
                                                         , y = .data$residual
                                                         )
                                          ) +
          ggplot2::geom_point(size = 2) +
          ggplot2::geom_hline(yintercept = 0
                              , linetype = 2
                              , colour = "red"
                              ) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                             , vjust = 0.5
                                                             , hjust = 1
                                                             )
                         ) %>%
          ggplot2::scale_colour_viridis_d(end = 0.9)


        res$resid_plot_num <- if(has_numeric) {

          plot_data <- res$resid %>%
            dplyr::select_if(is.numeric) %>%
            tidyr::pivot_longer(2:ncol(.))

          ggplot2::ggplot(data = plot_data
                          , ggplot2::aes(x = .data$value
                                         , y = .data$residual
                                         )
                          ) +
            ggplot2::geom_point(size = 2) +
            ggplot2::geom_smooth(method = "lm")  +
            ggplot2::geom_hline(yintercept = 0
                                , linetype = 2
                                , colour = "red"
                                ) +
            ggplot2::facet_wrap(~ .data$name
                                , scales = "free_x"
                                ) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                               , vjust = 0.5
                                                               , hjust = 1
                                                               )
                           ) +
            ggplot2::scale_colour_viridis_d()

        } else NULL


        res$resid_plot_char <- if(has_character) {

          plot_data <- res$resid %>%
            dplyr::mutate(dplyr::across(where(is.factor),as.character)) %>%
            dplyr::select(1
                          , where(is.character)
                          ) %>%
            tidyr::pivot_longer(2:ncol(.)) %>%
            dplyr::group_by(.data$name) %>%
            dplyr::mutate(levels = dplyr::n_distinct(.data$value)) %>%
            dplyr::ungroup() %>%
            dplyr::filter(levels < max_levels)

          ggplot2::ggplot(data = plot_data
                          , ggplot2::aes(x = .data$value
                                         , y = .data$residual
                                         )
                          ) +
            ggplot2::geom_boxplot() +
            ggplot2::geom_hline(ggplot2::aes(yintercept = 0)
                                , linetype = 2
                                , colour = "red"
                                ) +
            ggplot2::facet_wrap(~ .data$name
                                , scales = "free"
                                ) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                      , vjust = 0.5
                                                      , hjust = 1
                                                      )
                           )

        } else NULL

      }


      #---------post explore-------

      if(stats::family(mod)$family == "beta") class(mod) <- unique(c(class(mod),"betareg"))

      is_binomial_mod <- stats::family(mod)$family == "binomial"

      res$pred <- df %>%
        dplyr::distinct(dplyr::across(tidyselect::any_of(context))) %>%
        dplyr::mutate(list_length = if(has_ll) stats::median(exp(df$log_list_length)) else NULL
                      , log_list_length = if(has_ll) log(list_length) else NULL
                      , col = row.names(.)
                      , success = if(is_binomial_mod) 0 else NULL
                      , trials = if(is_binomial_mod) 100 else NULL
                      ) %>%
        tidybayes::add_epred_draws(mod
                                   , ndraws = draws
                                   , re_formula = NA
                                   , value = "pred"
                                   ) %>%
        dplyr::ungroup()


      res$res <- res$pred %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
        dplyr::summarise(n = dplyr::n()
                         , nCheck = nrow(tibble::as_tibble(mod))
                         , modMean = mean(.data$pred)
                         , modMedian = stats::quantile(.data$pred, 0.5)
                         , modci90lo = stats::quantile(.data$pred, 0.05)
                         , modci90up = stats::quantile(.data$pred, 0.95)
                         , text = paste0(round(.data$modMedian,2)
                                         , " ("
                                         , round(.data$modci90lo,2)
                                         , " to "
                                         , round(.data$modci90up,2)
                                         , ")"
                                         )
                         ) %>%
        dplyr::ungroup()


      #------res plot data-------

      plot_data <- df %>%
        dplyr::distinct(dplyr::across(tidyselect::any_of(context))) %>%
        dplyr::mutate(success = 0
                      , trials = 100
                      ) %>%
        dplyr::full_join(tibble::tibble(probs = 0.5) %>%
                           {if(has_ll) (.) %>%
                               dplyr::mutate(list_length = purrr::map_dbl(probs
                                                                   , function(x) stats::quantile(unique(exp(df$log_list_length))
                                                                                                 , probs = x
                                                                                                 )
                                                                   )
                                             , log_list_length = log(list_length)
                                             , length = paste0("At list length quantile "
                                                               , probs
                                                               , " = "
                                                               , list_length
                                                               )
                               ) else (.)
                             }
                         , by = character()
                         ) %>%
        tidybayes::add_epred_draws(mod
                                   , ndraws = draws
                                   , re_formula = NA
                                   , value = "pred"
                                   )

      sub_title <-  if(has_ll) {

        paste0("List length corrected reporting rate.\nDashed red lines indicate years for comparison (see text).")

      } else {

        paste0(mod_type
               , ".\nDashed red lines indicate years for comparison (see text)."
               )

      }

      sub_title_line <- paste0(
        sub_title
        , if(has_ll) {

          paste0("\nLines are "
                 , draws
                 , " draws from posterior distribution.\n"
                 , unique(plot_data$length)
                 )

        } else {

          paste0("\nLines are "
                 , draws
                 , " draws from posterior distribution."
                 )

        }
      )

      sub_title_ribbon <- paste0(sub_title
                                 , "\nMedian (thick line) and 90% credible intervals (shaded)."
                                 )

      #-------res plot_line-----------

      p <- ggplot2::ggplot(data = plot_data
                           , ggplot2::aes(x = .data[[time_var]]
                                          , y = .data[[resp_var]]
                                          )
                           ) +
        ggplot2::geom_line(ggplot2::aes(y = .data$pred
                                        , group = .draw
                                        )
                           , alpha = 0.5
                           ) +
        ggplot2::geom_vline(xintercept = tests$year
                            , linetype = 2
                            , colour = "red"
                            ) +
        ggplot2::facet_wrap(as.formula(paste0("~ "
                                              , geo_var
                                              )
                                       )
                            ) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                  , vjust = 0.5
                                                  , hjust = 1
                                                  )
                       ) +
        ggplot2::labs(title = plot_titles
                      , subtitle = sub_title_line
                      )

      if(has_ll) {

        p <- p +
          ggplot2::geom_jitter(data = df
                               ,ggplot2::aes(x = .data$year
                                             , y = .data[[resp_var]]
                                             , colour = exp(.data$log_list_length)
                                             )
                               , width = 0.1
                               , height = 0.05
                               ) +
          ggplot2::scale_colour_viridis_c() +
          ggplot2::labs(colour = "List length")

      }

      if(!has_ll) {

        p <- p +
          ggplot2::geom_jitter(data = df
                               , ggplot2::aes(x = .data$year
                                             , y = .data[[resp_var]]
                                             , colour = .data$trials
                                             )
                               , width = 0.1
                               , height = 0.01
                               ) +
          ggplot2::scale_colour_viridis_c()

      }

      res$plot_line <- p


      #------res plot_ribbon-------

      p <- ggplot2::ggplot() +
        ggplot2::geom_ribbon(data = res$res
                             , ggplot2::aes(.data[[time_var]]
                                            , .data$modMean
                                            , ymin = .data$modci90lo
                                            , ymax = .data$modci90up
                                            )
                             , alpha = 0.4
                    ) +
        ggplot2::geom_line(data = res$res
                           , ggplot2::aes(x = .data[[time_var]]
                                          , y = .data$modMean
                                          )
                           , linetype = 1
                           , size = 1.5
                           ) +
        ggplot2::geom_vline(xintercept = tests$year
                            , linetype = 2
                            , colour = "red"
                            ) +
        ggplot2::facet_wrap(as.formula(paste0("~ "
                                              , geo_var
                                              )
                                       )
                            ) +
        ggplot2::labs(title = plot_titles
                      , subtitle = sub_title_ribbon
                      )

      if(has_ll) {

        p <- p +
          ggplot2::geom_jitter(data = df
                    , ggplot2::aes(.data[[time_var]]
                                   , .data[[resp_var]]
                                   , colour = exp(.data$log_list_length)
                                   )
                    , width = 0.1
                    , height = 0.05
                    ) +
        ggplot2::scale_colour_viridis_c() +
        ggplot2::labs(colour = "List length")

      }

      if(!has_ll) {

        p <- p +
          ggplot2::geom_jitter(data = df
                               , ggplot2::aes(.data[[time_var]]
                                              , .data[[resp_var]]
                                              , colour = .data$trials
                                              )
                               , width = 0.1
                               , height = 0.05
                               ) +
          ggplot2::scale_colour_viridis_c() +
          ggplot2::labs(colour = "Trials")

      }

      res$plot_ribbon <- p


      #------year difference df-----------

      res$year_diff_df <- df %>%
        dplyr::distinct(dplyr::across(tidyselect::any_of(context[!context %in% time_var]))) %>%
        dplyr::full_join(tests
                         , by = character()
                         ) %>%
        dplyr::mutate(list_length = if(has_ll) median(exp(df$log_list_length)) else NULL
                      , log_list_length = if(has_ll) log(list_length) else NULL
                      , success = 0
                      , trials = 100
                      , nCheck = nrow(tibble::as_tibble(mod))
                      , mod_type = mod_type
                      , taxa = taxa
                      , common = common
                      ) %>%
        tidybayes::add_epred_draws(mod
                                   , ndraws = draws
                                   , re_formula = NA
                                   , value = "pred"
                                   ) %>%
        dplyr::ungroup() %>%
        dplyr::select(tidyselect::any_of(context)
                      , .data$type
                      , .data$pred
                      , .data$.draw
                      ) %>%
        tidyr::pivot_wider(names_from = "type"
                           , values_from = c(tidyselect::any_of(time_var)
                                             , "pred"
                                             )
                           ) %>%
        #setNames(gsub("\\d{4}", "", names(.))) %>%
        dplyr::mutate(diff = as.numeric(pred_recent - pred_reference))


      #-------year difference res---------

      res$year_diff_res <- res$year_diff_df %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
        dplyr::summarise(nCheck = dplyr::n()
                         , lower = sum(diff < 0) / nCheck
                         , higher = sum(diff > 0) / nCheck
                         , meanDiff = mean(diff)
                         , medianDiff = median(diff)
                         , cilo = stats::quantile(diff, probs = 0.05)
                         , ciup = stats::quantile(diff, probs = 0.95)
                         , reference = unique({{ reference_year }})
                         , recent = unique({{ recent_year }})
                         ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(likelihood = purrr::map(lower
                                       , ~cut(.
                                              , breaks = c(0, lulikelihood$maxVal)
                                              , labels = lulikelihood$likelihood
                                              , include.lowest = TRUE
                                              )
                                       )
                      ) %>%
        tidyr::unnest(cols = c(likelihood)) %>%
        dplyr::mutate(text = paste0(tolower(likelihood)
                                    , " to be lower in "
                                    , {{ geo_var }}
                                    , " ("
                                    , 100*round(lower,2)
                                    , "% chance)"
                                    )
                      , text = gsub("in Kangaroo Island","on Kangaroo Island",text)
                      )

      #------year difference plot--------

      plot_data <- res$year_diff_df %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(geo_var))) %>%
        dplyr::mutate(lower = sum(diff < 0) / dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(likelihood = purrr::map(lower
                                       , ~cut(.
                                              , breaks = c(0,lulikelihood$maxVal)
                                              , labels = lulikelihood$likelihood
                                              , include.lowest = TRUE
                                              )
                                       )
                      ) %>%
        tidyr::unnest(cols = c(likelihood)) %>%
        dplyr::mutate(likelihood = forcats::fct_expand(likelihood
                                                       ,levels(lulikelihood$likelihood)
                                                       )
                      )

      res$year_diff_plot <- ggplot2::ggplot(data = plot_data
                                            , ggplot2::aes(.data$diff
                                                           , .data[[geo_var]]
                                                           , fill = .data$likelihood
                                                           )
                                            ) +
        ggridges::geom_density_ridges() +
        ggplot2::geom_vline(xintercept = 0
                   , linetype = 2
                   , colour = "red"
                   ) +
        ggplot2::scale_fill_viridis_d(drop = FALSE) +
        ggplot2::labs(title = plot_titles
                      , subtitle = paste0("Difference in "
                                          , recent
                                          , " "
                                          , tolower(mod_type)
                                          , " compared to "
                                          , reference
                                          )
                      , x = "Difference"
                      , y = "IBRA Subregion"
                      , fill = "Likelihood of decrease"
                      , caption = paste0("Red dotted line indicates no change between "
                                         , reference
                                         , " and "
                                         , recent
                                         )
                      )

      rio::export(res
                  , out_file
                  )

      rm(res)

      gc()

    } else {

      print("already done")

      }

    }
