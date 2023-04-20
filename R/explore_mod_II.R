
#' Explore (pre-) and (post-) results from `make_mod_res` after `make_ll_model`
#'
#' General workflow is make the model (`make_ll_model`), make the model results
#' (`make_mod_res`) then explore results (`explore_mod_ll`). Might work for
#' other rstanarm models? Not sure anymore.
#'
#' @param model_results List. results from `make_ll_model()`
#' @param taxa
#' @param common
#' @param max_levels
#' @param recent
#' @param limit_preds
#' @param plot_draws
#' @param diff_category
#' @param diff_direction
#' @param do_gc
#'
#' @return
#' @export
#'
#' @examples
  explore_mod_ll <- function(model_results
                             , taxa
                             , common
                             , max_levels = 15
                             , recent = as.numeric(format(Sys.Date(), "%Y")) - 2
                             , limit_preds = TRUE
                             , plot_draws = 200
                             , diff_category = model_results$cat_col
                             , diff_direction = c("lower", "higher")
                             , do_gc = TRUE
                             ) {

    diff_direction <- diff_direction[1]

    purrr::walk2(names(model_results)
                 , model_results
                 , assign
                 , envir = environment()
                 )

    rm(model_results)

    results <- c(as.list(environment()))

    `:=` <- rlang::`:=`

    where <- tidyselect:::where

    taxa <- as.character(taxa)
    common <- as.character(common)

    message(print(taxa))

    if(!exists("ndraws", inherits = FALSE)) ndraws <- nrow(tibble::as_tibble(mod))


    #-------setup explore-------

    plot_titles <- bquote(~italic(.(taxa))*":" ~ .(common))

    results$has_cov <- sum(grepl("cov"
                        , names(mod$coefficients)
                        )
                  ) > 0

    # variables to explore
    if(stats::family(mod)$family == "binomial") {

      df$prop <- df$y / df$y_total

      resp_var <- "prop"

    } else y


    var_exp <- c(resp_var
                 , cov_col
                 , cat_col
                 , var_col
                 , y_total
                 , if(include_random) random_col
                 )


    dat_exp <- mod$data %>%
      dplyr::select(tidyselect::any_of(var_exp)) %>%
      janitor::remove_constant()

    results$has_numeric <- dat_exp %>%
      dplyr::select(-1) %>%
      dplyr::select(where(is.numeric)) %>%
      ncol() %>%
      `>` (0)

    results$has_character <- dat_exp %>%
      dplyr::select(-1) %>%
      dplyr::mutate(dplyr::across(where(is.factor),as.character)) %>%
      dplyr::select(where(is.character)) %>%
      ncol() %>%
      `>` (0)

    # Character variables
    if(results$has_character) {

      plot_data <- dat_exp %>%
        dplyr::mutate(dplyr::across(where(is.factor), as.character)) %>%
        dplyr::select_if(is.character) %>%
        tidyr::gather(variable, value, 1:ncol(.)) %>%
        dplyr::group_by(.data$variable) %>%
        dplyr::mutate(levels = dplyr::n_distinct(.data$value)) %>%
        dplyr::ungroup() %>%
        dplyr::filter(levels < max_levels)

      # count character
      results$count_char <- ggplot2::ggplot(data = plot_data) +
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

      results$y_vs_char <- ggplot2::ggplot(plot_data) +
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
    if(results$has_numeric) {

      plot_data <- dat_exp %>%
        dplyr::select(where(is.numeric)) %>%
        tidyr::gather(variable, value, 1:ncol(.))

      # Count numeric
      results$count_num <- ggplot2::ggplot(data = plot_data
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
        tidyr::gather(variable, value, 2:ncol(.))

      results$y_vs_num <- ggplot2::ggplot(data = plot_data
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
      dplyr::select(where(~is.numeric(.x)|is.factor(.x) & dplyr::n_distinct(.x) < max_levels)) %>%
      dplyr::mutate(dplyr::across(where(is.factor),factor))

    results$pairs <- GGally::ggpairs(plot_data) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                         , vjust = 0.5
                                                         )
                     )


    #-------residuals-------

    if(length(residuals(mod)) == nrow(dat_exp)) {

      results$resid <- dat_exp %>%
        dplyr::mutate(residual = residuals(mod)
                      , fitted = fitted(mod)
                      ) %>%
        dplyr::select(residual, fitted, everything())

      results$resid_plot <- ggplot2::ggplot(data = results$resid
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


      results$resid_plot_norm <- ggplot(results$resid
                                    , aes(residual)
                                    ) +
        ggplot2::geom_function(fun = dnorm
                               , colour = "light blue"
                               , size = 1
                              , args = list(mean = mean(results$resid$residual)
                                            , sd = sd(results$resid$residual)
                                            )
                               ) +
        ggplot2::geom_density(colour = "dark blue"
                              , size = 1
                              ) +
        ggplot2::labs(x = "value"
                      , y = "Density"
                      )

      results$resid_plot_num <- if(results$has_numeric) {

        plot_data <- results$resid %>%
          dplyr::select_if(is.numeric) %>%
          dplyr::select(!tidyselect::matches("cbind")) %>%
          dplyr::select(residual, everything()) %>%
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


      results$resid_plot_char <- if(results$has_character) {

        plot_data <- results$resid %>%
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


    #------res plot data-------

    if(results$has_cov) {

      cov_name <- gsub("_", " ", cov_col)

      cov_text <- paste0("At "
                         , cov_name
                         , " quantile "
                         , cov_q
                         , " = "
                         , cov[1, "cov"]
                         )

    }

    sub_title <-  if(results$has_cov) {

      paste0("Dashed red lines indicate "
             , var_col
             , " for comparison (see text)."
             )

    } else {

      paste0("Dashed red lines indicate years for comparison (see text).")

    }

    sub_title_line <- paste0(
      sub_title
      , if(results$has_cov) {

        paste0("\nLines are "
               , ndraws
               , " draws from posterior distribution.\n"
               , cov_text
               )

      } else {

        paste0("\nLines are "
               , ndraws
               , " draws from posterior distribution."
               )

      }
    )

    sub_title_ribbon <- paste0(sub_title
                               , "\nMedian (thick line) and 90% credible intervals (shaded)."
                               )

    #-------res plot_line-----------

    reference <- max(recent, na.rm = TRUE) + ref

    tests <- tibble::tribble(~type, ~year
                             , "reference", reference
                             , "recent", max(recent, na.rm = TRUE)
                             )

    get_draws <- sort(sample(1 : (chains * iter), plot_draws))

    pred_plot_draws <- pred %>%
      dplyr::filter(.draw %in% get_draws)

    facet_form <- if(any(cat_col %in% names(dat_exp), random_col %in% names(dat_exp))) {

      as.formula(paste0(if(cat_col %in% names(dat_exp)) cat_col
                        , " ~ "
                        , if(random_col %in% names(dat_exp)) random_col
                        )
                 )

    } else NULL

    p <- ggplot2::ggplot(data = pred_plot_draws
                         , ggplot2::aes(x = .data[[var_col]])
                         ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$pred
                                      , group = .data$.draw
                                      )
                         , alpha = 0.5
                         ) +
      ggplot2::geom_vline(xintercept = tests$year
                          , linetype = 2
                          , colour = "red"
                          ) +
      ggplot2::facet_wrap(facet_form) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                , vjust = 0.5
                                                , hjust = 1
                                                )
                     ) +
      ggplot2::labs(title = plot_titles
                    , subtitle = sub_title_line
                    )

    if(results$has_cov) {

      p <- p +
        ggplot2::geom_point(data = dat_exp %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(dplyr::across(var_col))
                                                )
                             , ggplot2::aes(x = .data[[var_col]]
                                           , y = .data[[resp_var]]
                                           , colour = .data[[cov_col]]
                                           )
                             ) +
        ggplot2::scale_colour_viridis_c() +
        ggplot2::labs(colour = cov_col)

    } else {

      p <- p +
        ggplot2::geom_point(data = dat_exp %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(!!rlang::ensym(var_col))
                                                )
                             , ggplot2::aes(x = .data[[var_col]]
                                           , y = .data[[resp_var]]
                                           )
                             )

    }

    results$plot_line <- p


    #------res plot_ribbon-------

    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = res
                           , ggplot2::aes(.data[[var_col]]
                                          , .data[[paste0("q", median(100 * res_q))]]
                                          , ymin = .data[[paste0("q"
                                                                 , stringr::str_pad(100 * min(res_q)
                                                                                    , width = 2
                                                                                    , pad = "0"
                                                                                    )
                                                                 )
                                                          ]]
                                          , ymax = .data[[paste0("q"
                                                                 , stringr::str_pad(100 * max(res_q)
                                                                                    , width = 2
                                                                                    , pad = "0"
                                                                                    )
                                                                 )
                                                          ]]
                                          )
                           , alpha = 0.4
                  ) +
      ggplot2::geom_line(data = res
                         , ggplot2::aes(.data[[var_col]]
                                        , .data[[paste0("q", median(100 * res_q))]]
                                        )
                         , linetype = 1
                         , size = 1.5
                         , alpha = 0.6
                         ) +
      ggplot2::geom_vline(xintercept = tests$year
                          , linetype = 2
                          , colour = "red"
                          ) +
      ggplot2::facet_wrap(facet_form) +
      ggplot2::labs(title = plot_titles
                    , subtitle = sub_title_ribbon
                    )

    if(results$has_cov) {

      p <- p +
        ggplot2::geom_point(data = df %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(dplyr::across(tidyselect::any_of(var_col)))
                                                )
                  , ggplot2::aes(.data[[var_col]]
                                 , .data[[resp_var]]
                                 , colour = .data[[cov_col]]
                                 )
                  ) +
      ggplot2::scale_colour_viridis_c()

    } else {

      p <- p +
        ggplot2::geom_point(data = df %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(dplyr::across(tidyselect::any_of(var_col)))
                                                )
                             , ggplot2::aes(.data[[var_col]]
                                            , .data[[resp_var]]
                                            )
                             )

    }

    results$plot_ribbon <- p


    #------year difference plot--------

    plot_data <- pred %>%
      dplyr::filter(!!rlang::ensym(var_col) == max(recent, na.rm = TRUE)) %>%
      dplyr::group_by(dplyr::across(tidyselect::any_of(cat_col))) %>%
      dplyr::left_join(res %>%
                         dplyr::filter(!!rlang::ensym(var_col) == max(recent, na.rm = TRUE)) %>%
                         envFunc::add_likelihood({{ diff_direction }})
                       )

    if(nrow(plot_data) > 0) {

      results$year_diff_plot <- ggplot2::ggplot(data = plot_data
                                            , ggplot2::aes(.data$diff
                                                           , .data[[diff_category]]
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
                                          , max(recent, na.rm = TRUE)
                                          , " compared to "
                                          , reference
                                          )
                      , x = "Difference"
                      #, y = "IBRA Subregion"
                      , fill = "Likelihood of decrease"
                      , caption = paste0("Red dotted line indicates no change between "
                                         , reference
                                         , " and "
                                         , max(recent, na.rm = TRUE)
                                         )
                      )

    }

    if(do_gc) {

      stuff <- grep("results", ls(), value = TRUE, invert = TRUE)

      rm(list = stuff)

      gc()

    }

    return(results)

  }