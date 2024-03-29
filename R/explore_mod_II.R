
#' Explore (pre-) and (post-) results from `make_mod_res` after `make_ll_model`
#'
#' General workflow is make the model (`make_ll_model`), make the model results
#' (`make_mod_res`) then explore results (`explore_mod_ll`). Might work for
#' other rstanarm models? Not sure anymore.
#'
#' @param model_results List. results from `make_ll_model()`
#' @param plot_title Character. Title used on all plots.
#' @param common
#' @param max_levels
#' @param recent
#' @param limit_preds
#' @param plot_draws
#' @param diff_direction
#' @param diff_category Character name of column in original data to condition
#' on. If left as `NULL` will default to `cat_col` if there is more than one
#' level, or new category `all` if there in not. Useful to condition on
#' `random_col` if desired.
#' @param dot_col Character name of column in original data to colour the
#' original data points by in model results plots. Default is the secondary `x`
#' variable `cov_col`.
#' @param do_gc
#'
#' @return
#' @export
#'
#' @examples
  explore_mod_ll <- function(model_results
                             , plot_title = NULL
                             , max_levels = 15
                             , recent = as.numeric(format(Sys.Date(), "%Y")) - 2
                             , limit_preds = TRUE
                             , plot_draws = 500
                             , diff_direction = c("positive", "negative")
                             , diff_category = NULL
                             , facet_col = cat_col
                             , dot_col = cov_col
                             , do_gc = TRUE
                             ) {

    if(!is.numeric(plot_draws)) plot_draws <- model_results$ndraws

    if(!exists("plot_title")) plot_title <- ""

    diff_direction <- diff_direction[1]

    purrr::walk2(names(model_results)
                 , model_results
                 , assign
                 , envir = environment()
                 )

    rm(model_results)

    results <- as.list(environment())

    `:=` <- rlang::`:=`

    where <- tidyselect:::where


    # divergent plots------

    if(results$num_divergent > 0) {

      np_cp <- bayesplot::nuts_params(mod$stanfit)

      if(length(mod$coefficients) < 6) {

        results$pairs <- bayesplot::mcmc_pairs(as.array(mod)
                                               , np = np_cp
                                               , pars = grep("^b"
                                                             , names(mod$coefficients)
                                                             , value = TRUE
                                                             , invert = F
                                                             )
                                               , off_diag_args = list(size = 0.75)
                                               )

      }

      results$paracord <- bayesplot::mcmc_parcoord(as.array(mod)
                                                   , np = np_cp
                                                   ) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


    }


    # setup explore-------

    results$has_cov <- sum(grepl("cov"
                        , names(mod$coefficients)
                        )
                  ) > 0

    # variables to explore
    if(stats::family(mod)$family == "binomial") {

      df$prop <- df$y / df$y_total

      resp_var <- "prop"

    } else resp_var <- y


    var_exp <- unique(c(resp_var
                        , cov_col
                        , cat_col
                        , var_col
                        , y_total
                        , random_col
                        , dot_col
                        )
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

    # exp character------
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
        ggplot2::labs(title = plot_title
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
        ggplot2::labs(title = plot_title
                      , subtitle = paste0("Boxplots of response variable ("
                                          , resp_var
                                          , ") against character variables"
                                          )
                      )

    }

    # exp numeric------
    if(results$has_numeric) {

      plot_data <- dat_exp %>%
        dplyr::select(where(is.numeric)) %>%
        tidyr::gather(variable, value, 1:ncol(.))

      # Count numeric
      results$count_num <- ggplot2::ggplot(data = plot_data
                                       , ggplot2::aes(.data$value
                                                      , y = after_stat(count)
                                                      )
                                       ) +
        ggplot2::geom_density() +
        ggplot2::facet_wrap(~ .data$variable
                            , scales = "free"
                            ) +
        ggplot2::labs(title = plot_title
                      , subtitle = "Count density function for numeric variables"
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
        ggplot2::labs(title = plot_title
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


    # pairs plot-------

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


      results$resid_plot_norm <- ggplot2::ggplot(results$resid
                                                 , ggplot2::aes(residual)
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

      cov_text <- paste0("At "
                         , gsub("_", " ", cov_col)
                         , " quantile "
                         , cov_q
                         , " = "
                         , cov[1, "cov"]
                         )

    }

    if(exists("tests")) {

      sub_title <-  paste0("Dashed red lines indicate "
                           , var_col
                           , " for comparison (see text)."
                           )

    }

    sub_title_line <- paste0(
      if(exists("sub_title")) paste0(sub_title, "\n")
      , if(results$has_cov) {

        paste0("Lines are "
               , plot_draws
               , " draws from posterior distribution.\n"
               , cov_text
               )

      } else {

        paste0("Lines are "
               , plot_draws
               , " draws from posterior distribution."
               )

      }
    )

    sub_title_ribbon <- paste0(if(exists("sub_title")) paste0(sub_title, "\n")
                               , "Median (thick line) and 90% credible intervals (shaded)."
                               )

    #-------res plot_line-----------

    if(is.numeric(ref)) {

      reference <- max(recent, na.rm = TRUE) + ref

      tests <- tibble::tribble(~type, ~year
                               , "reference", reference
                               , "recent", max(recent, na.rm = TRUE)
                               )

    }


    get_draws <- sort(sample(1 : (length(results$mod$stanfit@stan_args) *
                                    (results$mod$stanfit@stan_args[[1]]$iter - results$mod$stanfit@stan_args[[1]]$warmup)
                                  )
                             , plot_draws
                             )
                      )

    pred_plot_draws <- pred %>%
      dplyr::filter(.draw %in% get_draws)

    if(!is.null(facet_col)) {

      facet_form <- as.formula(paste0(facet_col[1]
                                      , " ~ "
                                      , if(!is.na(facet_col[2])) {

                                        facet_col[2]

                                        } else "."

                                      )
                               )

    } else NULL

    p <- ggplot2::ggplot(data = pred_plot_draws
                         , ggplot2::aes(x = .data[[var_col]])
                         ) +
      ggplot2::geom_line(ggplot2::aes(y = .data$pred
                                      , group = .data$.draw
                                      , linetype = .data$divergent
                                      )
                         , alpha = 0.3
                         ) +
      ggplot2::facet_wrap(facet_form) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                , vjust = 0.5
                                                , hjust = 1
                                                )
                     ) +
      ggplot2::labs(title = plot_title
                    , subtitle = sub_title_line
                    , linetype = "divergent"
                    )

    if(exists("tests")) {

      p <- p +
        ggplot2::geom_vline(xintercept = tests$year
                            , linetype = 2
                            , colour = "red"
                            )

    }

    if(!is.null(dot_col)) {

      if("numeric" %in% class(dat_exp[[dot_col]])) {

        scale_col <- ggplot2::scale_colour_viridis_c()

      } else {

        scale_col <- ggplot2::scale_colour_viridis_d()

      }

      p <- p +
        ggplot2::geom_point(data = dat_exp %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(dplyr::across(var_col))
                                                )
                             , ggplot2::aes(x = .data[[var_col]]
                                           , y = .data[[resp_var]]
                                           , colour = .data[[dot_col]]
                                           )
                             ) +
        scale_col +
        ggplot2::labs(colour = dot_col)

    } else {

      p <- p +
        ggplot2::geom_point(data = dat_exp %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(!!rlang::ensym(var_col))
                                                )
                             , ggplot2::aes(x = .data[[var_col]]
                                           , y = .data[[resp_var]]
                                           )
                            , colour = "blue"
                             )

    }

    results$plot_line <- p


    #------res plot_ribbon-------

    p <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = results$res_divergent
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
                                          , linetype = .data$divergent
                                          , fill = .data$divergent
                                          )
                           , alpha = 0.4
                  ) +
      ggplot2::geom_line(data = res
                         , ggplot2::aes(.data[[var_col]]
                                        , .data[[paste0("q", median(100 * res_q))]]
                                        )
                         , alpha = 0.5
                         ) +
      ggplot2::facet_wrap(facet_form) +
      ggplot2::labs(title = plot_title
                    , subtitle = sub_title_ribbon
                    , linetype = "divergent"
                    , fill = "divergent"
                    )

    if(exists("tests")) {

      p <- p +
        ggplot2::geom_vline(xintercept = tests$year
                            , linetype = 2
                            , colour = "red"
                            )

    }

    if(!is.null(dot_col)) {

      if("numeric" %in% class(dat_exp[[dot_col]])) {

        scale_col <- ggplot2::scale_colour_viridis_c()

      } else {

        scale_col <- ggplot2::scale_colour_viridis_d()

      }

      p <- p +
        ggplot2::geom_point(data = df %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(dplyr::across(tidyselect::any_of(var_col)))
                                                )
                  , ggplot2::aes(.data[[var_col]]
                                 , .data[[resp_var]]
                                 , colour = .data[[dot_col]]
                                 )
                  ) +
      scale_col

    } else {

      p <- p +
        ggplot2::geom_point(data = df %>%
                              dplyr::inner_join(res %>%
                                                  dplyr::distinct(dplyr::across(tidyselect::any_of(var_col)))
                                                )
                            , ggplot2::aes(.data[[var_col]]
                                           , .data[[resp_var]]
                                           )
                            , colour = "blue"
                            )

    }

    results$plot_ribbon <- p


    #------year difference plot--------

    plot_data <- pred %>%
      dplyr::filter(!is.na(diff)) %>%
      {if(is.numeric(ref)) (.) %>%
          dplyr::filter(!!rlang::ensym(var_col) == max(recent, na.rm = TRUE)) else
            (.) %>%
          dplyr::filter(!!rlang::ensym(var_col) == max(!!rlang::ensym(var_col), na.rm = TRUE))
        } %>%
      dplyr::inner_join(res %>%
                         envFunc::add_likelihood({{ diff_direction }})
                       )

    if(nrow(plot_data) > 0) {

      if(all(is.null(results$cat_col), is.null(diff_category))) {

        diff_category <- "all"

        plot_data <- plot_data %>%
          dplyr::mutate(all = "all")

      } else if (is.null(diff_category)) {

        diff_category <- results$cat_col

      }

      p <- ggplot2::ggplot(data = plot_data
                                            , ggplot2::aes(.data$diff
                                                           , .data[[diff_category]]
                                                           , fill = .data$likelihood
                                                           #, height = ..density..
                                                           )
                                            ) +
        ggplot2::geom_ribbon(stat = "density"
                            , outline.type = "upper"
                            , aes(ymin = after_stat(group)
                                  , ymax = after_stat(group + ndensity)
                                  )
                            , trim = TRUE
                            ) +
        ggplot2::scale_fill_viridis_d(drop = FALSE)  +
        ggplot2::labs(title = plot_title
                      , x = "Difference"
                      #, y = "IBRA Subregion"
                      , fill = paste0("Likelihood of "
                                      , if(grepl("pos", diff_direction)) "increase"
                                      , if(grepl("neg", diff_direction)) "decrease"
                                      , "\nrelative to reference:"
                                      , paste0("\n", ref)
                                      )
                      )

      if(ps_thresh != 0) {

        p <- p +
          ggplot2::geom_vline(xintercept = c(ps_thresh, -ps_thresh)
                              , linetype = 2
                              , colour = "blue"
                              ) +
          labs(caption = paste0("Area between the blue dotted lines is practical equivalence (i.e. no practical change)"))

      } else {

        p <- p +
          ggplot2::geom_vline(xintercept = 0
                              , linetype = 2
                              , colour = "blue"
                              ) +
          labs(caption = paste0("Blue dotted line indicates no change"))

      }


      results$year_diff_plot <- p

    }


    # Clean up-------

    if(do_gc) {

      stuff <- grep("results", ls(), value = TRUE, invert = TRUE)

      rm(list = stuff)

      gc()

    }

    return(results)

  }
