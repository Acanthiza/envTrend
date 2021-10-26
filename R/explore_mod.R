
#' Explore (pre- and most-) model results.
#'
#' @param taxa Scientific name of taxa (for labelling things).
#' @param common Common name of taxa (also used in labels).
#' @param df Data used for model saved at `mod_path`.
#' @param mod_path Path to saved model.
#' @param mod_type Type of model (e.g. 'reporting rate', or 'occupancy')
#' @param resp_var What is the response variable?
#' @param exp_var What are the variables to include in model exploration?
#' @param max_levels Maximum number of classes to include in categorial plots.
#' @param draws Number of draws from posterior distribution to display in plots.
#' @param post_groups Character names of variables to include in model results.
#' @param tests Years at which to predict (and compare change) as dataframe
#' with type = c("reference", "recent") and year = four-digit year corresponding
#' to references and recent.
#' @param re_run Logical. If file `gsub("\\.rds", "_res.rds", mod_path)` already
#' exists, should it be re-run?
#'
#' @return A list with components
#' \itemize{
#'   \item{count_char}{ggplot object. Count of levels within character variables.}
#'   \item{y_vs_char}{ggplot object. Response variable vs character variables.}
#'   \item{count_num}{ggplot object. Histogram of each numeric variable.}
#'   \item{y_vs_num}{ggplot object. Response variable vs numeric variables with `geom_smooth()`.}
#'   \item{pairs}{`GGally::ggpairs` object from `df`.}
#'   \item{pred}{Dataframe of `posterior_predict` for levels of interest.}
#'   \item{res}{`pred` summarised by `post_groups`.}
#' }
#' @export
#'
#' @examples
  explore_mod <- function(taxa
                          , common
                          , df
                          , mod_path
                          , mod_type
                          , resp_var = "prop"
                          , exp_var
                          , max_levels = 30
                          , draws = 200
                          , post_groups
                          , tests = NULL
                          , re_run = FALSE
                          ) {

    print(taxa)

    if(isTRUE(is.null(tests))) {

      tests <- tibble::tribble(~type, ~year
                                 , "reference", 2000
                                 , "recent", 2015
                                 ) %>%
        tidyr::unnest(cols = c(year))

    }

    reference <- tests$year[tests$type == "reference"]

    recent <- tests$year[tests$type == "recent"]



    context <- c(post_groups, mod_type)

    #-------do_run--------

    out_file <- gsub("\\.rds"
                     , "_res.rds"
                     , mod_path
                     )


    do_run <- if(file.exists(out_file)) {

      if(re_run) TRUE else FALSE

    } else TRUE


    if(do_run) {

      out_dir <- dirname(mod_path)

      mod <- read_rds(mod_path)

      #-------setup explore-------

      res <- list()

      plot_titles <- bquote(~italic(.(taxa))*":" ~ .(common))

      has_ll <- sum(grepl("list_length",names(mod$coefficients))) > 0

      if(has_ll) context <- c(context, "list_length")

      if(!resp_var %in% names(df)) df <- df %>%
        dplyr::group_by(across(any_of(exp_var))) %>%
        dplyr::summarise(!!ensym(resp_var) := sum(success)/n()) %>%
        dplyr::ungroup()


      # variables to explore
      var_exp <- c(resp_var
                   , colnames(df)
      ) %>%
        unique()

      dat_exp <- df %>%
        dplyr::select(all_of(var_exp))

      has_numeric <- dat_exp %>%
        dplyr::select(-1) %>%
        dplyr::select(where(is.numeric)) %>%
        ncol() %>%
        `>` (0)

      has_character <- dat_exp %>%
        dplyr::select(-1) %>%
        dplyr::mutate(across(where(is.factor),as.character)) %>%
        dplyr::select(where(is.character)) %>%
        ncol() %>%
        `>` (0)

      # Character variables
      if(has_character) {

        # count character
        res$count_char <- ggplot(dat_exp %>%
                                   dplyr::mutate(across(where(is.factor),as.character)) %>%
                                   dplyr::select_if(is.character) %>%
                                   tidyr::gather(variable,value,1:ncol(.)) %>%
                                   dplyr::group_by(variable) %>%
                                   dplyr::mutate(levels = n_distinct(value)) %>%
                                   dplyr::ungroup() %>%
                                   dplyr::filter(levels < max_levels)
        ) +
          geom_histogram(aes(value)
                         , stat = "count"
          ) +
          facet_wrap(~variable
                     , scales = "free"
          ) +
          theme(axis.text.x = element_text(angle = 90
                                           , vjust = 0.5
                                           , hjust = 1
          )
          ) +
          labs(title = plot_titles
               , sub_title = "Count of levels within character variables"
          )

        # resp_var vs character
        res$y_vs_char <-ggplot(dat_exp %>%
                                 dplyr::mutate({{resp_var}} := factor(!!ensym(resp_var))) %>%
                                 dplyr::mutate_if(is.factor,as.character) %>%
                                 dplyr::select_if(is.character) %>%
                                 dplyr::mutate({{resp_var}} := as.numeric(!!ensym(resp_var))) %>%
                                 tidyr::gather(variable,value,2:ncol(.)) %>%
                                 dplyr::group_by(variable) %>%
                                 dplyr::mutate(levels = n_distinct(value)) %>%
                                 dplyr::ungroup() %>%
                                 dplyr::filter(levels < max_levels)
        ) +
          geom_boxplot(aes(value,!!ensym(resp_var))) +
          facet_wrap(~variable, scales = "free") +
          theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
          labs(title = plot_titles
               , sub_title = paste0("Boxplots of response variable (",resp_var,") against character variables")
          )

      }

      # Numeric variables
      if(has_numeric) {

        # Count numeric
        res$count_num <- ggplot(dat_exp %>%
                                  dplyr::select(where(is.numeric)) %>%
                                  tidyr::gather(variable,value,1:ncol(.))
                                , aes(value)
        ) +
          geom_histogram() +
          facet_wrap(~variable, scales = "free") +
          labs(title = plot_titles
               , sub_title = "Histograms of numeric variables"
          )

        # resp_var vs. Numeric
        res$y_vs_num <- ggplot(dat_exp %>%
                                 dplyr::select(any_of(var_exp)) %>%
                                 dplyr::select(where(is.numeric)) %>%
                                 tidyr::gather(variable,value,2:ncol(.)) %>%
                                 dplyr::arrange(!!ensym(resp_var))
                               , aes(value,!!ensym(resp_var))
        ) +
          geom_point(alpha = 0.5) +
          geom_smooth() +
          facet_wrap(~variable, scales = "free") +
          theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
          labs(title = plot_titles
               , sub_title = paste0("Numeric variables plotted against response variable (",resp_var,")")
          )

      }

      res$pairs <- GGally::ggpairs(dat_exp %>%
                                     dplyr::mutate(across(where(is.character),factor)) %>%
                                     dplyr::select(where(~is.numeric(.x)|is.factor(.x) & n_distinct(.x) < 15)) %>%
                                     dplyr::mutate(across(where(is.factor),factor))
      ) +
        theme(axis.text.x=element_text(angle=90, vjust=0.5))


      #-------residuals-------

      if(length(residuals(mod)) == nrow(df)) {

        res$resid <- tibble(residual = residuals(mod)
                            , fitted = fitted(mod)
        ) %>%
          dplyr::bind_cols(df)


        res$resid_plot <- ggplot(res$resid, aes(fitted,residual)) +
          geom_point(size = 2) +
          geom_hline(aes(yintercept = 0), linetype = 2, colour = "red") +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) %>%
          scale_colour_viridis_d(end=0.9)


        res$resid_plot_num <- if(has_numeric) {

          ggplot(res$resid %>%
                   dplyr::select_if(is.numeric) %>%
                   tidyr::pivot_longer(2:ncol(.))
                 , aes(value,residual)
          ) +
            geom_point(size = 2) +
            geom_smooth(method = "lm")  +
            geom_hline(aes(yintercept = 0), linetype = 2, colour = "red") +
            facet_wrap(~name
                       , scales = "free_x"
            ) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            scale_colour_viridis_d()

        } else NULL


        res$resid_plot_char <- if(has_character) {

          ggplot(res$resid %>%
                   dplyr::mutate(across(where(is.factor),as.character)) %>%
                   dplyr::select(1,where(is.character)) %>%
                   tidyr::pivot_longer(2:ncol(.)) %>%
                   dplyr::group_by(name) %>%
                   dplyr::mutate(levels = n_distinct(value)) %>%
                   dplyr::ungroup() %>%
                   dplyr::filter(levels < max_levels)
                 , aes(value,residual)
          ) +
            geom_boxplot() +
            geom_hline(aes(yintercept = 0), linetype = 2, colour = "red") +
            facet_wrap(~name, scales = "free") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

        } else NULL

      }


      #---------post explore-------

      if(family(mod)$family == "beta") class(mod) <- unique(c(class(mod),"betareg"))

      is_binomial_mod <- family(mod)$family == "binomial"

      res$pred <- df %>%
        dplyr::distinct(across(any_of(post_groups))) %>%
        dplyr::mutate(list_length = if(has_ll) median(df$list_length) else NULL
                      , list_length_log = if(has_ll) log(list_length) else NULL
                      , col = row.names(.)
                      , success = if(is_binomial_mod) 0 else NULL
                      , trials = if(is_binomial_mod) 100 else NULL
        ) %>%
        dplyr::left_join(as_tibble(posterior_predict(mod
                                                     , newdata = .
                                                     , re.form = NA#insight::find_formula(mod)$random
                                                     , type = "response"
        )
        ) %>%
          tibble::rownames_to_column(var = "row") %>%
          tidyr::gather(col,value,2:ncol(.))
        ) %>%
        dplyr::mutate(rawValue = as.numeric(value)
                      , value = if(is_binomial_mod) rawValue/trials else rawValue
        )

      res$res <- res$pred %>%
        dplyr::group_by(across(any_of(post_groups))) %>%
        dplyr::summarise(n = n()
                         , nCheck = nrow(as_tibble(mod))
                         , modMean = mean(value)
                         , modMedian = quantile(value, 0.5)
                         , modci90lo = quantile(value, 0.05)
                         , modci90up = quantile(value, 0.95)
                         , text = paste0(round(modMedian,2)," (",round(modci90lo,2)," to ",round(modci90up,2),")")
        ) %>%
        dplyr::ungroup()


      #------res plot data-------

      plot_data <- df %>%
        dplyr::distinct(across(any_of(post_groups))) %>%
        dplyr::mutate(success = 0
                      , trials = 100
        ) %>%
        dplyr::full_join(tibble(probs = quant_probs[2]) %>%
                           {if(has_ll) (.) %>%
                               dplyr::mutate(list_length = map_dbl(probs
                                                                   ,~quantile(unique(df$list_length)
                                                                              , probs = .
                                                                   )
                               )
                               , list_length_log = log(list_length)
                               , length = paste0("At list length quantile ",probs," = ",list_length)
                               ) else (.)
                           }
                         , by = character()
        ) %>%
        tidybayes::add_fitted_draws(mod
                                    , n = draws
                                    , re_formula = NA
        )

      sub_title <-  if(has_ll) {

        paste0("List length corrected reporting rate.\nDashed red lines indicate years for comparison (see text).")

      } else {

        paste0(mod_type,".\nDashed red lines indicate years for comparison (see text).")

      }

      sub_title_line <- paste0(
        sub_title
        , if(has_ll) {

          paste0("\nLines are ",draws," draws from posterior distribution.\n",unique(plot_data$length))

        } else {

          paste0("\nLines are ",draws," draws from posterior distribution.")

        }
      )

      sub_title_ribbon <- paste0(sub_title,"\nMedian (thick line) and 90% credible intervals (shaded).")

      #-------res plot_line-----------

      p <- plot_data %>%
        ggplot(aes(x = year, y = !!ensym(resp_var))) +
        geom_line(aes(y = .value, group = .draw)
                  , alpha = 0.5
        ) +
        geom_vline(xintercept = tests$year
                   , linetype = 2
                   , colour = "red"
        ) +
        facet_wrap(as.formula(paste0("~ ",geo2))) +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(title = plot_titles
             , sub_title = sub_title_line
        )

      if(has_ll) p <- p +
        geom_jitter(data = df
                    ,aes(year
                         , !!ensym(resp_var)
                         , colour = list_length
                    )
                    , width = 0.1
                    , height = 0.05
        ) +
        scale_colour_viridis_c() +
        labs(colour = "List length")

      if(!has_ll) p <- p +
        geom_jitter(data = df
                    ,aes(year
                         , !!ensym(resp_var)
                         , colour = trials
                    )
                    , width = 0.1
                    , height = 0.01
        ) +
        scale_colour_viridis_c()

      res$plot_line <- p


      #------res plot_ribbon-------

      p <- ggplot() +
        geom_ribbon(data = res$res
                    , aes(year,modMean,ymin = modci90lo, ymax = modci90up)
                    , alpha = 0.4
        ) +
        geom_line(data = res$res
                  , aes(year,modMean)
                  , linetype = 1
                  , size = 1.5
        ) +
        geom_vline(xintercept = tests$year, linetype = 2, colour = "red") +
        facet_wrap(as.formula(paste0("~ ", geo2))) +
        labs(title = plot_titles
             , sub_title = sub_title_ribbon
        )

      if(has_ll) p <- p +
        geom_jitter(data = df
                    ,aes(year
                         ,!!ensym(resp_var)
                         , colour = list_length
                    )
                    , width = 0.1
                    , height = 0.05
        ) +
        scale_colour_viridis_c() +
        labs(colour = "List length")

      if(!has_ll) p <- p +
        geom_jitter(data = df
                    ,aes(year
                         , !!ensym(resp_var)
                         , colour = trials
                    )
                    , width = 0.1
                    , height = 0.05
        ) +
        scale_colour_viridis_c() +
        labs(colour = "Trials")

      res$plot_ribbon <- p


      #------year difference df-----------

      res$year_diff_df <- df %>%
        dplyr::distinct(across(any_of(post_groups[!post_groups %in% time_cols]))) %>%
        dplyr::full_join(test_years
                         , by = character()
                         ) %>%
        dplyr::mutate(list_length = if(has_ll) median(df$list_length) else NULL
                      , list_length_log = if(has_ll) log(list_length) else NULL
                      , col = row.names(.)
                      , success = 0
                      , trials = 100
                      , nCheck = nrow(as_tibble(mod))
                      , mod_type = mod_type
                      , taxa = taxa
                      , common = common
                      ) %>%
        dplyr::left_join(as_tibble(posterior_predict(mod
                                                     , newdata = .
                                                     , re.form = NA#insight::find_formula(mod)$random
                                                     )
                                   ) %>%
                           tibble::rownames_to_column(var = "row") %>%
                           tidyr::gather(col,value,2:ncol(.))
                         ) %>%
        dplyr::select(-c(col)) %>%
        dplyr::mutate(value = as.numeric(if(is_binomial_mod) value/trials else value)) %>%
        tidyr::pivot_wider(names_from = "type"
                           , values_from = c("year","value")
                           ) %>%
        setNames(gsub("\\d{4}","",names(.))) %>%
        dplyr::mutate(diff = as.numeric(value_recent-value_reference))


      #-------year difference res---------

      res$year_diff_res <- res$year_diff_df %>%
        dplyr::group_by(across(any_of(context))) %>%
        dplyr::summarise(n = n()
                         , nCheck = unique(nCheck)
                         , lower = sum(diff < 0)/nCheck
                         , higher = sum(diff > 0)/nCheck
                         , meanDiff = mean(diff)
                         , medianDiff = median(diff)
                         , cilo = quantile(diff, probs = 0.05)
                         , ciup = quantile(diff, probs = 0.95)
                         , reference = unique(year_reference)
                         , recent = unique(year_recent)
                         ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(likelihood = map(lower
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
                                    , geo2
                                    , " IBRA Subregion ("
                                    , 100*round(lower,2)
                                    , "% chance)"
                                    )
                      , text = gsub("in Kangaroo Island","on Kangaroo Island",text)
                      )

      #------year difference plot--------

      res$year_diff_plot <- res$year_diff_df %>%
        dplyr::group_by(across(any_of(geo2))) %>%
        dplyr::mutate(lower = sum(diff<0)/nCheck) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(likelihood = map(lower
                                       , ~cut(.
                                              , breaks = c(0,lulikelihood$maxVal)
                                              , labels = lulikelihood$likelihood
                                              , include.lowest = TRUE
                                              )
                                       )
                      ) %>%
        tidyr::unnest(cols = c(likelihood)) %>%
        dplyr::mutate(likelihood = fct_expand(likelihood,levels(lulikelihood$likelihood))) %>%
        ggplot(aes(diff
                   , !!ensym(geo2)
                   , fill = likelihood
                   )
               ) +
        ggridges::geom_density_ridges() +
        geom_vline(aes(xintercept = 0)
                   , linetype = 2
                   , colour = "red"
                   ) +
        scale_fill_viridis_d(drop = FALSE) +
        labs(title = plot_titles
             , sub_title = paste0("Difference in "
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

      write_rds(res
                , out_file
                )

    } else {

      print("alredy done")

      }

    }
