

#' Plot with increasing credible range error bars decreasing in size around
#' credible intervals
#'
#' Option to include original data. Currently needs columns `q01`, `q05`, `q25`,
#'  `q50`, `q75`, `q95` and `q99` in `df`.
#'
#' @param df Dataframe with `y`, `y_prop` etc
#' @param orig_data Dataframe with original data
#' @param x,y Chracter name of columns in `df` and `orig_data` with response
#' variable
#' @param facet_col Character name of column in `df` with a column to facet on.
#' @param y_name Character. Name to give to `y` in plot
#' @param likelihood_col Character name of column in `df` with likelihoods
#'
#' @return `ggplot`
#' @export
#'
#' @examples
  cat_plot <- function(df
                       , orig_data = NULL
                       , y = "prop"
                       , x = "year"
                       , facet_col = "LSA"
                       , y_name = "reporting rate"
                       , likelihood_col = "pos_raw"
                       ) {

    df_plot <- df %>%
      envFunc::add_likelihood(col = {{ likelihood_col }})

    p <- ggplot(data = df_plot
                , aes(x = !!rlang::ensym(x)
                      , xend = !!rlang::ensym(x)
                      )
                )

     p <- p +
       geom_segment(data = df_plot
                    , aes(y = q01, yend = q99, colour = likelihood)
                    , size = 1
                    , alpha = 0.5
                    ) +
       geom_segment(data = df_plot
                    , aes(y = q10, yend = q90, colour = likelihood)
                    , size = 2
                    , alpha = 0.5
                    ) +
       geom_segment(data = df_plot
                    , aes(y = q25, yend = q75, colour = likelihood)
                    , size = 3
                    , alpha = 0.5
                    ) +
       geom_point(data = df_plot
                  , aes(y = q50
                        , colour = likelihood
                        , shape = NULL
                        )
                  , size = 3
                  ) +
       scale_colour_viridis_d(drop = FALSE
                              , na.value = "dark grey"
                              ) +
       labs(y = y_name
            , colour = paste0("likelihood\nof "
                              , if(grepl("pos", likelihood_col)) "increase" else "decrease"
                              , " in\n"
                              , y_name
                              )
            )

     if(!is.null(orig_data)) {

       if(facet_col %in% names(df_plot)) {

         if(any(grepl("uncertainty|gaussian", as.character(levels(df_plot[facet_col]))))) {

           use_facet <- as.character(unique(df_plot[facet_col]))

           orig_data <- orig_data %>%
             dplyr::mutate(facet = use_facet)

         }

       }

       p <- p +
         geom_point(data = orig_data %>%
                      dplyr::filter(!!rlang::ensym(x) %in% unique(df_plot[x]))
                    , aes(y = !!ensym(y))
                    , colour = "black"
                    , width = 0.2
                    , height = 0.02
                    #, alpha = 0.5
                    )

     }

     if(facet_col %in% names(df_plot)) {

       p <- p +
         facet_wrap(as.formula(paste0("~", facet_col))
                    , scales = "free_y"
                    )

     }

     return(p)

  }
