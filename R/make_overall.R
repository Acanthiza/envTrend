#' Combine results from other models into a single overall result
#'
#'
#' @param taxa Character. Name of taxa.
#' @param common Character. Common name of taxa.
#' @param year_diff_df Dataframe of differences between a reference and 'recent'
#' year.
#'
#' @return List with components
#' \itemize{
#'   \item{overall}{Dataframe of summarised difference}
#'   \item{overall_text}{A paragraph with rmarkdown formating describing the
#'   result}
#'   \item{overall_plot}{ggplot object with distribution of overall credible
#'   values}
#' }
#' @export
#'
#' @examples
make_overall <- function(taxa
                         , common
                         , year_diff_df
                         ) {

  plot_titles <- bquote(~italic(.(taxa))*":" ~ .(common))

  res <- list()

  res$overall <- year_diff_df %>%
    dplyr::summarise(n = n()
                     , increase = sum(diff > 0)/n
                     , decline = sum(diff < 0)/n
                     , meanEff = mean(diff)
                     , medianEff = median(diff)
                     , cilo = quantile(diff, probs = 0.05)
                     , ciup = quantile(diff, probs = 0.95)
                     ) %>%
    dplyr::mutate(likelihood = map(decline
                                   , ~cut(.
                                          , breaks = c(0,lulikelihood$maxVal)
                                          , labels = lulikelihood$likelihood
                                          , include.lowest = TRUE
                                          )
                                   )
                  ) %>%
    tidyr::unnest(cols = c(likelihood)) %>%
    dplyr::mutate(text = paste0(tolower(likelihood)
                                , " to be declining ("
                                , 100*round(decline,2)
                                , "% chance)"
                                )
                  )

  res$overall_text <- paste0(
    "_"
    , taxa
    , "_"
    , if(!is.null(common)) paste0(" (",common,")")
    , " was "
    , res$overall$text
    , " across "
    , aoi_fullname
    , " based on "
    , n_distinct(year_diff_df$type)
    , " models ("
    , vec_to_sentence(unique(year_diff_df$type))
    , ") using data from "
    , n_distinct(year_diff_df[geo2])
    , " IBRA Subregions ("
    , vec_to_sentence(unique(year_diff_df[geo2]))
    , ")"
  )

  res$overall_plot <- year_diff_df %>%
    dplyr::mutate(likelihood = res$overall$likelihood) %>%
    ggplot(aes(diff,fill = likelihood)) +
    geom_density() +
    geom_vline(aes(xintercept = 0)
               , linetype = 2
               , colour = "red"
               ) +
    scale_fill_viridis_d(drop = FALSE) +
    labs(title = plot_titles
         , subtitle = paste0("Distribution of credible values for change between "
                             , recent
                             ," and "
                             , reference
                             )
         , x = "Difference"
         , y = "Likelihood of difference"
         , fill = "Likelihood of decrease"
         , caption = paste0("Red dotted line indicates no change from ",reference)
         )

  return(res)

}
