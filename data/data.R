
#' Dataframe of likelihood thresholds and definitions
#'
#' Following the intergovernmental panel on climate change [IPCC](https://www.ipcc.ch/site/assets/uploads/2017/08/AR5_Uncertainty_Guidance_Note.pdf).
#'
#' @format A data frame with `r nrow(lulikelihood)` rows and
#' `r ncol(lulikelihood)` variables:
#' \describe{
#'   \item{likelihood}{Factor. Definitions.}
#'   \item{maxVal}{Double. Maximum proportion to fall in likelihood category}
#'   \item{range}{Factor. Result of `cut(maxVal, breaks = c(0,.$maxVal))`}
#'   \item{loose}{Factor with `r levels(lulikelihood$loose)` levels}
#'   \item{very}{Factor with `r levels(lulikelihood$very)` levels}
#'   \item{extreme}{Factor with `r levels(lulikelihood$extreme)` levels}
#'   \item{exceptional}{Factor with `r levels(lulikelihood$exceptional)` levels}
#'   ...
#' }
"lulikelihood"

