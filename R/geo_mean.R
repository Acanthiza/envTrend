

#' Geometric mean
#'
#'Defined as:
#' \loadmathjax
#' \mjdeqn{ \left(\prod_{i=1}^n x_i\right)^\frac{1}{n} = \sqrt[n]{x_1 x_2 \cdots x_n}
#' }{ASCII representation}
#'
#' Based heavily on the `geoMean()` function in the \href{https://alexkowa.github.io/EnvStats/}{EnvStats}
#' package by \href{https://github.com/alexkowa}{alexkowa}, and various
#' \href{https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in}{stackoverflow posts}.
#'
#' @param x Numeric
#' @param na.rm Remove any `NA` values prior to calculation
#'
#' @return
#' @export
#'
#' @examples
#' x <- rnorm(100, 1, 0.3)
#' geo_mean(x)
#'
#' x <- c(x, NA)
#' geo_mean(x)
#' geo_mean(x, na.rm = TRUE)
#'
geo_mean <- function (x
                      , na.rm = FALSE
                      ) {

  if(!is.vector(x, mode = "numeric") || is.factor(x)) {

    stop("'x' must be a numeric vector")

  }

  wna <- is.na(x)

  if(sum(wna > 0)) {

    if(na.rm) x <- x[!wna] else return(NA)

  }

  if(any(x <= 0, na.rm = na.rm)) {

    warning("Non-positive values in 'x'")

    return(NA)

  } else return(exp(mean(log(x))))

}
