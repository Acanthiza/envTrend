% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_rr_model.R
\name{make_rr_model}
\alias{make_rr_model}
\title{Make, and save to disk, a reporting rate model.}
\usage{
make_rr_model(taxa, df, out_file, geo_cols, random_col = "grid_l", ...)
}
\arguments{
\item{taxa}{Character name of taxa for which model is being run. Used to name
output file.}

\item{df}{Cleaned, filtered data frame.}

\item{out_file}{Path into which model results are saved.}

\item{geo_cols}{Character name of columns in \code{df} containing geographic
context. Last index in \code{geo_cols} is used as primary analysis level.}

\item{random_col}{Character name of column in \code{df} containing random factor
for model. This is usually the larger of two (probably raster) grid cell
sizes.}

\item{...}{Passed to rstanarm::stan_gamm4 (e.g. chains, iter)}
}
\value{
\code{out_file}
}
\description{
Make, and save to disk, a reporting rate model.
}
