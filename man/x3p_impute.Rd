% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/x3p_impute.R
\name{x3p_impute}
\alias{x3p_impute}
\title{Impute missing values}
\usage{
x3p_impute(
  x3p,
  ifout = FALSE,
  ifsave = FALSE,
  dir_name = NULL,
  gif_name = NULL,
  ifplot = FALSE
)
}
\arguments{
\item{x3p}{An \code{x3p} object representing a topographic scan.}

\item{ifout}{A Boolean flag indicating whether the imputation procedure should extrapolate. Set to \code{TRUE} for extrapolation.}

\item{ifsave}{A Boolean flag indicating whether to save the imputation procedure gif.}

\item{dir_name}{A string representing the directory name where the gif should be saved. Required when \code{ifsave = TRUE}.}

\item{gif_name}{A string representing the gif name. Required when \code{ifsave = TRUE}.}

\item{ifplot}{A Boolean flag indicating whether to save ggplot lists in the output attributes. Automatically set to \code{TRUE} when \code{ifsave = TRUE}.}
}
\value{
An \code{x3p} object after imputation.
}
\description{
This function imputes missing values in an \code{x3p} object.
}
\examples{
x3p <- x3p_subsamples[[1]]
insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)

x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
  ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = TRUE
)

attr(x3p_inner_impute, "x3p_impute_0_plot")
attr(x3p_inner_impute, "x3p_impute_1_plot")
attr(x3p_inner_impute, "x3p_impute_n_plot")

if (interactive()) {
  x3p_image_autosize(x3p_inner_impute)
}

}
