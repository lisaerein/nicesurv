% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggsurv.R
\name{ggsurv}
\alias{ggsurv}
\title{Lisa's Survival Plot}
\usage{
ggsurv(
  sfit,
  surv.lty = 1,
  surv.size = 1,
  surv.col = NA,
  ci = TRUE,
  ci.lty = 2,
  ci.size = 0.5,
  ci.ribbon = TRUE,
  ci.alpha = 0.2,
  cens = TRUE,
  cens.col = "red",
  cens.size = 2,
  cens.shape = 3,
  groups = NA,
  grlabs = NA,
  grname = NA,
  perc = FALSE,
  xlab = "\\nTime",
  ylab = "Survival\\n",
  xlim = NA,
  ylim = c(0, 1),
  xby = NA,
  xbrlabs = NA,
  yby = 0.1,
  risktab = FALSE,
  risktab.times = NA,
  risktab.label = "Number at risk",
  risktab.margins = c(0, 0, 0.2, 0.15),
  risktab.events = TRUE,
  main = "",
  plab = NA,
  grid = c(TRUE, TRUE),
  themebw = TRUE,
  citrans = FALSE
)
}
\arguments{
\item{sfit}{survfit object (REQUIRED).}

\item{surv.lty}{Numeric vector for line type of survival lines. Default = 1.}

\item{surv.size}{Number for size of survival lines. Default = 1.}

\item{surv.col}{Character vector for color of survival lines. Default = "black".}

\item{ci}{Logical indicator to plot confidence intervals. Default = TRUE.}

\item{ci.lty}{Number for line type of CI lines. Default = 2.}

\item{ci.size}{Number for size of CI lines, Default = 1.}

\item{ci.ribbon}{Logical indicator to use ribbons instead of lines for CI area. Default = FALSE.}

\item{ci.alpha}{Number between 0 and 1 for shading (alpha) of CI ribbon. Default = 0.20.}

\item{cens}{Logical indicator to plot markers for censoring. Default = TRUE.}

\item{cens.col}{Character for color of censoring symbols. Default = "red".}

\item{cens.shape}{Number for censoring symbol (pch graphical parameter). Default = 3 (plus sign).}

\item{groups}{Character vector for groups as listed in dataset. Default = NA (no groups).}

\item{grlabs}{Character vector for group levels, must be in same order as groups. Default = NA (no groups or use group levels from dataset).}

\item{grname}{Character label for group legend name. Default = NA (no legend title).}

\item{perc}{Logical indicator to show y axis as percentages. Default = FALSE.}

\item{xlab}{Character label for x axis. Default = "Time".}

\item{ylab}{Character label for y axis. Default = "Survival".}

\item{xlim}{Numeric vector of length 2 for x axis min and max. Default = NA using 0 to data maximum.}

\item{ylim}{Numeric vector of length 2 for y axis min and max. Default = c(0,1).}

\item{xby}{Number for x axis major tick marks. Default = NA (ggplot selected).}

\item{yby}{Numeric for y axis major tick marks. Default = 0.1.}

\item{risktab}{Logical indicator to include risk table at bottom of plot. Default = FALSE.}

\item{risktab.times}{Numeric vector of times for risk table. Default = NA (Includes times for x axis tick marks).}

\item{risktab.label}{Character label for risk table. Default = "Number at risk".}

\item{risktab.margins}{Numeric vector for plot margins in format c(top, right, bottom, left) where each value is a proportion of plot dimensions.
Adjustments may be needed to accomodate long group names or a large number of groups. Default = c(0.00, 0.00, 0.20, 0.15).}

\item{risktab.events}{Logical indicator to include number of events (for non-stratified plots only). Default = TRUE.}

\item{main}{Character label for plot title. Default = " ".}

\item{plab}{Character for annotation in top right corner (for p-value or other notes). Default = NA (no annotation).}

\item{grid}{Logical vector of length 2 to plot major gridlines in x and y directions. Default = c(TRUE, TRUE).
If one value is provided it will apply to both directions.}

\item{themebw}{Indicator for use of ggplot2 theme_bw. Default = TRUE.}

\item{citrans}{Logical indicator whether to apply 1-x transformation to plot cumulative incidence. Default = FALSE.}
}
\description{
This function creates a survival plot from a survfit object using ggplot2 graphics.
}
\keyword{KM}
\keyword{Lisa}
\keyword{consulting}
\keyword{curve}
\keyword{ggplot2}
\keyword{plot}
\keyword{summary}
\keyword{survival}
