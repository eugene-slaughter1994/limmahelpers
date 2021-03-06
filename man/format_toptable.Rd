% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_toptable.R
\name{format_toptable}
\alias{format_toptable}
\title{Format the topTable of an lmFit contrast and display results in a datatable}
\usage{
format_toptable(fit, contrast)
}
\arguments{
\item{fit}{An MArrayLM object produced by limma::lmFit or limma::contrasts.fit}

\item{contrast}{String indicating the column name/contrast of interest from the linear model}
}
\value{
A HTML widget to display rectangular data
}
\description{
Format the topTable of an lmFit contrast and display results in a datatable
}
\examples{
design <- stats::model.matrix(~ 0 + disease, coldata)

colnames(design) <- c("Healthy", "Disease1", "Disease2")

fit <- limma::lmFit(metabolites, design)

format_toptable(fit, contrast = "Disease1")
}
