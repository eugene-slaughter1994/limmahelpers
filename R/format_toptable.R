#' Applying empirical Bayes moderation, formatting topTable results, and producing datatable of results
#'
#' @param fit an MArrayLM object produced by limma::lmFit or limma::contrasts.fit
#' @param contrast column name specifying which contrast of the linear model is of interest
#' @param cont.matrix numeric matrix with rows corresponding to coefficients in fit and columns containing contrasts
#'
#' @return an HTML widget to display rectangular data using DT::datatable
#' @export
#'
#' @examples
format_toptable <- function(fit, contrast, cont.matrix) {

  fit2 <- limma::contrasts.fit(fit, cont.matrix)

  fit2 <- limma::eBayes(fit2)

  toptable <- limma::topTable(fit2, contrast, number = nrow(fit2)) |>
                dplyr::mutate(FC = 2^logFC,
                              P.Value = round(P.Value, 4),
                              P.Value = dplyr::if_else(P.Value < 0.001, "< 0.001",
                                                       as.character(round(P.Value, 3))),
                              adj.P.Val = round(adj.P.Val, 4),
                              adj.P.Val = dplyr::if_else(adj.P.Val < 0.001, "< 0.001",
                                                         as.character(round(adj.P.Val, 3))),
                              dplyr::across(tidyselect::where(is.numeric), ~round(.x, 2))) |>
                dplyr::select(logFC, FC, everything())

  DT::datatable(toptable)

}
