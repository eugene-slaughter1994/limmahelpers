#' Format topTable of lmFit contrast and display results in datatable
#'
#' @param fit An MArrayLM object produced by limma::lmFit or limma::contrasts.fit
#' @param contrast String indicating the column name/contrast of interest from the linear model
#'
#' @return A HTML widget to display rectangular data
#' @export
#'
#' @examples
#' design <- stats::model.matrix(~ 0 + grouping, coldata)
#' fit <- limma::lmFit(metabolites, design)
#' format_toptable(fit, contrast = "HFLVAD")
format_toptable <- function(fit, contrast) {

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
