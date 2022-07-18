
adj_to_p <- function(fit, contrast){

  df <- limma::topTable(fit, coef = contrast, number = nrow(fit))

  if ((df |> dplyr::filter(adj.P.Val < 0.05) |> nrow()) == 0){

    r <- 1

  } else {

    r <- df |>
          dplyr::mutate(rn = dplyr::row_number()) |>
          dplyr::filter(adj.P.Val <= 0.05) |>
          dplyr::pull(rn) |>
          max() + 1
  }

  n <- nrow(df) + 1

  lp10 <- -log10(r / n * 0.05)

  lp10

}


#' Generate Volcano Plot for lmFit Contrast
#'
#' @param fit An MArrayLM object produced by limma::lmFit or limma::contrasts.fit
#' @param contrast String indicating the column name/contrast of interest from the linear model
#' @param ref_line If TRUE, add a reference line for the -log10 adjusted p-value corresponding
#'   to an adjusted p-value of 0.05
#' @param title
#'
#' @return
#' @export
#'
#' @examples
volcano_plot <- function(fit, contrast, ref_line = TRUE, title){

  p <- limma::topTable(fit, coef = contrast, number = nrow(fit)) |>
    dplyr::mutate(log10p = -log10(P.Value),
                  up_down = dplyr::if_else(P.Value < 0.05 & logFC < 0, "Down",
                            dplyr::if_else(P.Value < 0.05 & logFC >= 0, "Up", "Non-sig."))) |>
    tibble::rownames_to_column("ID") |>
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = logFC, y = log10p, colour = up_down, label = ID),
                        size = 1, alpha = 2/3) +
    ggplot2::scale_color_manual(name = "Direction of Difference",
                                values = c("Down" = "red", "Up" = "blue", "Non-sig." = "grey")) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::labs(
      title = title,
      y = "-log10(p)",
      x = "log fold change"
    )

  if (ref_line) {

    p <- p  + ggplot2::geom_hline(aes(yintercept = adj_to_p(contrast)),
                                  linetype = "dashed", size = 0.25)

  }

  plotly::ggplotly(p, tooltip = c("x", "y", "label"), width = 1000, height = 600)

}
