
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


#' Generate a volcano plot for an lmFit contrast
#'
#' @param fit An MArrayLM object produced by limma::lmFit or limma::contrasts.fit
#' @param contrast String indicating the column name/contrast of interest
#' @param ref_line If TRUE, add a reference line for the -log10 adjusted p-value corresponding \cr
#'   to an adjusted p-value of 0.05
#' @param title String assigned as the title of the volcano plot
#'
#' @return
#' @export
#'
#' @examples
#' design <- model.matrix(~ 0 + disease, coldata)
#'
#' colnames(design) <- c("Healthy", "Disease1", "Disease2")
#'
#' cont.matrix <- limma::makeContrasts("HealthyvsDisease1" = Disease1-Healthy,
#'                                     "HealthyvsDisease2" = Disease2-Healthy,
#'                                     "Disease1vsDisease2" = Disease2-Disease1,
#'                                     levels = design)
#'
#' fit <- limma::lmFit(metabolites, design = design)
#'
#' fit2 <- limma::contrasts.fit(fit, cont.matrix)
#'
#' fit2 <- limma::eBayes(fit2)
#'
#' volcano_plot(fit2, contrast = "HealthyvsDisease1", title = "Volcano Plot: Healthy vs. Disease 1")
volcano_plot <- function(fit, contrast, ref_line = TRUE, title){

  dat <- limma::topTable(fit, coef = contrast, number = nrow(fit)) |>
         dplyr::mutate(log10p = -log10(P.Value),
                       up_down = dplyr::if_else(P.Value < 0.05 & logFC < 0, "Down",
                                 dplyr::if_else(P.Value < 0.05 & logFC >= 0, "Up", "Non-sig.")))

  if (!"ID" %in% names(dat)) { dat <- dat |> tibble::rownames_to_column("ID") }


  suppressWarnings(
    p <-  dat |>
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
  )

  if (ref_line) {

    p <- p  + ggplot2::geom_hline(ggplot2::aes(yintercept = adj_to_p(fit, contrast)),
                                  linetype = "dashed", size = 0.25)

  }

  suppressWarnings(plotly::ggplotly(p, tooltip = c("x", "y", "label"), width = 1000, height = 600))

}
