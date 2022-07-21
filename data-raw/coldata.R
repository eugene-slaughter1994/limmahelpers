## code to prepare `coldata` dataset goes here

metab_dat_list <- list(
  "Batch1" = hRUV::b1,
  "Batch2" = hRUV::b2,
  "Batch3" = hRUV::b3,
  "Batch4" = hRUV::b4,
  "Batch5" = hRUV::b5
)


metab_dat_list <- lapply(metab_dat_list, function(dat) {
  SummarizedExperiment::assay(dat, "logRaw", withDimnames = FALSE) = log2(SummarizedExperiment::assay(dat, "raw") + 1)
  dat
})


metab_dat_list <- hRUV::clean(metab_dat_list, threshold = 0.5,
                              method = "intersect", assay = "logRaw", newAssay = "rawImpute")


metab_dat <- hRUV::hruv(
  dat_list = metab_dat_list,
  assay = "rawImpute",
  intra = "loessShort",
  inter = "concatenate",
  intra_k = 5, inter_k = 5,
  pCtlName = "biological_sample",
  negCtl = NULL,
  intra_rep = "short_replicate",
  inter_rep = "batch_replicate"
)

coldata <- SummarizedExperiment::colData(metab_dat)

coldata <- coldata[which(coldata$pool == FALSE), c("Sample", "sample_name")]

coldata$disease <- sample(c("Healthy", "Disease1", "Disease2"), size = nrow(coldata), replace = TRUE)

coldata <- coldata[, c("Sample", "disease")]

usethis::use_data(coldata, overwrite = TRUE)
