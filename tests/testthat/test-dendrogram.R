test_that("list works (PCA grid) ", {
  pca <- PCA(data = new_lipidData)
  ps <- plotScores(model = pca,
                   optns = list(color = new_lipidMetadata$Timepoint,
                                ellipse = "color"))

  #1st 2 PCs
  expect_no_error(dendrogram(ps = ps, p = 2))

  #1st 3 PCs
  expect_no_error(dendrogram(ps = ps, p = 3))

})

test_that("oplsda works", {
  model<- oplsda(X = new_lipidData,
                 Y = new_lipidMetadata$sample_batch,
                 type = "OPLS")
  ps <- plotScores(model = model,
                   optns = list(color = new_lipidMetadata$sample_batch,
                                ellipse = "hotellings"))

  expect_no_error(object = dendrogram(ps = ps, p = 2))

})

test_that("ggplot works", {
  pca <-  PCA(data = new_lipidData)

  ps <- plotScores(model = pca,
                   optns = list(PCi = 1,
                                PCj = 2,
                                color = new_lipidMetadata$Timepoint,
                                ellipse = "color"))

  expect_no_error(object = dendrogram(ps = ps, p = 2))

})

test_that("more than 3 groups works", {
  # ####USArrests####
  # # Replicate each row three times
  replicated_data <- USArrests[rep(row.names(USArrests), each = 3), ]

  # Apply jitter to each numeric column
  jittered_data <- replicated_data
  numeric_cols <- sapply(jittered_data, is.numeric)
  jittered_data[numeric_cols] <- lapply(jittered_data[numeric_cols], jitter)

  # Remove the ".1" and ".2" suffixes from row names
  jittered_data$state<- rownames(jittered_data)
  jittered_data$state <- gsub("\\.\\d+$", "", (jittered_data$state))

  ###PCA###
  pca<- PCA(data = jittered_data[,1:4])
  ps<- plotScores(model = pca,
                  optns = list(color = jittered_data$state,
                               discretePalette = c("#36ff20", "#3df721", "#44ef22", "#4be723", "#52e023", "#59d824", "#60d025", "#67c826", "#6dc027", "#74b828", "#7bb029", "#82a929", "#89a12a", "#90992b", "#97912c", "#9e892d", "#a5812e", "#ac792f", "#b37130", "#ba6a30", "#c16231", "#c85a32", "#ce5233", "#d54a34", "#dc4235", "#e33a36", "#ea3336", "#f12b37", "#f82338", "#ff1b39",
                                                   "#224eff", "#2157ff", "#2061fe", "#206afe", "#1f73fd", "#1e7dfd", "#1d86fc", "#1c8ffc", "#1c99fc", "#1ba2fb", "#1aabfb", "#19b4fa", "#19befa", "#18c7fa", "#17d0f9", "#16daf9", "#15e3f8", "#15ecf8", "#14f6f7", "#13fff7")))

  expect_no_error(dendrogram(ps = ps, p = 2, breaks = 20))
})
