
test_that("You can use a data frame", {

  #don't specify control, expect warning
  expect_warning(object = lipidGraph(model = as.data.frame(new_lipidData),
                                     stat = "fc",
                                     filter = "none",
                                     optns = list(factor = (new_lipidMetadata$Timepoint))),
                 regexp = "No control specified in optns for factor. The first entry was set as the control")

  #do specify control
  lg<- lipidGraph(model = as.data.frame(new_lipidData),
                  stat = "fc",
                  filter = "none",
                  optns = list(factor = (new_lipidMetadata$Timepoint),
                               control = "Control"))

  expect_equal(object = unique(lg[["data"]][["Group"]])[1], expected = "COVID")
  expect_equal(object = unique(lg[["data"]][["Group"]])[2], expected = "MISC")


  #specify which group want graphed
  lg<- lipidGraph(model = as.data.frame(new_lipidData),
                  stat = "fc",
                  filter = "none",
                  optns = list(factor = new_lipidMetadata$Timepoint,
                               control = "Control",
                               columns_to_plot = "MISC"))
  expect_equal(object = unique(lg[["data"]][["Group"]])[1], expected = "MISC")
  expect_equal(object = unique(lg[["data"]][["Group"]])[2], expected = as.character(NA))

  #if change control, do the correct groups show up? use cd instead of fc

  lg<- lipidGraph(model = as.data.frame(new_lipidData),
                  stat = "cd",
                  filter = "none",
                  optns = list(factor = (new_lipidMetadata$Timepoint),
                               control = "MISC"))
  expect_length(object = unique(lg[["data"]][["Group"]]), n = 2)
  expect_contains(object = lg[["data"]][["Group"]], expected = "Control")
  expect_contains(object = lg[["data"]][["Group"]], expected = "COVID")

})

test_that("NA in factor will throw error",{
  #change one new_lipidMetadata Class entry to NA
  new_lipidMetadata2 <- new_lipidMetadata
  new_lipidMetadata2[1,"Timepoint"] <- NA

  expect_error(object = lipidGraph(model = as.data.frame(new_lipidData),
                                   stat = "cd",
                                   filter = "none",
                                   optns = list(factor = (new_lipidMetadata2$Timepoint),
                                                control = "Control")),
               regexp = "One of your factors is NA, please change this before running lipidGraph")

})

test_that("external stat can be supplied, including with more than one group to be graphed",{

  #supply external stat

   ########adjusted p-value########
   model <- as.data.frame(new_lipidData)

   optns <- list(factor = as.vector(new_lipidMetadata$Timepoint), control = "Control", method = "fdr")

   id <- colnames(model)
   df <- model

     # #empty df
      pvalUnadjusted <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))
      pvalRescaled <- data.frame(matrix(NA, nrow = ncol(df), ncol = 1))

     df[,"factor"] <- as.numeric(relevel(as.factor(optns$factor), ref = optns$control))
     unique_factors <- unique(df[,"factor"])

     for(j in 2: length(unique_factors)){

       df2 <- df[df[,"factor"] %in% c(1, j), ]

       pval <- data.frame(matrix(NA, nrow = ncol(df)-1, ncol = 1))
       for(i in 1:(ncol(df2)-1)){
         pval[i,]<-kruskal.test(df2[,i], df2[,"factor"])$p.value
       }

       pvalUnadjusted[,j] <- pval

       #rescaled and adjusted p-value
       for(i in 1:(ncol(df2)-1)){
         pvalRescaled[i,j] <- abs(log10(p.adjust(pvalUnadjusted[i, j], method = optns$method)))
       }
     }

     # Create a mapping between numbers and words, rename pval dataframe columns
     mapping <- setNames(unique(optns$factor), unique(df[,"factor"]))

     testnames<- as.data.frame(mapping)
     testnames$rowName <- rownames(testnames)

     for (i in seq_len(nrow(testnames))) {
       col_number <- as.numeric(testnames[i, 2])
       new_col_name <- as.character(testnames[i, 1])
       names(pvalUnadjusted)[col_number] <- new_col_name
       names(pvalRescaled)[col_number] <- new_col_name
     }

   lg<- lipidGraph(model = as.data.frame(new_lipidData),
                   stat = "external",
                   filter = "none",
                   optns = list(factor = new_lipidMetadata$Timepoint,
                                control = "Control",
                                external = pvalRescaled))

   expect_length(object = unique(lg[["data"]][["Group"]]), n = 2)
   expect_contains(object = lg[["data"]][["Group"]], expected = "COVID")
   expect_contains(object = lg[["data"]][["Group"]], expected = "MISC")

  #warnign that can't use filter with external stat
   expect_warning(object = lipidGraph(model = as.data.frame(new_lipidData),
                                      stat = "external",
                                      filter = 1,
                                      optns = list(factor = new_lipidMetadata$Timepoint,
                                                   control = "Control",
                                                   external = pvalRescaled)),
                  regexp = "filter automatically set to none for externally supplied stat. Please filter prior to using lipidGraph." )


  #external stat and columns_to_plot work together
   pca<- PCA(data = new_lipidData, plot = FALSE, rank =3)
     lp <- lipidGraph(model = pca,
                      stat = "external",
                      optns = list(factor = new_lipidMetadata$Timepoint,
                                   control = "Control",
                                   columns_to_plot = "COVID",
                                   external = (pvalRescaled$COVID)
                      ))

   expect_length(object = unique(lp[["data"]][["Group"]]), n = 1)
})

test_that("the settings for color work", {

  #if choose ony one group can you set color to direction
  lg<- lipidGraph(model = as.data.frame(new_lipidData),
                  stat = "cd",
                  filter = "none",
                  optns = list(factor = new_lipidMetadata$Timepoint,
                               control = "Control",
                               columns_to_plot = "MISC",
                               color = "Direction"))
  g <- ggplot_build(plot = lg)

  gd <- data.frame(colours = unique(g$data[[1]]["colour"]),
                   label = g$plot$scales$scales[[1]]$get_labels())

  expect_contains(gd[,2], expected = c("negative", "positive"))

  #can't have more than one group and set direction as color
  lg<- lipidGraph(model = as.data.frame(new_lipidData),
                  stat = "cd",
                  filter = "none",
                  optns = list(factor = new_lipidMetadata$Timepoint,
                               filter = "none",
                               control = "Control",
                               color = "Direction"))


  g <- ggplot_build(plot = lg)

  gd <- data.frame(colours = unique(g$data[[1]]["colour"]),
                   label = g$plot$scales$scales[[1]]$get_labels())

expect_contains(gd[,2], expected = c("COVID", "MISC"))

#can override Guide title for more than one Group
lg<- lipidGraph(model = as.data.frame(new_lipidData),
                stat = "cd",
                filter = "none",
                optns = list(factor = new_lipidMetadata$Timepoint,
                             control = "Control",
                             color = "Direction",
                             guides = guides(color = guide_legend(title = "Polarity"),
                                             size = guide_legend(paste0("|cd|"))
                                             )
                             )
                )

expect_equal(object = lg[["guides"]][["colour"]][["title"]], expected = "Polarity")
})

#if no second side chain is listed
test_that("If no 2nd side chain, still handled", {

  #multiple groups
  idx <- grep("TAG", names(new_lipidData))
  TAG <- new_lipidData[,idx]

  lg <- lipidGraph(model = as.data.frame(TAG),
                   stat = "cd",
                   filter = "none",
                   optns = list(factor = new_lipidMetadata$Timepoint,
                                control = "Control"))

  g <- ggplot_build(plot = lg)

  gd <- data.frame(colours = unique(g$data[[1]]["colour"]),
                   label = g$plot$scales$scales[[1]]$get_labels())

  expect_contains(gd[,2], expected = c("COVID", "MISC"))

#single Group
  idy <- which(new_lipidMetadata$Timepoint == "MISC")
  Class <- new_lipidMetadata[-idy,"Timepoint"]
  TAG2 <- TAG[-idy,]

  lg <- lipidGraph(model = as.data.frame(TAG2),
                   stat = "cd",
                   filter = "none",
                   optns = list(factor = Class,
                                control = "Control",
                                color = "Direction"))

  g <- ggplot_build(plot = lg)

  gd <- data.frame(colours = unique(g$data[[1]]["colour"]),
                   label = g$plot$scales$scales[[1]]$get_labels())

  expect_contains(gd[,2], expected = c("negative", "positive"))

  expect_equal(object =  unique(lg[["data"]][["Group"]]), expected = "COVID")


})


   #use PCA model

  test_that("You can use PCA object", {

    pca<- PCA(data = new_lipidData, plot = FALSE, rank =3)

    lg<- lipidGraph(model = pca,
                    stat = "cd",
                    filter = "none",
                    optns = list(factor = (new_lipidMetadata$Timepoint),
                                 control = "Control"))


    #did the class, side chain and id turn out in the correct format? Use the first lipid to test
    expect_equal(object = lg[["data"]][["class"]][1], expected = "CE")
    expect_equal(object = lg[["data"]][["sc1"]][1], expected = "14:0")
    expect_equal(object = lg[["data"]][["id"]][1], expected = "CE(14:0)")
  })

   #use oplsda model
test_that("You can use oplsda object", {
  op<- oplsda(X = new_lipidData, Y = new_lipidMetadata$Timepoint, type = "PLS")
  lg<- lipidGraph(model = op,
                  stat = "cd",
                  filter = "none",
                  optns = list(factor = (new_lipidMetadata$Timepoint),
                               control = "Control"))

  #did the class, side chain and id turn out in the correct format? Use the first lipid to test
  expect_equal(object = lg[["data"]][["class"]][1], expected = "CE")
  expect_equal(object = lg[["data"]][["sc1"]][1], expected = "14:0")
  expect_equal(object = lg[["data"]][["id"]][1], expected = "CE(14:0)")

})


#provide specific range
test_that("You can provide a start and end lipid", {
  pca<- PCA(data = new_lipidData, plot = FALSE, rank =3)

  #error should appear
  expect_error(object = lipidGraph(model = pca,
                                     stat = "cd",
                                   filter = "none",
                                     optns = list(factor = (new_lipidMetadata$Timepoint),
                                                  control = "Control",
                                                  lipidStart = "CE(18:0)")), regexp =  "lipidEnd has not been supplied")
  lg <- lipidGraph(
                    model = pca,
                    stat = "cd",
                    filter = "none",
                    optns = list(
                      factor = (new_lipidMetadata$Timepoint),
                      control = "Control",
                      lipidStart = "CE(18:0)",
                      lipidEnd = "LPC(20:0)"
                    )
                  )

  expect_equal(object = lg[["data"]][["id"]][1], expected = "CE(18:0)")
  expect_equal(object = lg[["data"]][["id"]][818], expected = "LPC(20:0)")

})

test_that("can filter by top x most significant", {
  lp <- lipidGraph(model = as.data.frame(new_lipidData),
                   stat = "cd",
                   filter = 1:10,
                   optns = list(factor = new_lipidMetadata$Timepoint,
                                control = "Control",
                                columns_to_plot = "COVID",
                                color = "Direction"))

  expect_equal(object = nrow(lp[["data"]]), expected = 10)
})


test_that("can filter by threshold", {
  pca<- PCA(data = new_lipidData, plot = FALSE, rank =3)

  lp <- lipidGraph(model = pca,
                   stat = "pval",
                   filter = 1.3,
                   optns = list(factor = new_lipidMetadata$Timepoint,
                                control = "Control"))
  min_sig <- min(lp[["data"]][["Sig"]])
  expect_true(min_sig >= 1.3,
              sprintf("Minimum Sig value is %f, which is not 1.3 or higher", min_sig))
})





