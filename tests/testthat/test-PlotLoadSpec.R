# PlotLoadSpec

test_that("PCA loading using prcomp", {
  # build a pca model using prcomp
  load("../mva-plots/data/NMR_1D_rat_urine_spectra.rda")

  PC = 2
  # build the PCA model using prcomp
  model<-prcomp(X, rank. = PC)

  # check the class of the pca model
  expect_true(class(model)[1]=="prcomp", "The class of the pca model should be prcomp")

  model_loadings_plot<-PlotLoadSpec(model = model,X = X, PC = PC)
  
  # check the labels match with the model which should be PCA and statistical reconstruction
  expect_true(model_loadings_plot$labels$title=="PCA" & model_loadings_plot$labels$subtitle=="Statistical reconstruction", 
              "prcomp is for PCA only therefore loading should be in Statistical reconstruction")
  
  # check the loading plot is correspondign the principal componet chosen
  expect_true(gsub("PC: ","",model_loadings_plot$labels$caption)==paste0(PC," loadings"), 
              "plotted Loadings shold be from the same PC chosen to plot ")  

})

test_that("PCA loadings using mva.plots PCA",{

  load("../mva-plots/data/NMR_1D_rat_urine_spectra.rda")
  
  PC = 2
  
  # build the PCA model using mva-plots PCA()
  model<-mva.plots::PCA(X)
  
  # check the class of the pca model
  expect_true(class(model)[1]=="list" & names(model)[1] == "data"& names(model)[2] == "plots", "The class of the pca model should be list of 2. data and plots ")
  
  model_loadings_plot<-PlotLoadSpec(model = model, X = X, PC = PC)
  
  # check the labels match with the model which should be PCA and statistical reconstruction
  expect_true(model_loadings_plot$labels$title=="PCA" & model_loadings_plot$labels$subtitle=="Statistical reconstruction", 
              "prcomp is for PCA only therefore loading should be in Statistical reconstruction")
  
  # check the loading plot is correspondign the principal componet chosen
  expect_true(gsub("PC: ","",model_loadings_plot$labels$caption)==paste0(PC," loadings"), 
              "plotted Loadings shold be from the same PC chosen to plot ") 

})


test_that("OPLS-DA loadings using mva.plots oplsda no median trace",{
  load("../mva-plots/data/NMR_1D_rat_urine_spectra.rda")
  
  model = oplsda(X,Y = Y,type = "OPLS")

  # check the class and type of the model
  expect_true(class(model)[1]=="opls" & model@typeC=="OPLS-DA", "The class of model should be opls and the type should be OPLS-DA")
  
  model_loadings_plot<-PlotLoadSpec(model = model,Median = FALSE)
  
  # check the loading plot data dimension
  expect_true(ncol(model_loadings_plot$data)==3, "Without Median trace, the data should have three columns")
  
})


test_that("OPLS-DA loadings using mva.plots oplsda with median trace",{
  load("../mva-plots/data/NMR_1D_rat_urine_spectra.rda")
  
  model = oplsda(X,Y = Y,type = "OPLS")
  
  # check the class and type of the model
  expect_true(class(model)[1]=="opls" & model@typeC=="OPLS-DA", "The class of model should be opls and the type should be OPLS-DA")
  
  model_loadings_plot<-PlotLoadSpec(model = model,Median = TRUE)
  
  # check the loading plot data dimension
  expect_true(ncol(model_loadings_plot$data)==5, "With Median trace, the data should have five columns")
  
})
