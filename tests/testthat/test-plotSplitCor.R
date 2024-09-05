
test_that("test if the function can handle NA", {
  lipidData <- readRDS("~/git/phenological/mva-plots/inst/extdata/lipidData.rds")
  lipidMetadata <- readRDS("~/git/phenological/mva-plots/inst/extdata/lipidMetadata.rds")
  c1Name = names(table(lipidMetadata$sample_batch))[1]
  c2Name = names(table(lipidMetadata$sample_batch))[2]
  corList<-list()
  corList[[1]]<-cor(lipidData[which(lipidMetadata$sample_batch==c1Name),which(sapply(strsplit(colnames(lipidData),"(",fixed = T),"[",1)=="DCER")])
  corList[[2]]<-cor(lipidData[which(lipidMetadata$sample_batch==c2Name),which(sapply(strsplit(colnames(lipidData),"(",fixed = T),"[",1)=="DCER")])
  test<-corList[[1]]
  test[5,5]<-NA
  corList[[1]]<-test
  expect_no_error(plotSplitCor(corList = corList,type = "full"))

})
