# Define a test for the gPairsLower function
test_that("gPairsLower correctly updates GGally object", {

#Create GGally ggpairs object
  p <- ggpairs(data = iris)

#use gPairsLower to remove the top part
  updated_g <- gPairsLower(g=ggpairs(data = iris, diag="blank",
                                     upper="blank"))
#should have trimmed off 9 plots from the top half
  trimmed <- length(p[["plots"]]) - length(updated_g[["plots"]])

  expect_equal(trimmed, 9)

})
