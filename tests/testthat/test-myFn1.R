# # Define a test for the myFn1 function INCOMPLETE
# test_that("myFn1 returns a ggplot object with expected properties", {
#   # Create a sample data frame
#   data <- data.frame(
#     x = c(1, 2, 3, 4, 5),
#     y = c(5, 4, 3, 2, 1)
#   )
#   # Generate some sample data
#   set.seed(123)
#   data <- as.data.frame(matrix(rnorm(100), ncol = 5))
#   data[data < 0] <- abs(data[data < 0])
#
#   data$fact <- sample(c("control", "treatment"), 20, replace = TRUE)
#
#  p<- GGally::ggpairs(data = data[,1:5],
#                   diag="blank",
#                   upper="blank",
#                   lower=list(continuous = myFn1),
#                   progress = F,
#                   switch = "both") +
#     theme_bw() +
#    geom_point()
#
# p2 <-  GGally::ggpairs(data = data[,1:5],
#                  diag="blank",
#                  upper="blank",
#                  #lower=list(continuous = myFn1),
#                  progress = F,
#                  switch = "both") +
#    theme_bw() +
#   geom_point()
#
#   # Define the mapping
#   mapping <- aes(x = x, y = y)
#
#   # Call the myFn1 function
#   plot <- myFn1(data, mapping, method = "stat_ellipse")
#
#   # Check if the result is a ggplot object
#   expect_type(plot, "ggplot")
#
#   # Check if the plot has the expected properties
#   expect_true("theme_minimal" %in% class(plot$layers[[1]]$theme$theme))
#   expect_true("geom_hline" %in% class(plot$layers[[2]]$geom$geom))
#   expect_true("geom_vline" %in% class(plot$layers[[3]]$geom$geom))
#   expect_equal(plot$data, data)
#   expect_equal(plot$mapping, mapping)
# })

