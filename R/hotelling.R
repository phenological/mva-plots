






#' @param x scores in the x direction
#' @param y scores in the y direction
#' @param alpha confidence level, default is 0.95 (95%)
#' @param len the amount of points to be generated to make the ellipses, default is 200

hotellings <- function(x, y, alfa=0.95, len=200) {

  n <- length(x)
  A <- 2

  #generate numbers from 0 to 2pi(2*pi*radius being the perimeter of the circle)
  mypi <- seq(0, 2 * pi, length=len)

  #radius for x direction using the variance, f-statistic and df
  r1 <- sqrt(var(x) * qf(alfa, 2, n - 2) * (2*(n^2 - 1)/(n * (n - 2))))

  #radius for y direction
  r2 <- sqrt(var(y) * qf(alfa, 2, n - 2) * (2*(n^2 - 1)/(n * (n - 2))))

  #generate coordinates for ellipses
  cbind(r1 * cos(mypi) + mean(x), r2 * sin(mypi) + mean(y))
}
scores<- a$pcdf[,1:8]
confidence_ellipse95 <- hotellings(scores[,1], scores[,2], alfa = 0.95, len=200)
confidence_ellipse99 <- hotellings(scores[,1], scores[,2], alfa = 0.99, len=200)

plot(scores[,1], scores[,2], xlim = c(min(confidence_ellipse99[,1]), max(confidence_ellipse99[,1])),
     ylim = c(min(confidence_ellipse99[,2]), max(confidence_ellipse99[,2])), ylab = 'Scores PC2', xlab = 'Scores PC1')
abline(h = 0, v = 0)
points(confidence_ellipse95, type = 'l', lty = 2, col = 'darkgreen')
points(confidence_ellipse99, type = 'l', lty = 2, col = 'red')

#' @title Hotelling's T2 ellipse in 2D
#' @description This function is used to calculate hotellings T2 ellipse
#' @param x num vector descrbing dimension 1
#' @param y, num vector descrbing dimension 2
#' @param alpha num, probability level
#' @references Geladi, P and Kowalski, B.R. (1986), Partial least squares and regression: a tutorial. \emph{Analytica Chimica Acta}, 185, 1-17.
#' @return data.frame containing H.T2 ellipse cooredinates
#' @author \email{torben.kimhofer@@murdoch.edu.au}
#' @importFrom ellipse ellipse
#' @importFrom stats cov
#' @family NMR ++
#' @keywords internal
.hotellingsT2 <- function(x, y, alpha = 0.95) {
  SD <- cov(cbind(x, y), use = "complete.obs")
  el <- ellipse(SD, centre = colMeans(cbind(x, y), na.rm = TRUE), level = alpha)
  colnames(el) <- c("V1", "V2")
  xlim <- c(min((c(el[, 1], x))), max((c(el[, 1], x))))
  xlim <- xlim + c(diff(range(xlim)) * -0.05, diff(range(xlim)) * +0.05)
  ylim <- c(min((c(el[, 2], y))), max((c(el[, 2], y))))
  ylim <- ylim + c(diff(range(ylim)) * -0.05, diff(range(ylim)) * +0.05)
  df <- as.data.frame(el)

  return(df)
}

# Calculate Hotellings T2 elipse
df=.hotellingsT2(x=sc[,1], y=sc[,2])

#kap<-kappa(obj@X)

g <- ggplot() +
  geom_polygon(data = df, aes_string(x = "V1", y = "V2"), fill = NA, alpha = 0.4, colour='black', linetype=3) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70")+
  theme_bw()+
  labs(caption = expression('Dashed line: Hotelling\'s T'^2~'ellipse ('*alpha*'=0.95)'))

#generate coordinates for ellipse
library(HotellingEllipse)
ellipseCoord(data = pcdf, pcx = , pcy = , conf.limit = 95)

ellipseCoord <- function(data, pcx = 1, pcy = 2, conf.limit = 0.95, pts = 200) {

  # matrix of data
  X <- as.matrix(data)

  # Sample size
  n <- nrow(X)

  # Confidence limit
  alpha <- as.numeric(conf.limit)

  # Number of points
  m <- as.numeric(pts)
  p <- seq(0, 2*pi, length.out = m)

  # # Hotelling’s T-square limit
  Tsq_limit <- (2*(n-1)/(n-2))*stats::qf(p = alpha, df1 = 2, df2 = (n-2))

  # Coordinate points
  rx <- sqrt(Tsq_limit*stats::var(X[, pcx]))
  ry <- sqrt(Tsq_limit*stats::var(X[, pcy]))

  #coordinates
  res.coord <- tibble::tibble(
    x = rx*cos(p) + mean(X[, pcx], na.rm = TRUE),
    y = ry*sin(p) + mean(X[, pcy], na.rm = TRUE)
  )

  return(res.coord)



 #for PC's up to threshold, calc coordinates for hotelling's
  data= a$pcdf[,1:8]
  thresh = 3
  conf.limit = 0.95
  pts = 200

  X <- as.matrix(data)
  n <- nrow(X)
  alpha <- as.numeric(conf.limit)
  m <- as.numeric(pts)
  p <- seq(0, 2*pi, length.out = m)

  Tsq_limit <- (2*(n-1)/(n-2))*stats::qf(p = alpha, df1 = 2, df2 = (n-2))

  radiusList <- list()
  for (i in 1:thresh){
    radiusList[[i]] <- sqrt(Tsq_limit*stats::var(X[, i]))
    }

  radiusList<- unlist(radiusList)

  xCoordList <- list()
  for (i in 1:thresh){
    xCoordList[[i]] <- (radiusList[i])*cos(p) + mean(X[, i], na.rm = TRUE)
  }

  yCoordList <- list()
  for (i in 1:thresh){
    yCoordList[[i]] <- (radiusList[i])*sin(p) + mean(X[, i], na.rm = TRUE)
  }


  pca_scores %>%
    ggplot(aes(x = Dim.1, y = Dim.3)) +
    geom_point(aes(fill = Tsq), shape = 21, size = 3, color = "black") +
    scale_fill_viridis_c(option = "viridis") +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = a1, b = b1, angle = 0), size = .5, linetype = "dotted") +
    geom_ellipse(aes(x0 = 0, y0 = 0, a = a2, b = b2, angle = 0), size = .5, linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "solid", color = "black", size = .2) +
    geom_vline(xintercept = 0, linetype = "solid", color = "black", size = .2) +
    labs(
      title = "Scatterplot of PCA scores",
      subtitle = "PC1 vs. PC3",
      x = "PC1",
      y = "PC3",
      fill = "T2 stats",
      caption = "Figure 1"
    ) +
    theme_bw()


  myFn3 <- function(data, mapping, method="geom_ellipse"){
    p <- ggplot(data = data, mapping = mapping) +
      theme_minimal() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
      scale_color_brewer(palette = "Set2")
    return(p)
  }

