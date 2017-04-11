## @knitr Chunk-1000

#Define Constants
xBegin = -4
xEnd = 4
vNumDataPoints = 501
mean1 = -0.5
mean2 = 0.5
sd1 = 0.25
sd2 = 0.25

#Define Functions
ComputeYVals = function(vXVals = NULL, vMean = 0, vSD = 1) {
  if(is.null(vXVals)) stop("Error: parameter \'vXVals\' must not be NULL");
  yVals = dnorm(x = vXVals, mean = vMean, sd = vSD)
}

DrawPolygon = function(xValsForward = NULL, xValsReverse = NULL, yValsForward = NULL, yValsReverse = NULL, pColor = "red", pAlpha = 0.5, plty = 0) {
  if(is.null(xValsForward) | is.null(yValsForward)) {
    stop("Error: xValsFoward and yValsForward must not be NULL\n")
  }
  if(is.null(xValsReverse)) {
    xValsReverse = rev(xValsForward)
  }
  
  if(is.null(yValsReverse)) {
    yValsReverse = rep(0, length(yValsForward))
  }
  
  polygon( x = c(xValsForward, xValsReverse),
           y = c(yValsForward, yValsReverse),
           col = adjustcolor(col = pColor, alpha.f = pAlpha),
           lty = plty
  )
}

#Define Working Code
xvals = seq(from = xBegin, to = xEnd, length.out = vNumDataPoints)
y1_vals = ComputeYVals(vXVals = xvals, vMean = mean1, vSD = sd1)
y2_vals = ComputeYVals(vXVals = xvals, vMean = mean2, vSD = sd2)
set1 = cbind.data.frame(xvals, yvals = y1_vals)
set2 = cbind.data.frame(xvals, yvals = y2_vals)
plot( x = NULL,
      y = NULL,
      xlim = range(xBegin, xEnd),
      #ylim = range(y1_vals - y1_vals * 0.1, y2_vals + y2_vals * 0.1),
      ylim = extendrange(x = range(y1_vals, y2_vals), f = 0.10),
      main = "Conceptual Illustration of Two Separate Normal Distributions",
      xlab = "x",
      ylab = "Probability Density")
points(xvals, y1_vals, lty = "dashed", type = "l")
points(xvals, y2_vals, lty = "dashed", type = "l")

# Color the portion of the left distribution that doesn't overlap with the right distribution
DrawPolygon( xValsForward = set1$xvals[set1$xvals <= 0],
             xValsReverse = rev(set2$xvals[set2$xvals <= 0]),
             yValsForward = set1$yvals[set1$xvals <= 0],
             yValsReverse = rev(set2$yvals[set2$xvals <= 0]),
             pColor = "red")

# Color the portion of the right distribution that doesn't overlap with the left distribution
DrawPolygon( xValsForward = set2$xvals[set2$xvals >= 0],
             xValsReverse = rev(set1$xvals[set1$xvals >= 0]),
             yValsForward = set2$yvals[set2$xvals >= 0],
             yValsReverse = rev(set1$yvals[set1$xvals >= 0]),
             pColor = "yellow")

# Color the portion of the two distributions that overlap
DrawPolygon( xValsForward = c(set2$xvals[set2$xvals <= 0], set1$xvals[set1$xvals >= 0]),
             xValsReverse = NULL,
             yValsForward = c(set2$yvals[set2$xvals <= 0], set1$yvals[set1$xvals >= 0]),
             yValsReverse = NULL,
             pColor = "green")


abline(v = c(mean1, mean2), lty = "dashed")