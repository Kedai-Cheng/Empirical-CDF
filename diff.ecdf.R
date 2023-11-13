diff.ecdf <- function(x , y) {
  x <- x
  y <- y
  combined <- sort(c(x,y))
  ecdf.x <- ecdf.y <- rep(NA , length(combined))
  for (i in 1:length(combined)){
    ecdf.x[i] <- mean(x <= combined[i])
    ecdf.y[i] <- mean(y <= combined[i])
  }
  
  diff <- abs(ecdf.x - ecdf.y)
  
  max.diff <- max(diff)
  location.max.diff <- combined[which(diff == max(diff))]
  text <- c(paste("The maximum distance between two sample cumulative distribution is",
                        max.diff,", around variable value of",location.max.diff,"."))
  
  result <- list("Max.Diff.Dist" = max.diff,
                 "Location.Max.Diff.Dist" = location.max.diff,
                 "Summary" = text)
  
  return(result)
}

#####################
###### Example ######
#####################
set.seed(100)
x <- rnorm(100)
y <- rexp(60)
diff.ecdf(x = x , y = y)
