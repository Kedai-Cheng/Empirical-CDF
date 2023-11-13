multi.ecdf <- function(data){
  result <- list()
  ##################
  ### One-Sample ###
  ##################
  if (is.vector(data)){
    x <- data
    n <- length(x)
    x.sort <- sort(x)
    knot <- unique(x.sort)
    ecdf <- rep(NA , length(knot)) 
    
    for (j in 1:length(knot)){
      ecdf[j] <- mean(x <= knot[j] , na.rm = TRUE)
    }
    # Store result #
    result <- c(result , list("Sample 1" = data.frame(cbind(knot , ecdf))))
  } 
  #################
  ### K-samples ###
  #################
  else{
    n.samples <- dim(data)[2]
    
    for (i in 1:n.samples) {
      x <- data[,i]
      n <- length(x)
      x.sort <- sort(x)
      knot <- unique(x.sort)
      ecdf <- rep(NA , length(knot)) 
      
      for (j in 1:length(knot)){
        ecdf[j] <- mean(x <= knot[j] , na.rm = TRUE)
      }
      # Store result #
      result <- c(result , 
                  list(data.frame(cbind(knot , ecdf))))
      names(result)[i] <- paste(c("Sample ",i) , collapse = "")
    }
  }
    ###############
    return(result)
}

#####################
###### Example ######
#####################
set.seed(100)
n <- 100
data <- cbind(
  rnorm(n),
  rexp(n),
  rpois(n , lambda = 2)
)

multi.ecdf(data = data)
