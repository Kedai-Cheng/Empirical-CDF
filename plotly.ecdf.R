plotly.ecdf <- function(data,
                        x.lab = "Values",
                        y.lab = "CDF",
                        x.lab.size = 12,
                        y.lab.size = 12,
                        x.tick.size = 15,
                        y.tick.size = 15,
                        title = "Empirical CDF of Data",
                        title.size = 15,
                        title.position.x = 0.5,
                        title.position.y = 0.975,
                        legend.size = 12,
                        pt.cex = 6,
                        pt.color = "#1f77b4",
                        lwd = 1,
                        line.color = "#1f77b4",
                        line.type = c("solid","dash","dot","dashdot")){
  #########################
  n.samples <- dim(data)[2]
  if (length(pt.color) != n.samples){
    warning("length(pt.color) does not equal to number of samples",call.=TRUE)
    pt.color <- rep(pt.color[1] , n.samples)
  }
  
  if (length(line.color) != n.samples){
    warning("length(line.color) does not equal to number of samples",call.=TRUE)
    line.color <- rep(line.color[1] , n.samples)
  }
  
  min.grand <- min(data)
  max.grand <- max(data)
  plot.x.min <- min.grand - (max.grand-min.grand)/10
  plot.x.max <- max.grand + (max.grand-min.grand)/10
  
  result <- list()
  #########################
  plot <- plot_ly()%>%
    layout(title = list(text = title,
                        x = title.position.x,
                        y = title.position.y,
                        font = list(size = title.size)),
           xaxis = list(title = x.lab,
                        tickfont = list(x.tick.size),
                        titlefont = list(x.lab.size),
                        zeroline = FALSE),
           yaxis = list(title = y.lab,
                        tickfont = list(y.tick.size),
                        titlefont = list(y.lab.size),
                        zeroline = FALSE))
  ### Start for loop for each sample ###
  for (i in 1:n.samples) {
    x <- data[,i]
    n <- length(x)
    x.sort <- sort(x)
    knot <- unique(x.sort)
    ecdf <- rep(NA , length(knot)) 
    
    for (j in 1:length(knot)){
      ecdf[j] <- sum(x <= knot[j])/n
    }
    # Store result #
    result <- c(result , list(data.frame(cbind(knot , ecdf))))
    # plot #
    plot.x <- c(plot.x.min,
                rep(knot , each = 3),
                plot.x.max)
    plot.y <- c(0 , 0)
    for (k in 1:length(knot)){
      plot.y <- c(plot.y , NA , rep(ecdf[k] , 2))
    }
    
    plot <- plot %>%
      add_trace(x = plot.x , y = plot.y , type = "scatter" , mode = "lines+markers",
                marker = list(size = c(rep(lwd , 3),
                                       rep(c(pt.cex , lwd , lwd) , n)),
                              color = pt.color[i]),
                line = list(width = lwd , color = line.color[i] , dash = line.type),
                name = paste("Sample" , i))
  }
  ############################
  print(plot)
  return(result)
}

################################################################################
#####################
###### Example ######
#####################
n <- 10
data <- cbind(rnorm(n),
              rexp(n),
              rpois(n , lambda = 2))
plotly.ecdf(data = data , line.type = "dot",
            pt.color = c("red" , "blue" , "black" , "orange"),
            line.color = c("red" , "blue" , "black" , "orange"),
            pt.cex=20,
            lwd = 10)
