plotly.ecdf <- function(data,
                        x.lab = "Values",
                        y.lab = "Empirical CDF",
                        x.lab.size = 12,
                        y.lab.size = 12,
                        x.tick.size = 15,
                        y.tick.size = 15,
                        title = "Empirical CDF of Data",
                        title.size = 15,
                        title.position.x = 0.5,
                        title.position.y = 0.975,
                        legend.size = 12,
                        cex = 6,
                        lwd = 1,
                        color = "#1f77b4",
                        line.type = c("solid","dash","dot","dashdot"),
                        plot.bg.color = "#f6f6f6",
                        paper.bg.color = "#f6f6f6",
                        legend.names = NULL){
  library(plotly)
  min.grand <- min(data , na.rm = TRUE)
  max.grand <- max(data , na.rm = TRUE)
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
                        tickfont = list(size = x.tick.size),
                        titlefont = list(size = x.lab.size),
                        zeroline = FALSE),
           yaxis = list(title = y.lab,
                        tickfont = list(size = y.tick.size),
                        titlefont = list(size = y.lab.size),
                        zeroline = FALSE),
           plot_bgcolor  = plot.bg.color,
           paper_bgcolor = paper.bg.color,
           legend = list(font = list(size = legend.size)))
  ##################
  ### One-Sample ###
  ##################
  if (is.vector(data)){
    
    if(is.null(legend.names)){
      legend.names <- paste("Sample" , 1)
    } else {
      legend.names <- legend.names[1]
    }
    
    x <- data
    n <- length(x)
    x.sort <- sort(x)
    knot <- unique(x.sort)
    ecdf <- rep(NA , length(knot)) 
    
    for (j in 1:length(knot)){
      ecdf[j] <- mean(x <= knot[j] , na.rm = TRUE)
    }
    # Store result #
    result <- c(result , list(legend.names = data.frame(cbind(knot , ecdf))))
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
                                       rep(c(cex , lwd , lwd) , n)),
                              color = color[1],
                              line=list(width=0)),
                line = list(width = lwd , color = color[1] , dash = line.type),
                name = legend.names)
  }
  #################
  ### K-samples ###
  #################
  else{
    n.samples <- dim(data)[2]
    
    if(is.null(legend.names)){
      legend.names <- paste("Sample" , 1:n.samples)
    } else {
      if (length(legend.names) != n.samples){
        warning("length(legend.names) does not equal to number of samples",call.=TRUE)
        legend.names <- rep(legend.names[1] , n.samples)
      }
    }
    
    if (length(color) != n.samples){
      warning("length(color) does not equal to number of samples",call.=TRUE)
      color <- rep(color[1] , n.samples)
    }
    
    ### Start for loop for each sample ###
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
      names(result)[i] <- legend.names[i]
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
                                         rep(c(cex , lwd , lwd) , n)),
                                color = color[i],
                                line=list(width=0)),
                  line = list(width = lwd , color = color[i] , dash = line.type),
                  name = legend.names[i])
    }
  }
  ############################
  print(plot)
  # return(result)
}

################################################################################
#####################
###### Example ######
#####################
# One-Sample #
set.seed(100)
n <- 100
x <- rnorm(n)
plotly.ecdf(data = x , color = "#003DA5" , 
            line.type = "dash")

# K-samples #
set.seed(100)
n <- 100
data <- cbind(rnorm(n),
              rexp(n),
              rpois(n , lambda = 2))
plotly.ecdf(data = data , cex = 12 , lwd = 4,
            line.type = "dot",
            color = c("#EBBC4E","#3F6C7D","#A6192E"))

