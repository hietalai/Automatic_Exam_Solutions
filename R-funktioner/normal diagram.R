
### Draws normal-distribution
normal <- function(test, crit, side = "neq"){
  require(ggplot2)
  
  Z <- seq(-5, 5, by = 0.01)
  
  Y <- dnorm(Z, mean = 0, sd = 1)
  
  data <- data.frame(Z, Y)
  
  plot <- ggplot(data = data, aes(x = Z, y = Y)) + geom_line(size = 2) + theme_bw() + 
    theme(axis.title.y = element_text(angle = 0)) + 
    scale_x_continuous(limits = c(-4, 4))
  
  if(side == "neq"){
    plot <- plot + geom_area(data = data[Z < -crit,], fill = "red") +
      geom_area(data = data[Z > crit,], fill = "red")
  } else if (side == ">"){
    plot <- plot + geom_area(data = data[Z > crit,], fill = "red")
  } else {
    plot <- plot + geom_area(data = data[Z < crit,], fill = "red")
  }
  
  plot <- plot + geom_segment(x = test, xend = test, y = 0, yend = dnorm(test), 
                 col = "black", size = 1) +
    annotate("text", x = test, y = -0.005, 
             label = "z[test]", parse = T) +
    theme(panel.grid = element_blank())
  
  return(plot)
  
}

### Draws t-distribution
t <- function(test, crit, df, side = "neq"){
  require(ggplot2)
  Z <- seq(-5, 5, by = 0.01)
  
  Y <- dt(Z, df = df)
  
  data <- data.frame(Z, Y)
  
  plot <- ggplot(data = data, aes(x = Z, y = Y)) + geom_line() + theme_bw() + 
    theme(axis.title.y = element_text(angle = 0)) + 
    scale_x_continuous(limits = c(-5, 5))
  
  if(side == "neq"){
    plot <- plot + geom_area(data = data[Z < -crit,], fill = "red") +
      geom_area(data = data[Z > crit,], fill = "red")
  } else if (side == ">"){
    plot <- plot + geom_area(data = data[Z > crit,], fill = "red")
  } else {
    plot <- plot + geom_area(data = data[Z < crit,], fill = "red")
  }
  
  plot <- plot + geom_segment(x = test, xend = test, y = 0, yend = dt(test, df), 
                              col = "black", size = 1) +
    annotate("text", x = test, y = -0.005, 
             label = "t[test]", parse = T)
  
  return(plot)
  
}

### Draws Chi^2-distribution
chi <- function(test, crit, df){
  require(ggplot2)
  Z <- seq(0, 15, by = 0.01)
  
  Y <- dchisq(Z, df = df)
  
  data <- data.frame(Z, Y)
  
  plot <- ggplot(data = data, aes(x = Z, y = Y)) + geom_line() + theme_bw() + 
    theme(axis.title.y = element_text(angle = 0)) + 
    scale_x_continuous(limits = c(0, 15)) +
    scale_y_continuous(breaks = NULL) + 
    geom_area(data = data[Z > crit,], fill = "red") +
    geom_segment(x = test, xend = test, y = 0, yend = dchisq(test, df), 
                 col = "black", size = 1) + 
    annotate("text", x = test, y = -0.005, 
             label = "{chi^2}[test]", parse = T)
  
  return(plot)
}