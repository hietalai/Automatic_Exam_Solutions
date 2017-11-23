## Linear transformation calculations

linear.transformation <- function(mean.x, sigma.x = NULL, a, b){
  cat(paste("$$ \\mu_X = ", mean.x, "$$"))
  
  cat(paste("$$ Y = ", a, "+ ", b, "X$$"))
  
  mean <- a + b*mean.x
  
  cat(paste("$$ \\mu_Y = ", a, "+ ", b, "*\\mu_X =", round(mean, 3), "$$"))
  
  if(!is.null(sigma.x)){
    cat(paste("$$ \\sigma_X = ", sigma.x, "$$"))  
    sigma <- b*sigma.x
    
    cat(paste("$$ \\sigma_Y = ", b, "*\\sigma_X =", round(sigma, 3), "$$"))
  }

}