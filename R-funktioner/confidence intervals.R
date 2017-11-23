prop.conf.int <- function(x, n, conf = 0.95, alternative = "two.sided"){
  if(length(x) != length(n)){
    stop("Length of x and n are not the same.")
  }
  if(length(x) > 2){
    stop("More than two groups cannot be calculated.")
  }
  if(length(x) == 1){
    p <- x/n
    
    if(alternative == "two.sided"){
      lower <- p - qnorm(1 - (1-conf)/2) * sqrt(p*(1-p)/n)
      upper <- p + qnorm(1 - (1-conf)/2) * sqrt(p*(1-p)/n)
    }
  return(list(estimate = p, conf.int = c(lower, upper)))
  } else {
    p <- x/n
            
    if(alternative == "two.sided"){
      lower <- p[1] - p[2] - qnorm(1 - (1-conf)/2) * 
        sqrt(p[1]*(1-p[1])/sum(n[1]) + p[2]*(1-p[2])/sum(n[2]))
      upper <- p[1] - p[2] + qnorm(1 - (1-conf)/2) * 
        sqrt(p[1]*(1-p[1])/sum(n[1]) + p[2]*(1-p[2])/sum(n[2]))
    }
  return(list(estimate = p, conf.int = c(lower, upper)))
  }
  
}

# testing
# prop.conf.int(56, 78)
