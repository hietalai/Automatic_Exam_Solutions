### Solver for probability problems

## Binomial distribution
binomial <- function(n, pi, x, side = "le"){
  # Presents distribution
  cat(paste("$$ X \\sim bin(n = ", n, "; \\pi = ", pi,") $$"))
  
  # Calulates the probability
  if(side == "="){
    prob <- dbinom(x = x, size = n, prob = pi)
  } else if(side == "le"){
    prob <- pbinom(q = x, size = n, prob = pi)
  } else {
    prob <- pbinom(q = x-1, size = n, prob = pi, lower.tail = FALSE)
  }
  
  # Round probability to 3 decimals
  prob <- round(prob, 3)
  
  # If the side is created with a LaTeX command (i.e. no symbol is sent)
  if(nchar(side) > 1){
    cat(paste("$$ Pr(X ", paste("\\", side, sep = ""), x, ") = ", prob, " $$"))  
  } else {
    cat(paste("$$ Pr(X ", side, x, ") = ", prob, " $$"))  
  }
}


## Poisson distribution
poisson <- function(n, pi, x, approx = TRUE, side = "le"){
  # If it is an approximation of the binomial distribution include this in solution
  if(approx){
    cat(paste("$$ X \\sim bin(n = ", n, "; \\pi = ", pi,") $$"))
    
    cat(paste("$$ X \\approx po(\\mu = n*\\pi =", n*pi, ") $$"))
  }
  
  # Calulates the probability
  if(side == "="){
    prob <- dpois(x = x, lambda = n*pi)
  } else if(side == "le"){
    prob <- ppois(q = x, lambda = n*pi)
  } else {
    prob <- ppois(q = x-1, lambda = n*pi, lower.tail = FALSE)
  }
  
  # Round probability to 3 decimals
  prob <- round(prob, 3)
  
  # If the side is created with a LaTeX command (i.e. no symbol is sent)
  if(nchar(side) > 1){
    cat(paste("$$ Pr(X ", paste("\\", side, sep = ""), x, ") = ", prob, " $$"))  
  } else {
    cat(paste("$$ Pr(X ", side, x, ") = ", prob, " $$"))  
  }
}

## Binom to normal approx
bin.to.norm <- function(n, pi, x, side = "le"){
  cat(paste("$$  X \\sim bin(n = ", n, "; \\pi = ", pi, ") $$"))
  
  if(n*pi*(1-pi) > 5){
    cat(paste("$$ n*\\pi*(1-\\pi)= ", n * pi * (1-pi), "> 5 $$"))  
    
    mean <- n*pi
    sd <- sqrt(n*pi*(1-pi))
    
    cat(paste("$$ X \\approx N(\\mu = n*\\pi = ", round(mean, 3), 
              "; \\sigma = \\sqrt{n*\\pi*(1-\\pi)} = ", round(sd, 3), ")$$")) 
    
    if(side == "le"){
      x <- x + 0.5
      prob <- pnorm(q = x, mean = mean, sd = sd)
    } else if (side == "<") {
      x <- x - 0.5
      prob <- pnorm(q = x, mean = mean, sd = sd)
    } else if (side == ">"){
      x <- x + 0.5
      prob <- pnorm(q = x, mean = mean, sd = sd, lower.tail = FALSE)
    } else {
      x <- x - 0.5
      prob <- pnorm(q = x, mean = mean, sd = sd, lower.tail = FALSE)
    }
    
    if (nchar(side) == 2){
      cat(paste("$$ Pr(X", paste("\\", side, sep = ""), x, ") $$"))
      
      cat(paste("$$ Pr(Z", paste("\\", side, sep = ""), "\\frac{", x, "-", round(mean, 3),"}{", round(sd, 3),"}) $$"))
      cat(paste("$$ Pr(Z", paste("\\", side, sep = ""), round((x-mean)/sd, 3), ")$$"))
      cat(paste("$$ Pr(Z", paste("\\", side, sep = ""), round((x-mean)/sd, 3), ") = ", round(prob, 3), "$$"))
    } else {
      cat(paste("$$ Pr(X", side, x, ") $$"))
      
      cat(paste("$$ Pr(Z", side, "\\frac{", x, "-", round(mean, 3),"}{", round(sd, 3),"}) $$"))
      cat(paste("$$ Pr(Z", side, round((x-mean)/sd, 3), ")$$"))
      cat(paste("$$ Pr(Z", side, round((x-mean)/sd, 3), ") = ", round(prob, 3), "$$"))
    }
    
    
  } else {
    
    cat("No approximation can be made as normality is not assumed")
  }
}



## Normal distribution
normal <- function(mean, sigma, x, side = "le", calc = FALSE){
  # Presents distribution
  if(calc){ # If the distribution is presented earlier and this is only used for calculations
    
  } else {
    cat(paste("$$ X \\sim N(\\mu = ", mean, "; \\sigma = ", sigma,") $$"))
  }
  
  # Calulates the probability
  if(side == "="){
    prob <- dnorm(x = x, mean = mean, sd = sigma)
  } else if(side == "le"){
    prob <- pnorm(q = x, mean = mean, sd = sigma)
  } else {
    prob <- pnorm(q = x, mean = mean, sd = sigma, lower.tail = FALSE)
  }
  
  if(calc){
    return(prob)
  } else {
    # Round probability to 3 decimals
    prob <- round(prob, 3)
    
    # If the side is created with a LaTeX command (i.e. no symbol is sent)
    if(nchar(side) > 1){
      cat(paste("$$ Pr(X ", paste("\\", side, sep = ""), x, ") = ", prob, " $$"))  
    } else {
      cat(paste("$$ Pr(X ", side, x, ") = ", prob, " $$"))  
    }
  }
}

## Hypergeometric distribution

## Geometric distribution

## Central limit theorem
sample.mean.distr <- function(mean.x, sigma.x, n, x){
  
  cat(paste("$$ \\mu_X = ", mean.x, "$$"))
  cat(paste("$$ \\sigma_X = ", sigma.x, "$$"))
  
  sigma <- sigma.x/sqrt(n)
  
  cat(paste("$$ \\bar{X} \\sim N \\left(\\mu = ", mean.x, 
            "; \\sigma = \\frac{", sigma.x, "}{\\sqrt{", n, "}} = ", round(sigma, 3), " \\right) $$"))
  
  if(length(x) > 1){
    cat(paste("$$ Pr(", x[1], "\\le \\bar{X} \\le", x[2], ")$$"))
    cat(paste("$$ Pr(\\bar{X} \\le", x[2], ") - Pr(\\bar{X} \\le", x[1], ")$$"))
    
    up <- normal(mean = mean.x, sigma = sigma, x = x[2], calc = TRUE)
    down <- normal(mean = mean.x, sigma = sigma, x = x[1], calc = TRUE)
  
    cat(paste("$$ Pr(Z \\le \\frac{", x[2],"-", round(mean.x, 3), "}{", round(sigma, 3), "} = ", round((x[2]-mean.x)/sigma, 3), 
              ") - Pr(Z \\le \\frac{", x[1],"-", round(mean.x, 3), "}{", round(sigma, 3), "} = ", round((x[1]-mean.x)/sigma, 3), ")$$"))
    cat(paste("$$ ", round(up, 3), "-", round(down, 3), "$$"))
    cat(paste("$$ ", round(up-down, 3), "$$"))
  } else {
    # CALCULATE PROBABILITY OF ONE SIDE
  }
}

## Linear transformation normal probability
linear.transformation <- function(mean.x, sigma.x, a, b, x, side = "le"){
  cat(paste("$$ \\mu_X = ", mean.x, "$$"))
  cat(paste("$$ \\sigma_X = ", sigma.x, "$$"))
  
  cat(paste("$$ Y = ", a, "+ ", b, "X$$"))
  
  mean <- a + b*mean.x
  sigma <- b*sigma.x
  cat(paste("$$ Y \\sim N(\\mu = ", a, "+ ", b, "*\\mu_X =", round(mean, 3), 
            "; \\sigma = ", b, "*\\sigma_X = ", round(sigma, 3), ") $$"))
  
  cat(paste("$$ Pr(Y ", paste("\\", side, sep =""), x, ")$$"))
  
  cat(paste("$$ Pr(Z ", paste("\\", side, sep =""), 
            "\\frac{", x,"-", round(mean, 3), "}{", round(sigma, 3), "} = ", round((x-mean)/sigma, 3), ")$$"))
  if(side == "le"){
    prob <- normal(mean = mean, sigma = sigma, x = x, calc = TRUE)  
  } else {
    prob <- 1 - normal(mean = mean, sigma = sigma, x = x, calc = TRUE)  
  }
  
  cat(paste("$$", round(prob, 3), "$$"))
  
}

## Distribution of a sum
sum.x <- function(mean.x, sigma.x, constant = 0, n, x, side = "le"){
  cat(paste("$$ \\mu_X = ", mean.x, "$$"))
  cat(paste("$$ \\sigma_X = ", sigma.x, "$$"))
  
  if(constant > 0){
    cat(paste("$$ Y = ", constant, "+ \\sum_{i = 1}^{", n, "} X_i $$"))  
  } else {
    cat(paste("$$ Y = \\sum_{i = 1}^{", n, "} X_i $$"))  
  }
  
  
  mean <- constant + n*mean.x
  sigma <- sqrt(n)*sigma.x
  cat(paste("$$ Y \\sim N(\\mu = ", constant, "+ ", n, "*\\mu_X =", round(mean, 3), 
            "; \\sigma = \\sqrt{", n, "}*\\sigma_X = ", round(sigma, 3), ") $$"))
  
  cat(paste("$$ Pr(Y ", paste("\\", side, sep =""), x, ")$$"))
  
  cat(paste("$$ Pr(Z ", paste("\\", side, sep =""), 
            "\\frac{", x,"-", round(mean, 3), "}{", round(sigma, 3), "} = ", round((x-mean)/sigma, 3), ")$$"))
  if(side == "le"){
    prob <- normal(mean = mean, sigma = sigma, x = x, calc = TRUE)  
  } else {
    prob <- 1 - normal(mean = mean, sigma = sigma, x = x, calc = TRUE)  
  }
  
  cat(paste("$$", round(prob, 3), "$$"))
  
}



