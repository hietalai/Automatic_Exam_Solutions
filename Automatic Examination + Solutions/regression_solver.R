### Regression Solver


## Simple linear regression ##
## Estimating the regression coefficients
coefficient_estimator <- function(x, y){
  if(length(x) != length(y)){
    stop("x and y are of different lengths")
  }
  
  cat("Skattning av $b_1$:")
  
  sumx <- round(sum(x), 3)
  sumy <- round(sum(y), 3)
  sumx2 <- round(sum(x^2), 3)
  sumxy <- round(sum(x*y), 3)
  
  cat(paste("$$ \\sum x = ", sumx, " $$"))
  cat(paste("$$ \\sum x^2 = ", sumx2, " $$"))
  cat(paste("$$ \\sum x*y = ", sumxy, " $$"))
  cat(paste("$$ \\sum y = ", sumy, " $$"))
  
  cat(paste("$$ b_1 = \\frac{", sumxy, "- \\frac{", sumx, "*", sumy, "}{", length(x), "}}{", 
            sumx2, "- \\frac{", sumx, "^2", "}{", length(x), "} }$$"))
  
  SSxy <- sum(x*y) - (sum(x)*sum(y))/length(x)
  SSxx <- sum(x^2) - (sum(x)^2)/length(x)
  
  b1 <- SSxy/SSxx
  
  cat(paste("$$ b_1 = ", round(b1, 3), "$$"))
  
  cat("Skattning av $b_0$:")
  
  cat(paste("$$ b_0 = \\frac{", sumy, "}{", length(x), "} - ", round(b1, 3), "* \\frac{", sumx, "}{", length(x), "} $$"))
  
  b0 <- mean(y) - b1 * mean(x)
  
  cat(paste("$$ b_0 = ", round(b0, 3), "$$"))
  
}

## Point estimation
point_estimate <- function(x, y, xnew){
  
  model <- lm(formula = y ~ x)
  
  coef <- model$coefficients
  
  ynew <- coef %*% c(1, xnew)
  
  cat(paste("$$ \\hat y_{x^* = ", xnew, "} = ", round(coef[1], 3), "+ ", round(coef[2], 3), "*", xnew, "$$"))
  
  cat(paste("$$ \\hat y_{x^* = ", xnew, "} = ", round(ynew, 3), "$$"))
  
}



