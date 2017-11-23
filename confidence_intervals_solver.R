### Confidence intervals


## Proportion CI
prop.ci <- function(x, n, alpha = 0.05, side = "ne"){
  p <- x/n
  
  if(n*p*(1-p) < 5){
    cat(paste("$$ n * p * (1-p) =", round(n*p*(1-p), 1), "< 5 $$"))
    cat(paste("Statistikan anses inte vara normalfördelad och därmed uppfylls inte antaganden för beräkningarna."))
  } else {
    cat(paste("$$ p = ", round(p, 3), "$$"))
    
    if(side == "ne"){
      z <- qnorm(1 - alpha/2)
      
      cat(paste("$$ ", round(p, 3), "\\pm", round(z, 2), " * \\sqrt{ \\frac{", round(p, 3), "*", round(1-p, 3), "}{", n, "}} $$"))  
      
      fm <- z * sqrt(p*(1-p)/n)
      lower <- p - z * sqrt(p*(1-p)/n)
      upper <- p + z * sqrt(p*(1-p)/n)
      
      cat(paste("$$ ", round(p, 3), "\\pm", round(fm, 3), " $$"))  
      
      cat(paste("$$ ", round(lower, 3), " \\le \\pi \\le", round(upper, 3), "$$"))
    } else if(side == "<"){
      z <- qnorm(1-alpha)
      
      cat(paste("$$ ", round(p, 3), "+", round(z, 2), " * \\sqrt{ \\frac{", round(p, 3), "*", round(1-p, 3), "}{", n, "}} $$"))  
      
      limit <- p + z * sqrt(p*(1-p)/n)
      
      cat(paste("$$ \\pi", side, round(limit, 3), "$$"))
      
    } else {
      z <- qnorm(1-alpha)
      
      cat(paste("$$ ", round(p, 3), "-", round(z, 2), " * \\sqrt{ \\frac{", round(p, 3), "*", round(1-p, 3), "}{", n, "}} $$"))  
      
      limit <- p - z * sqrt(p*(1-p)/n)
      
      cat(paste("$$ \\pi", side, round(limit, 3), "$$"))
    }
  }
}


## Mean CI
mean.ci <- function(x.mean, 
                    x.sigma, 
                    x.n, 
                    alpha = 0.05, 
                    side = "ne", 
                    N = NA, 
                    total = FALSE){
  if(x.n > 30){
    if (!is.na(N)){
      
    }
    cat("Eftersom $n > 30$ approximeras $t$ med $z$.")
    
    if(side == "ne"){
      z <- qnorm(1 - alpha/2)
      
      cat(paste("$$ ", round(x.mean, 3), "\\pm", round(z, 2), " * \\frac{", round(x.sigma, 3), "}{\\sqrt{ ", x.n, "}} $$"))  
      
      lower <- x.mean - z * x.sigma/sqrt(x.n)
      upper <- x.mean + z * x.sigma/sqrt(x.n)
      
      cat(paste("$$ ", round(x.mean, 3), "\\pm", round(z * x.sigma/sqrt(x.n), 3), " $$"))  
      
      cat(paste("$$ ", round(lower, 3), " \\le \\mu \\le", round(upper, 3), "$$"))
    } else if(side == "<"){
      z <- qnorm(1-alpha)
      
      cat(paste("$$ ", round(x.mean, 3), "+", round(z, 2), " * \\frac{", round(x.sigma, 3), "}{\\sqrt{ ", x.n, "}} $$"))  
      
      limit <- x.mean + z * x.sigma/sqrt(x.n)
      
      cat(paste("$$ \\mu", side, round(limit, 3), "$$"))
      
    } else {
      z <- qnorm(1-alpha)
      
      cat(paste("$$ ", round(x.mean, 3), "-", round(z, 2), " * \\frac{", round(x.sigma, 3), "}{\\sqrt{ ", x.n, "}} $$"))  
      
      limit <- x.mean - z * x.sigma/sqrt(x.n)
      
      cat(paste("$$ \\mu", side, round(limit, 3), "$$"))
    }
  } else {
    cat("Eftersom $n < 30$ måste $t$-fördelningen användas.")
    if(!is.na(N)){
      if(side == "ne"){
        cat("$$ \\bar{x}-t_{n-1;1-\\alpha}*\\sqrt{\\frac{s_x^2}{n}\\left(1-\\frac{n}{N}\\right)} $$")
        t <- qt(p = 1-alpha/2, df = n-1)
          
        cat(paste("$$ ", round(x.mean, 3), "\\pm", round(t, 2), " * \\sqrt{\\frac{", round(x.sigma, 3), "^2}{", x.n, "}\\left(1-\\frac{", x.n, "}{", N, "}\\right)} $$"))  
        
        lower <- x.mean - t * sqrt((x.sigma^2/x.n)*(1-x.n/N))
        upper <- x.mean + t * sqrt((x.sigma^2/x.n)*(1-x.n/N))
        
        cat(paste("$$ ", round(x.mean, 3), "\\pm", round(t * sqrt((x.sigma^2/x.n)*(1-x.n/N)), 3), " $$"))  
        
        cat(paste("$$ ", round(lower, 3), " \\le \\mu \\le", round(upper, 3), "$$"))
        
      }    
    } else {
      # using the t distribution
      if(side == "ne"){
        t <- qt(p = 1 - alpha/2, df = x.n - 1)
        
        cat(paste("$$ ", round(x.mean, 3), "\\pm", round(t, 2), " * \\frac{", round(x.sigma, 3), "}{\\sqrt{ ", x.n, "}} $$"))  
        
        lower <- x.mean - t * x.sigma/sqrt(x.n)
        upper <- x.mean + t * x.sigma/sqrt(x.n)
        
        cat(paste("$$ ", round(x.mean, 3), "\\pm", round(t * x.sigma/sqrt(x.n), 3), " $$"))  
        
        cat(paste("$$ ", round(lower, 3), " \\le \\mu \\le", round(upper, 3), "$$"))
      } else if(side == "<"){
        t <- qt(1-alpha, df = x.n - 1)
        
        cat(paste("$$ ", round(x.mean, 3), "+", round(t, 2), " * \\frac{", round(x.sigma, 3), "}{\\sqrt{ ", x.n, "}} $$"))  
        
        limit <- x.mean + t * x.sigma/sqrt(x.n)
        
        cat(paste("$$ \\mu", side, round(limit, 3), "$$"))
        
      } else {
        t <- qt(1-alpha, df = x.n - 1)
        
        cat(paste("$$ ", round(x.mean, 3), "-", round(t, 2), " * \\frac{", round(x.sigma, 3), "}{\\sqrt{ ", x.n, "}} $$"))  
        
        limit <- x.mean - t * x.sigma/sqrt(x.n)
        
        cat(paste("$$ \\mu", side, round(limit, 3), "$$"))
      }
    }
  }
  
  if(total & side == "ne"){
    
    cat(paste("$$ N*", round(lower, 3), " \\le N * \\mu \\le N *", round(upper, 3), "$$"))
    cat(paste("$$ ", N * round(lower, 3), " \\le N * \\mu \\le ", N*round(upper, 3), "$$"))
    
    
  }
}

## Mean CI for differences
mean.diff.ci <- function(x.mean, x.sigma, x.n, alpha = 0.05, side = "ne"){
  if(length(x.mean) != length(x.sigma)){
    stop("Lengths of the means and standard deviations are different.")
  } 
  
  if(all(x.n > 30)){
    cat("Eftersom $n > 30$ approximeras $t$ med $z$.")
    
    if(side == "ne"){
      z <- qnorm(1 - alpha/2)
      
      cat(paste("$$ (", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                ")\\pm", round(z, 2), " *\\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}} $$"))  
      
      lower <- (x.mean[1] - x.mean[2]) - z * sqrt(sum(x.sigma^2/x.n))
      upper <- (x.mean[1] - x.mean[2]) + z * sqrt(sum(x.sigma^2/x.n))
      
      cat(paste("$$ ", round((x.mean[1] - x.mean[2]), 3),"\\pm", round(z * sqrt(sum(x.sigma^2/x.n)), 3), "$$"))
      
      cat(paste("$$ ", round(lower, 3), " \\le \\mu_1 - \\mu_2 \\le", round(upper, 3), "$$"))
    } else if(side == "<"){
      z <- qnorm(1-alpha)
      
      cat(paste("$$ (", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                ")+", round(z, 2), " *\\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}} $$"))  
      
      limit <- (x.mean[1] - x.mean[2]) + z * sqrt(sum(x.sigma^2/x.n))
      
      cat(paste("$$ \\mu_1 - \\mu_2", side, round(limit, 3), "$$"))
      
    } else {
      z <- qnorm(1-alpha)
      
      cat(paste("$$ (", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                ")-", round(z, 2), " *\\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}} $$"))  
      
      limit <- (x.mean[1] - x.mean[2]) - z * sqrt(sum(x.sigma^2/x.n))
      
      cat(paste("$$ \\mu_1 - \\mu_2", side, round(limit, 3), "$$"))
    }
  } else {
    # using the t distribution
    cat("Eftersom $n^* < 30$ måste $t$-fördelningen användas.")
    
    if(side == "ne"){
      t <- qt(1-(alpha/2), df = min(x.n) - 1)
      
      cat(paste("$$ (", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                ")\\pm", round(t, 2), " *\\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}} $$"))  
      
      lower <- (x.mean[1] - x.mean[2]) - t * sqrt(sum(x.sigma^2/x.n))
      upper <- (x.mean[1] - x.mean[2]) + t * sqrt(sum(x.sigma^2/x.n))
      
      cat(paste("$$ ", round((x.mean[1] - x.mean[2]), 3),"\\pm", round(t * sqrt(sum(x.sigma^2/x.n)), 3), "$$"))
      
      cat(paste("$$ ", round(lower, 3), " \\le \\mu_1 - \\mu_2 \\le", round(upper, 3), "$$"))
    } else if(side == "<"){
      t <- qt(1-alpha, df = min(x.n) - 1)
      
      cat(paste("$$ (", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                ")+", round(t, 2), " *\\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}} $$"))  
      
      limit <- (x.mean[1] - x.mean[2]) + t * sqrt(sum(x.sigma^2/x.n))
      
      cat(paste("$$ \\mu_1 - \\mu_2", side, round(limit, 3), "$$"))
      
    } else {
      t <- qt(1-alpha, df = min(x.n) - 1)
      
      cat(paste("$$ (", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                ")-", round(t, 2), " *\\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}} $$"))  
      
      limit <- (x.mean[1] - x.mean[2]) - t * sqrt(sum(x.sigma^2/x.n))
      
      cat(paste("$$ \\mu_1 - \\mu_2", side, round(limit, 3), "$$"))
    }
  }
}

## Proportion CI for differences
prop.diff.ci <- function(x, n, alpha = 0.05, side = "ne"){
  if(length(n) != length(x)){
    stop("Lengths of n and p are different.")
  } else {
    p <- x/n
  }
  
  if(any(n*p*(1-p) < 5)){
    stop("Populations not deemed normally distributed.")
  }
  
  # State information known from the data
  cat(paste("$$ p_1 = ", round(p[1], 3), "$$"))
  cat(paste("$$ p_2 = ", round(p[2], 3), "$$"))
  cat(paste("$$ n_1 = ", n[1], "$$"))
  cat(paste("$$ n_2 = ", n[2], "$$"))
  
  if(side == "ne"){
    z <- qnorm(1 - alpha/2)
    
    cat(paste("$$ (", round(p[1], 3), "-", round(p[2], 3), 
              ")\\pm", round(z, 2), " *\\sqrt{ \\frac{", round(p[1], 3), "(1-", round(p[1], 3), 
              ")}{", n[1], "} + \\frac{", round(p[2], 3), "(1-", round(p[2], 3), 
              ")}{", n[2], "}} $$"))  
    
    lower <- (p[1] - p[2]) - z * sqrt(sum(p*(1-p)/n))
    upper <- (p[1] - p[2]) + z * sqrt(sum(p*(1-p)/n))
    
    cat(paste("$$ ", round((p[1] - p[2]), 3),"\\pm", round(z * sqrt(sum(p*(1-p)/n)), 3), "$$"))
    
    cat(paste("$$ ", round(lower, 3), " \\le \\pi_1 - \\pi_2 \\le", round(upper, 3), "$$"))
  } else if(side == "<"){
    z <- qnorm(1-alpha)
    
    cat(paste("$$ (", round(p[1], 3), "-", round(p[2], 3), 
              ")+", round(z, 2), " *\\sqrt{ \\frac{", round(p[1], 3), "(1-", round(p[1], 3), 
              ")}{", n[1], "} + \\frac{", round(p[2], 3), "(1-", round(p[2], 3), 
              ")}{", n[2], "}} $$")) 
    
    limit <- (p[1] - p[2]) + z * sqrt(sum(p*(1-p)/n))
    
    cat(paste("$$ \\pi_1 - \\pi_2", side, round(limit, 3), "$$"))
    
  } else {
    z <- qnorm(1-alpha)
    
    cat(paste("$$ (", round(p[1], 3), "-", round(p[2], 3), 
              ")-", round(z, 2), " *\\sqrt{ \\frac{", round(p[1], 3), "(1-", round(p[1], 3), 
              ")}{", n[1], "} + \\frac{", round(p[2], 3), "(1-", round(p[2], 3), 
              ")}{", n[2], "}} $$")) 
    
    limit <- (p[1] - p[2]) - z * sqrt(sum(p*(1-p)/n))
    
    cat(paste("$$ \\pi_1 - \\pi_2", side, round(limit, 3), "$$"))
  }
}