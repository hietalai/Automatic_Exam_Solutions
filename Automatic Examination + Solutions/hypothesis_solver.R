### Hypothesis testing

require(xtable)

# Function that produces results of test decision
decision <- function(test, krit, side = "ne"){
  if(side == "ne"){
    if(test > -krit & test < krit){
      cat(paste("Teststatistikan hamnar ej i det kritiska området."))
      decision <- "ej"
    } else {
      cat(paste("Teststatistikan hamnar i det kritiska området."))
      decision <- ""
    }
  } else if(side == "<"){
    if(test > -krit){
      cat(paste("Teststatistikan hamnar ej i det kritiska området."))
      decision <- "ej"
    } else {
      cat(paste("Teststatistikan hamnar i det kritiska området."))
      decision <- ""
    }
  } else {
    if(test < krit){
      cat(paste("Teststatistikan hamnar ej i det kritiska området."))
      decision <- "ej"
    } else {
      cat(paste("Teststatistikan hamnar i det kritiska området."))
      decision <- ""
    }
  }
  return(decision)
}

## Testing differences of proportions
prop.dif.test <- function(n, x, d0 = 0, alpha = 0.05, side = "ne"){
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
  
  # Calculate pooled proportions
  pp <- sum(x)/sum(n)
    
  cat(paste("$$ p_p = \\frac{", sum(x),"}{", sum(n) , "} = ", round(pp, 3), "$$"))
  
  # Present hypotheses
  cat(paste("$$ H_0: \\pi_1 - \\pi_2 =", d0, "$$"))
  if(nchar(side) > 1){
    cat(paste("$$ H_a: \\pi_1 - \\pi_2", paste("\\", side, sep = ""), d0, "$$"))
  } else {
    cat(paste("$$ H_a: \\pi_1 - \\pi_2", side, d0, "$$"))
  }
  
  # Calculating the test statistic
  if(d0 == 0){
    cat(paste("$$ z_{test} = \\frac{", round(p[1], 3), "-", round(p[2], 3), 
              "}{ \\sqrt{", round(pp, 3), "*", round(1-pp, 3), "* \\left( \\frac{1}{", n[1], "} + \\frac{1}{", n[2], "} \\right)}} $$"))
    
    cat(paste("$$ z_{test} = \\frac{", round(p[1] - p[2], 3), "}{", round(sqrt(pp*(1-pp)*(sum(1/n))), 3), "} $$"))
    
    ztest <- (p[1] - p[2])/sqrt(pp*(1-pp)*(sum(1/n)))
    cat(paste("$$ z_{test} = ", round(ztest, 2), " $$"))
  } else {
    # hypothesis testing without pooled proportion
    cat("Nothing created here yet")
  }
  
  # Calculating the critical value
  if(nchar(side) > 1){
    zkrit <- qnorm(1-(alpha/2))
    cat(paste("$$ z_{krit} = z_{1 - \\alpha/2} = \\pm", round(zkrit, 2), "$$"))
  } else {
    zkrit <- qnorm(1-alpha)
    if(side == "<"){
      cat(paste("$$ z_{krit} = z_{\\alpha} = -", round(zkrit, 2), "$$"))  
    } else {
      cat(paste("$$ z_{krit} = z_{1 - \\alpha} = ", round(zkrit, 2), "$$"))
    }
  }
  
  # Decision
  beslut <- decision(test = ztest, krit = zkrit, side = side)
  
  cat(paste(" $H_0$ kan ", beslut, " förkastas."))
  
}

## Testing differences of two means
mean.dif.test <- function(x.mean, x.sigma, x.n, d0 = 0, alpha = 0.05, 
                          side = "ne", approx = TRUE, var.equal = TRUE){
  if(length(x.mean) != length(x.sigma)){
    stop("Lengths of the means and standard deviations are different.")
  } 
  
  # Present hypotheses
  cat(paste("$$ H_0: \\mu_1 - \\mu_2 =", d0, "$$"))
  if(nchar(side) > 1){
    cat(paste("$$ H_a: \\mu_1 - \\mu_2", paste("\\", side, sep = ""), d0, "$$"))
  } else {
    cat(paste("$$ H_a: \\mu_1 - \\mu_2", side, d0, "$$"))
  }
  if(approx){
    if(min(x.n) > 30){
      cat("Eftersom $n^* > 30$ approximeras $t$ med $z$.")
      
      # Calculating the test statistic
      cat(paste("$$ z_{test} = \\frac{", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                "}{ \\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}}} $$"))
      
      cat(paste("$$ z_{test} = \\frac{", round(x.mean[1] - x.mean[2], 3), "}{", round(sqrt(sum(x.sigma^2/x.n)), 3), "} $$"))
      
      test <- (x.mean[1] - x.mean[2])/sqrt(sum(x.sigma^2/x.n))
      cat(paste("$$ z_{test} = ", round(test, 2), " $$"))
      
      # Calculating the critical value
      if(nchar(side) > 1){
        krit <- qnorm(1-(alpha/2))
        cat(paste("$$ z_{krit} = z_{1 - \\alpha/2} = \\pm", round(krit, 2), "$$"))
      } else {
        krit <- qnorm(1-alpha)
        if(side == "<"){
          cat(paste("$$ z_{krit} = z_{\\alpha} = -", round(krit, 2), "$$"))  
        } else {
          cat(paste("$$ z_{krit} = z_{1 - \\alpha} = ", round(krit, 2), "$$"))
        }
      }
    } else {
      # T-test
      cat("Eftersom $n^* < 30$ måste $t$-fördelningen användas.")
      
      # Calculating the test statistic
      cat(paste("$$ t_{test} = \\frac{", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                "}{ \\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}}} $$"))
      
      cat(paste("$$ t_{test} = \\frac{", round(x.mean[1] - x.mean[2], 3), "}{", round(sqrt(sum(x.sigma^2/x.n)), 3), "} $$"))
      
      test <- (x.mean[1] - x.mean[2])/sqrt(sum(x.sigma^2/x.n))
      cat(paste("$$ t_{test} = ", round(test, 2), " $$"))
      
      # Calculating the critical value
      if(nchar(side) > 1){
        krit <- qt(1-(alpha/2), df = min(x.n) - 1)
        cat(paste("$$ t_{krit} = t_{1 - \\alpha/2} = \\pm", round(krit, 2), "$$"))
      } else {
        krit <- qt(1-alpha, df = min(x.n) - 1)
        if(side == "<"){
          cat(paste("$$ t_{krit} = t_{\\alpha} = -", round(krit, 2), "$$"))  
        } else {
          cat(paste("$$ t_{krit} = t_{1 - \\alpha} = ", round(krit, 2), "$$"))
        }
      }
    }
  } else {
    if(all(x.n > 30)){
      cat("Eftersom båda grupperna har n > 30 approximeras $t$ med $z$.")
      
      # Calculating the test statistic
      cat(paste("$$ z_{test} = \\frac{", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                "}{ \\sqrt{ \\frac{", round(x.sigma[1], 3), "^2}{", x.n[1], "} + \\frac{", round(x.sigma[2], 3), "^2}{", x.n[2], "}}} $$"))
      
      cat(paste("$$ z_{test} = \\frac{", round(x.mean[1] - x.mean[2], 3), "}{", round(sqrt(sum(x.sigma^2/x.n)), 3), "} $$"))
      
      test <- (x.mean[1] - x.mean[2])/sqrt(sum(x.sigma^2/x.n))
      cat(paste("$$ z_{test} = ", round(test, 2), " $$"))
      
      # Calculating the critical value
      if(nchar(side) > 1){
        krit <- qnorm(1-(alpha/2))
        cat(paste("$$ z_{krit} = z_{1 - \\alpha/2} = \\pm", round(krit, 2), "$$"))
      } else {
        krit <- qnorm(1-alpha)
        if(side == "<"){
          cat(paste("$$ z_{krit} = z_{\\alpha} = -", round(krit, 2), "$$"))  
        } else {
          cat(paste("$$ z_{krit} = z_{1 - \\alpha} = ", round(krit, 2), "$$"))
        }
      }
    } else if(var.equal == TRUE){
      # Calculating the test statistic
      cat(paste("Vi antar här att varianserna är lika för de två grupperna."))
      pooled_var <- sum((x.n - 1)*x.sigma^2)/(sum(x.n) - 2)
      
      cat(paste("$$ s_p^2 = \\frac{(n_1 - 1)*s_1^2 + (n_2 - 1)*s_2^2}{(n_1 + n_2 - 2)} $$"))
      
      cat(paste("$$ = \\frac{", (x.n[1] - 1), "*", round(x.sigma[1], 3), "^2 + ",
                                (x.n[2] - 1), "*", round(x.sigma[2], 3), "^2}
                {", (sum(x.n) - 2), "} = ", round(pooled_var, 3), " $$"))
      
      cat(paste("$$ t_{test} = \\frac{", round(x.mean[1], 3), "-", round(x.mean[2], 3), 
                "}{ \\sqrt{", round(pooled_var, 3), " * \\left(\\frac{1}{", x.n[1], "} + \\frac{1}{", x.n[2], "}\\right)}} $$"))
      
      cat(paste("$$ t_{test} = \\frac{", round(x.mean[1] - x.mean[2], 3), "}{", round(sqrt(sum(pooled_var/x.n)), 3), "} $$"))
      
      test <- (x.mean[1] - x.mean[2])/sqrt(sum(pooled_var/x.n))
      cat(paste("$$ t_{test} = ", round(test, 2), " $$"))
      
      # Calculating the critical value
      if(nchar(side) > 1){
        krit <- qt(1-(alpha/2), df = sum(x.n) - 2)
        cat(paste("$$ t_{krit} = t_{(n_1 + n_2 -2), 1 - \\alpha/2} = \\pm", round(krit, 2), "$$"))
      } else {
        krit <- qt(1-alpha, df =  sum(x.n) - 2)
        if(side == "<"){
          cat(paste("$$ t_{krit} = t_{(n_1 + n_2 -2), \\alpha} = -", round(krit, 2), "$$"))  
        } else {
          cat(paste("$$ t_{krit} = t_{(n_1 + n_2 -2), 1 - \\alpha} = ", round(krit, 2), "$$"))
        }
      }
    }

    
  }
 
  # Decision
  beslut <- decision(test = test, krit = krit, side = side)
  
  cat(paste(" $H_0$ kan ", beslut, " förkastas."))
  
}

## Chi-square test
chi.square <- function(obs, alpha = 0.05){
  exp <- obs
  
  for(i in 1:nrow(exp)){
    exp[i,] <- colSums(obs)*sum(exp[i,])/sum(obs)
  }
  
  print(xtable(exp, digits = c(1, rep(2, ncol(exp))), caption = "Förväntade frekvenser (E)"), comment = FALSE)

  if(any(exp < 1, sum(exp < 5) > (ncol(exp)*nrow(exp)*0.2))){
    
    cat("Kraven uppfylls inte och ett $\\chi^2$-test kan ej beräknas")
    
  } else {
    # Present hypotheses
    cat(paste("$$ H_0:~Det~finns~inga~skillnader $$"))
    cat(paste("$$ H_a:~Det~finns~skillnader $$"))
    
    # Calculate statistic
    chisqtest <- sum((obs-exp)^2/exp)
    
    cat("$$ \\chi_{test}^2 = \\sum{\\frac{(O-E)^2}{E}} = ", round(chisqtest, 3), "$$")
    
    # Critical value
    chisqcrit <- qchisq(1-alpha, (nrow(obs)-1)*(ncol(obs)-1))
    
    cat("$$ \\chi_{krit}^2 = \\chi^2_{(", nrow(obs), "-1)(", ncol(obs), " -1);", alpha, "} = ", round(chisqcrit, 3), "$$")
    
    # Decision
    beslut <- decision(chisqtest, chisqcrit, side = "ge")
    
    cat(paste(" $H_0$ kan ", beslut, " förkastas."))
  }
 
}



