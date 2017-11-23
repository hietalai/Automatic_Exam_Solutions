#### Solver for questions regarding set theory


# Independent events

check.independence <- function(pr.a, pr.b, pr.ab){
  symbol <- ifelse(pr.a * pr.b == pr.ab, "=", "\\ne") 
  
  cat("Kontrollera uttrycket:")
  cat(paste("$$ Pr(A)*Pr(B) = Pr(A \\cap B) $$"))
  cat(paste("$$ ", pr.a, "*", pr.b, symbol, pr.ab, "$$"))
  
  if(nchar(symbol) == 1){
    cat("Händelserna är oberoende.")
  } else {
    cat("Händelserna är beroende.")    
  }
}


# Calculate union

union <- function(pr.a, pr.b, pr.ab, complement = FALSE){
  if(complement){
    cat(paste("$$ \\overline{Pr(A \\cup B)} = \\overline{Pr(A) + Pr(B) - Pr(A \\cap B)} $$"))
    
    cat(paste("$$ \\overline{Pr(A \\cup B)} = 1 - \\left(", pr.a, "+ ", pr.b, "- ", pr.ab, "\\right) = ", round(1 - (pr.a + pr.b - pr.ab), 3), "$$"))
    
    
  } else {
    cat(paste("$$ Pr(A \\cup B) = Pr(A) + Pr(B) - Pr(A \\cap B) $$"))
    
    cat(paste("$$ Pr(A \\cup B) = ", pr.a, "+ ", pr.b, "- ", pr.ab, " = ", pr.a + pr.b - pr.ab, "$$"))
  }
  
}

# Calculate conditional probabilites

cond <- function(pr.a = NA, pr.b = NA, pr.ab = NA, independent = FALSE, order = "ab"){
  if(independent){
    pr.ab <- round(pr.a * pr.b, 2)
  }
  
  if(order == "ab"){
    cat(paste("$$ Pr(A | B) = \\frac{Pr(A \\cap B)}{Pr(B)} $$"))
    
    cat(paste("$$ Pr(A | B) = \\frac{", pr.ab, "}{ ", pr.b, "} = ", round(pr.ab / pr.b, 2), "$$"))
  } else {
    cat(paste("$$ Pr(B | A) = \\frac{Pr(A \\cap B)}{Pr(A)} $$"))
    
    cat(paste("$$ Pr(B | A) = \\frac{", pr.ab, "}{ ", pr.a, "} = ", round(pr.ab / pr.a, 2), "$$"))
  }
}
