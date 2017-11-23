### Combinatorics solutions

char.counter <- function(str){
  
  table <- table(str_split(str, pattern = ""))
  
  return(table)
  
}

permutations <- function(n, k, repl = FALSE, k.ident = NULL){
  require(xtable, quietly = TRUE)
  
  #k.ident is vector containing the number in each group of elements.
  if(is.null(k.ident)){
    if(repl){
      cat("Permutationer med återläggning.")
      
      perm <- n^k
      
      cat("$$ P^{'k}_n = n^k = ", perm, "$$")
      
    } else {
      cat("Permutationer utan återläggning.")
      
      perm <- factorial(n)/factorial(n-k)
      
      cat("$$ P^{k}_n = \\frac{", n, "!}{(", n, "-", k, ")!} = ", perm, "$$")
    }  
  } else {
    cat("Permutationer utan återläggning där vissa element är lika.")
    
    perm <- factorial(n)/prod(factorial(k.ident))
    
    print(xtable(x = t(k.ident)), include.rownames = FALSE, comment = FALSE)
    
    cat("$$ P^{k_1, k_2, ...}_n = \\frac{", n, "!}{", paste(paste(k.ident, "!*", sep = ""), collapse = ""), "} = ", perm, "$$")
  }
  
  
}