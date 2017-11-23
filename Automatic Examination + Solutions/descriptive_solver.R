##### Solver for decriptive statistics

# Function for the mean
mean.desc <- function(x){
  cat(paste("$$ \\bar{x} = \\frac{\\sum{x}}{n} = \\frac{", sum(x), "}{", length(x), "}",
            " = ", round(sum(x)/length(x), 3), "$$"))
  
}

# Function for the standard deviation

std.desc <- function(x){
  
  std <- sqrt((sum(x^2)-sum(x)^2/length(x))/(length(x)-1))
  cat(paste("$$ s_x = \\sqrt{\\frac{\\sum{x^2}-\\frac{\\left(\\sum{x}\\right)^2}{n}}{n-1}} = ",
            "\\sqrt{\\frac{", sum(x^2), "- \\frac{", sum(x), "^2}{", length(x), "}}",
            "{", length(x) - 1, "}} = ", round(std, digits = 3), "$$"))
  
}

# Function for the correlation

cor.desc <- function(x, y, simple = TRUE){
  
  sumx <- round(sum(x), 3)
  sumy <- round(sum(y), 3)
  sumx2 <- round(sum(x^2), 3)
  sumy2 <- round(sum(y^2), 3)
  sumxy <- round(sum(x*y), 3)
  
  
  cor <- cor(x, y)
  
  if(simple){
    cat(paste("$$ r_{x,y} = \\frac{\\sum{x*y}-\\frac{\\sum{x}*\\sum{y}}{n}}
              {\\sqrt{\\sum{x^2}-\\frac{\\left(\\sum{x}\\right)^2}{n}*\\sum{y^2}-\\frac{\\left(\\sum{y}\\right)^2}{n}}} = ",
              "\\frac{", sumxy, "- \\frac{", sumx, "*", sumy, "}{", length(x), "}}",
              "{\\sqrt{", sumx2, "- \\frac{", sumx, "*", sumx, "}{", length(x), 
              "}*", sumy2, "- \\frac{", sumy, "*", sumy, "}{", length(y), "}}} = ", round(cor, digits = 3), "$$"))
    
    
  } else{
    cat(paste("$$ r_{x,y} = \\frac{\\sum{x*y}-\\frac{\\sum{x}*\\sum{y}}{n}}{(n-1)*s_x*s_y} = ",
              "\\frac{", sum(x*y), "- \\frac{", sum(x), "*", sum(y), "}{", length(x), "}}",
              "{", length(x)-1, "*", round(sd(x), 3),"*", round(sd(y), 3), "} = ", round(cor, digits = 3), "$$"))
    }
  
}





