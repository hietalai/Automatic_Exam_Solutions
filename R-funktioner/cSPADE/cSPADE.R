require(arulesSequences)
require(dplyr)

A <- factor(x = sample(1:3, size = 9, replace = TRUE))
B <- factor(x = sample(1:3, size = 9, replace = TRUE))
C <- factor(x = sample(1:3, size = 9, replace = TRUE))
ID <- rep(1:3, times = c(2, 4, 3))
TID <- c(1:2, 1:4, 1:3)


df <- data.frame(ID = ID, TID = TID, SIZE = rep(3, length(A)), A = A, B = B, C = C)

trans <- as(df[,4:ncol(df)], "transactions")
trans@itemsetInfo <- df[,1:3]

lapply(trans@data, FUN = function(x){dplyr::filter(trans@itemInfo, x)})

filter(trans@itemInfo, trans@data[,1])
  
inspect(trans)
trans@data
trans@itemInfo

trans@data[,1]






