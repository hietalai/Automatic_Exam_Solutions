install.packages("dplyr")
require(dplyr)

browseVignettes(package = "dplyr")

set.seed(128491)
n <- 1000000
TID_end <- sample(1:15, size = n, replace = TRUE)
TID <- sapply(X = TID_end, FUN = function(x){1:x})
ID <- rep(x = 1:n, times = sapply(X = TID, FUN = length))
TID <- unlist(TID)
event <- sample(x = letters, size = length(TID), replace = TRUE)

str_data <- data.frame(ID = ID, TID = TID, EVENT = event, stringsAsFactors = FALSE)
fac_data <- data.frame(ID = ID, TID = TID, EVENT = event, stringsAsFactors = TRUE)

object.size(fac_data)/object.size(str_data)














