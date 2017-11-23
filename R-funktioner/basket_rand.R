drinks <- c("beer", "water", "soda", "tea", "coffee", "juice")
dairy <- c("milk", "yoghurt", "ice cream", "butter") 
meat <- c("chicken", "red meat", "fish", "minced meat")
carbs <- c("pasta", "rice", "potatoes")

# Puts all items in a stock list
stock <- append(append(append(drinks, dairy), meat), carbs)
# Gives all items a weight in the sampling procedure below
weights <- c(0.25, 0.5, 1, 2)
weight <- sample(weights, size = length(stock), replace = TRUE, prob = c(0.3, 0.3, 0.4, 0.1))

# Defines number of transactions and creates template for them
nrTrans <- trunc(runif(1, min = 18000, max = 28000))
transactions <- matrix(NA, nrow = nrTrans, ncol = 2)

# Simulating transactions
for (i in 1:nrTrans){
  items <- trunc(runif(1, min = 3, max = 10))  
  transactions[i,1] <- 46706040200+i
  transactions[i,2] <- paste0(sample(stock, items, prob = weight), collapse = ",")
  
  ## Removes complementary items by a probability.
  # Drinks
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample <- sample(drinks, 2)
  if(sample[1] %in% s & 
       sample[2] %in% s
     ){
    rand <- runif(1, 0, 1)
    if(rand < 0.4){
      index <- which(s == sample[1])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    } else if (rand > 0.6){
      index <- which(s == sample[2])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    }
  }
  
  # Meats
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample <- sample(meat, 2)
  if(sample[1] %in% s & 
       sample[2] %in% s
  ){
    rand <- runif(1, 0, 1)
    if(rand < 0.3){
      index <- which(s == sample[1])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    } else if (rand > 0.7){
      index <- which(s == sample[2])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    }
  }
  
  
  # Carbs
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample <- sample(carbs, 2)
  if(sample[1] %in% s & 
       sample[2] %in% s
  ){
    rand <- runif(1, 0, 1)
    if(rand < 0.45){
      index <- which(s == sample[2])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    } else if (rand > 0.55){
      index <- which(s == sample[1])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    }
  }
  
  # Dairy
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample <- sample(dairy, 2)
  if(sample[1] %in% s & 
       sample[2] %in% s
  ){
    rand <- runif(1, 0, 1)
    if(rand < 0.8){
      index <- which(s == sample[1])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    } else if (rand > 0.9){
      index <- which(s == sample[2])
      transactions[i,2] <- paste0(s[-index], collapse = ",")
    }
  }
  
  ## Adding a dependent item.
  # Adding milk to soda
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample1 <- sample(drinks, 1)
  sample2 <- sample(dairy, 1)
  if(sample1 %in% s & 
       length(s) < 10 & 
       !(sample2) %in% s)
  {
    rand <- runif(1, 0, 1)
    if(rand < 0.4){
      s <- append(s, sample2)
      transactions[i,2] <- paste0(s, collapse = ",")
    } 
  }
  
  # Adding juice to ice cream
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample1 <- sample(drinks, 1)
  sample2 <- sample(dairy, 1)
  if(sample1 %in% s & 
      length(s) < 10 & 
      !(sample2) %in% s)
  {
    rand <- runif(1, 0, 1)
    if(rand < 0.4){
      s <- append(s, sample2)
      transactions[i,2] <- paste0(s, collapse = ",")
    } 
  }
  # Adding milk to tea
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample1 <- sample(drinks, 1)
  sample2 <- sample(dairy, 1)
  if(sample1 %in% s & 
       length(s) < 10 & 
       !(sample2) %in% s)
  {
    rand <- runif(1, 0, 1)
    if(rand < 0.4){
      s <- append(s, sample2)
      transactions[i,2] <- paste0(s, collapse = ",")
    } 
  }
  # Adding fish to rice
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample1 <- sample(carbs, 1)
  sample2 <- sample(dairy, 1)
  if(sample1 %in% s & 
     length(s) < 10 & 
     !(sample2) %in% s)
  {
    rand <- runif(1, 0, 1)
    if(rand < 0.4){
      s <- append(s, sample2)
      transactions[i,2] <- paste0(s, collapse = ",")
    } 
  }
  # Adding fish to rice
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample1 <- sample(meat, 1)
  sample2 <- sample(carbs, 1)
  if(sample1 %in% s & 
     length(s) < 10 & 
     !(sample2) %in% s)
  {
    rand <- runif(1, 0, 1)
    if(rand < 0.4){
      s <- append(s, sample2)
      transactions[i,2] <- paste0(s, collapse = ",")
    } 
  }
  # Adding fish to rice
  s <- unlist(strsplit(as.character(transactions[i,2]), ','))
  sample1 <- sample(meat, 1)
  sample2 <- sample(carbs, 1)
  if(sample1 %in% s & 
     length(s) < 10 & 
     !(sample2) %in% s)
  {
    rand <- runif(1, 0, 1)
    if(rand < 0.4){
      s <- append(s, sample2)
      transactions[i,2] <- paste0(s, collapse = ",")
    } 
  }
}

# Creates a transactional database with each row as an item and their transaction ID.
s <- strsplit(as.character(transactions[,2]), ',')

transactionsData <- data.frame(ID=rep(transactions[,1], vapply(s, FUN=length, FUN.VALUE=integer(1))), item=unlist(s))

write.csv(transactionsData, file = "./stormarknad.csv", row.names = F)













