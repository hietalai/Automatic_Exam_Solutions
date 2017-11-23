
## Author: https://stats.stackexchange.com/users/8772/darrelkj

library(tau)
library(diagram) 

#this will load the string
x <- tokenize("Questions must be at least 2 days old to be eligible for a bounty. There can only be 1 active bounty per question at any given time. Users must have at least 75 reputation to offer a bounty, and may only have a maximum of 3 active bounties at any given time. The bounty period lasts 7 days. Bounties must have a minimum duration of at least 1 day. After the bounty ends, there is a grace period of 24 hours to manually award the bounty. If you do not award your bounty within 7 days (plus the grace period), the highest voted answer created after the bounty started with at least 2 upvotes will be awarded half the bounty amount. If there's no answer meeting that criteria, the bounty is not awarded to anyone. If the bounty was started by the question owner, and the question owner accepts an answer during the bounty period, and the bounty expires without an explicit award - we assume the bounty owner liked the answer they accepted and award it the full bounty amount at the time of bounty expiration. In any case, you will always give up the amount of reputation specified in the bounty, so if you start a bounty, be sure to follow up and award your bounty to the best answer! As an additional bonus, bounty awards are immune to the daily reputation cap and community wiki mode.")

#the number of tokens in the string
n <- length(x)

list <- NULL

count <- 1

#this will remove spaces, list is new string with no spaces
for (i in 1:n) {
  if (x[i] != " ") {
    list[count] <- x[i]
    count <- count + 1
  }
}

#the unique words in the string
y <- unique(list)

#number of tokens in the string
n <- length(list)
#number of distinct tokens
m <- length(y)


#assign tokens to values
ind <- NULL
val <- NULL
#make vector of numbers in place of tokens
for (i in 1:m) {
  ind[i] <- i
  for (j in 1:n) {
    if (y[i] == list[j]) {
      val[j] = i
    } 
  }
}


d <- array(0, c(m, m))

#this finds the number of count of the word after the current word
for (i in 1:(n-1)) {
  d[val[i], val[i+1]] <- d[val[i], val[i+1]] + 1
}

dd <- t(d)
dd[dd < 2] <- 0

i <- apply(dd, MARGIN = 2, function(x) any(x != 0)) | apply(dd, MARGIN = 1, function(x) any(x != 0))

y_new <- y[i]

dd_new <- dd[i,i]

# Col = from, rows = to
plotmat(dd_new, box.size = 0.05, name = y_new, lwd = 2 * dd_new, cex.txt = 0)
