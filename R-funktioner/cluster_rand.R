writing <- function(data){
  dir <- "F:/OneDrive/OneDrive - Linköpings universitet/Jobb/LiU/Kurser/6. Avancerade kurser i statistik T5-6/732G12 - Data Mining/2016/Tenta/2017-03-30"
  setwd(dir)
  write.csv(data, file = "./cluster.csv", row.names=F)
}

# ### Groups of clusters with varied densities
# x1 <- rnorm(200, mean = -10, sd = 1)
# y1 <- rnorm(200, mean = -10, sd = 1)
# 
# x2 <- rnorm(100, mean = -5, sd = 1)
# y2 <- rnorm(100, mean = -5, sd = 1)
# 
# x3 <- rnorm(100, mean = -9, sd = 1)
# y3 <- rnorm(100, mean = -1, sd = 1)
# 
# x <- append(append(x1, x2), x3)
# y <- append(append(y1, y2), y3)
# 
# plot(x,y)
# 
# data1 <- cbind(x,y)
# #data2 <- cbind(x,y)
# 
# data1[,2]<- data1[,2]*4



### Noise density clusters
noise_x <- seq(-5, 5, by = 0.5)
noise_y <- seq(-5, 5, by = 0.5)

noise <- expand.grid(noise_x, noise_y)

white_noise <- matrix(rnorm(n = nrow(noise)*2, sd = 0.5), nrow = nrow(noise))
noise <- noise + white_noise

too_long <- which(apply(X = noise, MARGIN = 1, FUN = function(x) any(x < -5 |x > 5)))
noise <- noise[-too_long,]

x1 <- rnorm(400, mean = 2, sd = 0.5)
y1 <- rnorm(400, mean = 1, sd = 1)

x2 <- rnorm(400, mean = -3, sd = 0.5)
y2 <- rnorm(400, mean = 0, sd = 1)

x <- as.matrix(append(x1, x2))
y <- as.matrix(append(y1, y2))

too_long <- which(x > 5 | x < -5 | y > 5 | y < -5)
if(length(too_long) > 0){
  x <- x[-too_long]
  y <- y[-too_long]
}

data <- data.frame(x, y)
colnames(data) <- c("Var1", "Var2")

data2 <- rbind(noise, data)
colnames(data2) <- c("x", "y")

plot(data2)

writing(data2)

# ### Create circular shapes
# if(!require(mvtnorm)){
#   install.packages(("mvtnorm"))
#   require(mvtnorm)
# }
# # mean = c(position of x center, position of y center, position of majority of points (higher the close to the center))
# x <- matrix(rmvnorm(1000, mean = c(0,0,1.5)), ncol=3)
# y1 <- x/sqrt(rowSums(x^2))
# y3 <- y2 <- y1
# y2[,2] <- y2[,2]+3
# y3[,1] <- y3[,1]+1.8
# y3[,2] <- y3[,2]+1.5
# data3 <- rbind(y1, y2, y3)
# 
# colnames(data3) <- c("x", "y", "z")
# 
# plot(data3[,1], data3[,2])
# 
# writing(data3[,1:2])

# ### Create oval shapes
# rx <- 3
# ry <- 1
# 
# sample_circle <- function(rx = 1, ry = 1){
#   x <- runif(1000, -rx, rx)
#   y <- runif(1000, -ry, ry)
#   
#   temp <- data.frame(x,y)
#   
#   index <- (temp[,1]^2/rx^2)+(temp[,2]^2/ry^2) < 1
#   
#   data <- data.frame(temp[index,])
#   
#   return(data)
# } 
# 
# data <- sample_circle(3, 1)
# data2 <- sample_circle(3, 1)
# data3 <- sample_circle(1, 1) 
# data4 <- sample_circle(1, 1)
# 
# data2[,1] <- data2[,1] + 5
# data2[,2] <- data2[,2] + 2.2
# data3[,1] <- data3[,1] + 5
# data4[,2] <- data4[,2] + 2.2
# 
# data5 <- rbind(data, data2, data3, data4)
# colnames(data4) <- c("x", "y")
# 
# plot(data5)

# ### Create arbitrary shapes
# x1 <- runif(500, min = -5, max = 2)
# y1 <- sin(x1) - runif(length(x1))
# 
# x2 <- runif(500, min = -5, max = 2)
# y2 <- sin(x2) + 1.5 - runif(length(x2))
# 
# x <- append(x1, x2)
# y <- append(y1, y2)
# 
# data6 <- cbind(x, y)
# colnames(data6) <- c("X", "Y")
# 
# plot(data6)

# ### Circular clusters with noise
# if(!require(mvtnorm)){
#   install.packages(("mvtnorm"))
#   require(mvtnorm)
# }
# # mean = c(position of x center, position of y center, position of majority of points (higher the close to the center))
# x <- matrix(rmvnorm(1000, mean = c(0,0,1.5)),nc=3)
# y1 <- x/sqrt(rowSums(x^2))
# y2 <- y1
# y2[,2] <- y2[,2]+3
# 
# noise_nr <- 500
# x_noise <- runif(noise_nr, min = -2, max = 2)
# y_noise <- runif(noise_nr, min = -3, max = 6)
# 
# noise <- cbind(x_noise, y_noise, rep.int(1, length(y_noise)))
# 
# data <- rbind(y1[,-3], y2[,-3], noise[,-3])
# 
# plot(data)
# 
# colnames(data) <- c("x", "y")



