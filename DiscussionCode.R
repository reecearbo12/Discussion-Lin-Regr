#Question 1a

set.seed(1000)

#Population Line
w = rep(0:10, 2)
z = 90 - w

popsigma <- var(z)

popmod <- lm(z~w)

#Sample Line
x = rep(0:10, 2)
y = 90 - x + rnorm(22, 0, 5)

mymod <- lm(y~x)

plot(x, y)
abline(a = 90, b = -1, col = "blue")
abline(a = mymod$coefficients[1], b = mymod$coefficients[2], col = "darkgray")

#Question 1b

confinterval <- confint(mymod, level = 0.95)
confinterval

#Question 2a

set.seed(1000)

n <- 10000
slope <- as.vector(matrix(data=0,nrow=n,ncol=1))
intercept <- as.vector(matrix(data=0,nrow=n,ncol=1))
mse <- as.vector(matrix(data=0,nrow=n,ncol=1))
rho <- as.vector(matrix(data=0,nrow=1,ncol=1))

for (i in 1:n){
  x = rep(0:10, 2)
  y = 90 - x + rnorm(22, 0, 5)
  model <- lm(y~x)
  slope[i] <- model$coefficients[2]
  intercept[i] <- model$coefficients[1]
  mse[i] <- sum((model$residuals)**2) / (20)
  rho[i] <- slope[i] * (sd(x) / sd(y))
  
  plot(x,y)
  abline(a = 90, b = -1, col = "blue")
  abline(a = mymod$coefficients[1], b = mymod$coefficients[2], col = "darkgray")
}

#Question 2b
x2 <- seq(min(intercept), max(intercept), length = 100)
theoreticalsdb0 <- mean(mse) * sqrt(1/20 + mean(x)**2 / sum((x - mean(x))**2))
fun <- dnorm(x2, mean = mean(intercept), sd = sd(intercept))

hist(intercept, nclass = 100, prob = TRUE)
lines(x2, fun, col = "blue")

mean(intercept)

#Question 2c
theoreticalsdb1 <- mean(mse) / sum((x - mean(x))**2)

x2 <- seq(min(slope), max(slope), length = 100)
fun <- dnorm(x2, mean = mean(slope), sd = sd(slope))

hist(slope, nclass = 100, prob = TRUE)
lines(x2, fun, col = "blue")

mean(slope)

#Question 2d
hist(rho, nclass = 100)

#Question 2e
hist(mse, nclass = 100)

mean(mse)

#Question 2f
u = 20 * mse / popsigma
dom <- seq(min(u), max(u), length = 1000)
fun <- dchisq(dom, df = 47)

hist(u, nclass = 100, freq = FALSE)
lines(dom, fun, col = "blue")

mean(u)
var(u)
