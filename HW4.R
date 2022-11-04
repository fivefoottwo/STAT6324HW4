#Q-8A
set.seed (1)
x <- rnorm (100)
y <- x - 2 * x ^2 + rnorm (100)

#Q-8B
plot(x, y)

#Q-8C
library(boot)
set.seed (1)
data <- data.frame(x,y)
glm.fit.1 <- glm(y ~ x, data=data)
cv.glm(data, glm.fit.1)$delta[1]

glm.fit.2 <- glm(y ~ poly(x,2))
cv.glm(data, glm.fit.2)$delta[1]

glm.fit.3 <- glm(y ~ poly(x,3))
cv.glm(data, glm.fit.3)$delta[1]

glm.fit.4 <- glm(y ~ poly(x,4))
cv.glm(data, glm.fit.4)$delta[1]

#Q-8D
set.seed (100)
data <- data.frame(x,y)
glm.fit.1 <- glm(y ~ x, data=data)
cv.glm(data, glm.fit.1)$delta[1]

glm.fit.2 <- glm(y ~ poly(x,2))
cv.glm(data, glm.fit.2)$delta[1]

glm.fit.3 <- glm(y ~ poly(x,3))
cv.glm(data, glm.fit.3)$delta[1]

glm.fit.4 <- glm(y ~ poly(x,4))
cv.glm(data, glm.fit.4)$delta[1]

#Q-8F
summary(glm.fit.4)


#Q-9A
library(ISLR2)
attach(Boston)
mu.hat <- mean(medv)
mu.hat

#Q-9B
se.hat <- sd(medv)/sqrt(dim(Boston)[1])
se.hat

#Q-9C
set.seed(1)
boot.fn <- function(data, index) {
  mu <- mean(data[index])
  return (mu)
}
boot(medv, boot.fn, 1000)

#Q-9D
CI <- c(22.53281 - 2 * 0.4119374, 22.53281 + 2 * 0.4119374)
CI

t.test(Boston$medv)

#Q-9E
median.hat <- median(medv)
median.hat

#Q-9F
boot.fn <- function(data, index) {
  med <- median(data[index])
  return (med)
}
boot(medv, boot.fn, 1000)

#Q-9G
perc <- quantile(medv, c(0.1))
perc

#Q-9H
boot.fn <- function(data, index) {
  perc <- quantile(data[index], c(0.1))
  return (perc)
}
boot(medv, boot.fn, 1000)