library(datasets)
head(mtcars)
par(mfrow=c(1,1))
hist(mtcars$wt)
hist(mtcars$mpg)

# Plot scatter plot
plot(mtcars$wt,mtcars$mpg,
     col = "#cc0000",
     cex = 1.5,
     main = "MPG as a function of weight of cars",
     xlab = "Weight (in 1000 pounds)",
     ylab = "MPG")
