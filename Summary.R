#install.packages("pacman")
pacman::p_load(pacman)
library(datasets)

# Show summary
summary(iris$Species)
summary(iris$Septal.Length)
summary(iris)

# Load psych package
p_load(psych)

# Get info on package
p_help(psych)
p_help(psych, web = F)

# Describe
describe(iris$Sepal.Length)
describe(iris)
