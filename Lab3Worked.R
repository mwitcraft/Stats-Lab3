# Task 1
getwd()

# Task 2
spruce = read.csv("SPRUCE.csv")
head(spruce)

# Task 3
x11()
#Scatter plot of data
plot(data = spruce, Height~BHDiameter, pch = 21, bg = "Blue", cex = 1.2, 
     ylim = c(0, 1.1*max(Height)), xlim = c(0, 1.1*max(BHDiameter)))
# There does appear to be a straight line relationship

# 3 Trendscatter plots
library(s20x)
x11()
layout(matrix(1:3, nr = 3))
trendscatter(Height~BHDiameter, f = 0.5, data = spruce)
trendscatter(Height~BHDiameter, f = 0.6, data = spruce)
trendscatter(Height~BHDiameter, f = 0.7, data = spruce)

spruce.lm = lm(Height~BHDiameter, data = spruce)
plot(data = spruce, Height~BHDiameter, pch = 21, bg = "Blue", cex = 1.2, 
     ylim = c(0, 1.1*max(Height)), xlim = c(0, 1.1*max(BHDiameter)))
abline(spruce.lm)

# generated from the trendscatter plot are a better fit

# Task 4
x11()
layout(matrix(1:4, nr = 2, nc = 2, byrow = TRUE))
layout.show(4)
# Plot scatter and fitted line
plot(data = spruce, Height~BHDiameter, pch = 21, bg = "Blue", cex = 1.2, 
     ylim = c(0, 1.1*max(Height)), xlim = c(0, 1.1*max(BHDiameter)))
abline(spruce.lm)

yhat = with(spruce, predict(spruce.lm, data.frame(BHDiameter)))
plot(data = spruce, Height~BHDiameter, pch = 21, bg = "Blue", cex = 1.2, 
     ylim = c(0, 1.1*max(Height)), xlim = c(0, 1.1*max(BHDiameter)))
with(spruce, segments(BHDiameter, Height, BHDiameter, yhat))
abline(spruce.lm)

plot(data = spruce, Height~BHDiameter, pch = 21, bg = "Blue", cex = 1.2, 
     ylim = c(0, 1.1*max(Height)), xlim = c(0, 1.1*max(BHDiameter)))
with(spruce, abline(h = mean(Height)))
abline(spruce.lm)
with(spruce, segments(BHDiameter, mean(Height), BHDiameter, yhat, col = "Red"))

plot(data = spruce, Height~BHDiameter, pch = 21, bg = "Blue", cex = 1.2, 
     ylim = c(0, 1.1*max(Height)), xlim = c(0, 1.1*max(BHDiameter)))
with(spruce, abline(h = mean(Height)))
with(spruce, segments(BHDiameter, Height, BHDiameter, mean(Height), col = "Green"))

TSS = with(spruce, sum((Height - mean(Height))^2))
TSS
MSS = with(spruce, sum((yhat - mean(Height))^2))
MSS
RSS = with(spruce, sum((Height - yhat)^2))
RSS
RSS + MSS

MSS/TSS
  # MSS/TSS is R^2, or how close the data is to matching the regression line

# Task 5
summary(spruce.lm)
coef(spruce.lm)
  # Value of slope is 0.4814743
  # Value of intercept is 9.1468390
  # Equation of fitted line: Height = 9.1468390 + 0.4814743*BHDiameter
predict(spruce.lm, data.frame(BHDiameter = c(15, 18, 20)))
  # Value for 15 is column 1, 18 column 2, 20, column 3

# Task 6
x11()
library(ggplot2)
g = ggplot(spruce, aes(x = BHDiameter, y = Height, colour = BHDiameter)) 
g = g + geom_point() + geom_line() + geom_smooth(method = "lm")
g + ggtitle("Height vs BHDiameter")

# Task 7


