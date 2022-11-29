library(readxl)
library(data.table)
library(dplyr)
df <- read_excel("D:/User Files/Matthew/R Package For Dad/Database.xlsx")

xCoordinates <- c()
yCoordinates <- c()

cutoff <- sort(unique(df$rating))
for(cut in cutoff){
  Confidence <- df[df$rating >= cut,]
  TPPrecentage <- nrow(Confidence[Confidence$target == 1,])/nrow(df[df$target == 1,])
  FPThingy <- nrow(Confidence[Confidence$target == 0,])/length(unique(df$case))
  xCoordinates = append(xCoordinates, TPPrecentage)
  yCoordinates = append(yCoordinates, FPThingy)
}

x <- xCoordinates
y <- yCoordinates

#plot(xCoordinates, yCoordinates, main = "FROC Curve", xlab = "TPPrecentage", ylab = "FPThingy")

mdl1 <- lm(y ~ x, data = data.frame(x,y))
mdl2 <- lm(y ~ x + I(x^2), data = data.frame(x,y))
mdl3 <- lm(y ~ x + I(x^2) + I(x^3), data = data.frame(x,y))
mdl4 <- lm(y ~ I(x^2), data = data.frame(x,y))

prd <- data.frame(x = seq(0, 50, by = 0.5))

result <- prd
result$mdl1 <- predict(mdl1, newdata = prd)
result$mdl2 <- predict(mdl2, newdata = prd)
result$mdl3 <- predict(mdl3, newdata = prd)
result$mdl4 <- predict(mdl4, newdata = prd)

library(reshape2)
library(ggplot2)

result <-  melt(result, id.vars = "x", variable.name = "model",
                value.name = "fitted")
ggplot(result, aes(x = x, y = fitted)) +
  theme_bw() +
  geom_point(data = data.frame(x,y), aes(x = x, y = y)) +
  geom_line(aes(colour = model), size = 1)