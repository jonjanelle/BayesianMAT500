library(car)
library(MASS)

elsi <- read.csv(file.choose())

names(elsi)
str(elsi)

for (i in 5:6){elsi[,i]<-elsi[,i]/elsi[,7]}
elm <- lm(elsi[,4]^3~elsi[,2]+elsi[,3]+elsi[,5]+elsi[,6]+elsi[,9])
summary(elm)
windows()
par(mfrow=c(2,2))
plot(elm)
shapiro.test(resid(elm))
ncvTest(elm)
boxcox(elm, lambda = seq(1,6,by = 0.01))

elm1 <- lm(elsi[,4]^3.25~elsi[,2]+elsi[,3]+elsi[,5]+elsi[,6]+elsi[,9])
summary(elm1)
plot(elm1)

shapiro.test(resid(elm1))
ncvTest(elm1)

library(faraway)
for (i in 1:4){prplot(elm1, i)}
outlierTest(elm1)

elm2 <- update(elm1, .~.-elsi[,6])
#reduced model preferred
anova(elm2, elm1)
summary(elm2)
names(elsi)

plot(elsi[,2],elsi[,4])
windows()
par(mfrow = c(2,3))
for (i in 2:6){hist(elsi[,i], xlab = colnames(elsi)[i])}

summary(elm2)