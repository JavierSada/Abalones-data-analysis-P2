Programming Intro

> View(abalones)
> mydata <- abalones
> str(mydata) 
> library(ggplot2)
> library(moments)
> library(rockchalk)
> library(gridExtra)
> mydata$VOLUME <- mydata$LENGTH * mydata$DIAM * mydata$HEIGHT
> mydata$RATIO <- mydata$SHUCK / mydata$VOLUME
> mydata$SEX <- factor(mydata$SEX)

Programming 1a
> par(mfrow=c(1,2)
> hist(mydata$RATIO, main = "Histogram of Ratio Values", cex.main = 1.5, xlab = "Ratio Vaues", ylab = "Frequency", cex.lab = 0.8, cex.axis= 0.8, panel.first = grid(12), col = "dodgerblue3")
> abline(v=median(mydata$RATIO), col = "firebrick3", lty = 3, lwd = 2)
> abline(v=mean(mydata$RATIO), col = "green3", lwd = 2, lty = 3)
> curve(dnorm(x, mean=mean(mydata$RATIO), sd=sd(mydata$RATIO)), add=TRUE, col = "goldenrod2", lwd = 2)
> legend("topright", c("Density", "Mean", "Median"), col = c("goldenrod2", "green3", "firebrick3"), lwd = c(1.5, 1.5, 1.5), cex = 0.7, bg = "transparent", bty = "n")

> qqnorm(mydata$RATIO, main = "QQ Plot of Ratio", cex.main = 1.5, panel.first = grid(10), cex.axis = 0.8, cex.lab = 0.8, col = "dodgerblue3", pch = 18)
> qqline(mydata$RATIO, col = "firebrick3", lwd = 2)

> skewness(mydata$RATIO)
> kurtosis(mydata$RATIO)

Programming 1b
> mydata$L_RATIO <- log10(mydata$RATIO)
> skewness(mydata$L_RATIO)
> kurtosis(mydata$L_RATIO)
> par(mfrow=c(1,2))
> hist(mydata$L_RATIO, main = "Histogram of L Ratio Values", cex.main = 1.5, xlab = "Ratio Vaues", ylab = "Frequency", cex.lab = 0.8, cex.axis= 0.8, panel.first = grid(12), col = "dodgerblue3")
> abline(v=median(mydata$L_RATIO), col = "firebrick3", lty = 3, lwd = 2)
> abline(v=mean(mydata$L_RATIO), col = "green3", lwd = 2, lty = 3)
> curve(dnorm(x, mean=mean(mydata$L_RATIO), sd=sd(mydata$L_RATIO)), add=TRUE, col = "goldenrod2", lwd = 2)
> legend("topright", c("Density", "Mean", "Median"), col = c("goldenrod2", "green3", "firebrick3"), lwd = c(1.5, 1.5, 1.5), cex = 0.7, bg = "transparent", bty = "n")

> qqnorm(mydata$L_RATIO, main = "QQ Plot of Ratio", cex.main = 1.5, panel.first = grid(10), cex.axis = 0.8, cex.lab = 0.8, col = "dodgerblue3", pch = 18)
> qqline(mydata$L_RATIO, col = "firebrick3", lwd = 2)

> ggplot(mydata, aes(x = mydata$CLASS, y = mydata$L_RATIO, fill = mydata$CLASS)) +
+ labs(title="Plot of L Ratio  per class",x="Abalone Class", y = "Abalone L Ratio") +
+ scale_fill_discrete(name="Class")+
+ geom_boxplot()

Programming 1c
> bartlett.test(mydata$L_RATIO~mydata$CLASS,data = mydata)

Programming 2a 
> L_Ratio_Anova <- aov(mydata$L_RATIO ~ mydata$CLASS*mydata$SEX, data = mydata)
> summary(L_Ratio_Anova)

> L_Ratio_Anova2 <- aov(mydata$L_RATIO ~ mydata$CLASS+mydata$SEX, data = mydata)
> summary(L_Ratio_Anova2)

Programming 2b
> TukeyHSD(L_Ratio_Anova2, conf.level = 0.95)
> plot(tuk)

Programming 3a
> mydata$TYPE <- combineLevels(mydata$SEX, levs = c("F","M"), "ADULT")
> levels(mydata$TYPE)
> table(mydata$TYPE)
> addmargins(table)
> base <- data.frame(mydata$TYPE, mydata$VOLUME)
> infant <- subset(base, mydata$TYPE =="I")
> adult <- subset(base, mydata$TYPE =="ADULT")
> par(mfrow=c(1,2))
> hist(infant$mydata.VOLUME, main = "Histogram of Infant volumes", cex.main = 1.5, panel.first = grid(10), xlab = "Abalone Volume", ylab = "Frequency", cex.lab = 0.8, cex.axis = 0.8, col = "dodgerblue3")
> hist(adult$mydata.VOLUME, main = "Histogram of Adult volumes", cex.main = 1.5, panel.first = grid(10), xlab = "Abalone Volume", ylab = "Frequency", cex.lab = 0.8, cex.axis = 0.8, col = "dodgerblue3")

Programming 3b
> mydata$L_SHUCK <- log10(mydata$SHUCK)
> mydata$L_VOLUME <- log10(mydata$VOLUME)
> grid.arrange(
+ ggplot(mydata, aes(x=mydata$VOLUME, y=mydata$SHUCK, fil = mydata$CLASS))+
+ labs(title="Scatter Plot Abalone Shuck vs Volume by Class",x="Abalone Volume", y = "Abalone Shuck", linetype = "Class") +
+ geom_point(aes(color=mydata$CLASS)),
> ggplot(mydata, aes(x=mydata$L_VOLUME, y=mydata$L_SHUCK, fil = mydata$CLASS))+
+ labs(title="Scatter Plot Abalone L_Shuck vs L_Volume by Class",x="Abalone L_Volume", y = "Abalone L_Shuck", linetype = "Class") +
+ geom_point(aes(color=mydata$CLASS))
+ )

> grid.arrange(
> ggplot(mydata, aes(x= mydata$VOLUME, y= mydata$SHUCK, color = factor(mydata$TYPE, labels = c("Infant", "Adult"))))+
+ labs(color = "Type", title="Scatter Plot Abalone Shuck vs Volume by Type",x="Abalone Volume", y = "Abalone Shuck", linetype = "Class") +
+ geom_point()
> ggplot(mydata, aes(x= mydata$L_VOLUME, y= mydata$L_SHUCK, color = factor(mydata$TYPE, labels = c("Infant", "Adult"))))+
+ labs(color = "Type", title="Scatter Plot Abalone L Shuck vs L Volume by Type",x="Abalone L Volume", y = "Abalone L Shuck", linetype = "Class") +
+ geom_point()
+ )

Programming 4a
> linear_model <- lm(mydata$L_SHUCK ~ mydata$L_VOLUME + mydata$CLASS + mydata$TYPE, data = mydata)
> summary(linear_model)

> ggplot(linear_model, aes(x=mydata$L_VOLUME, y= linear_model$residuals, color = factor(mydata$TYPE, labels = c("Infant", "Adult"))))+
+ labs(color = "Type", title="Scatter Plot Residuals vs L Volume by Type",x="Abalone L Volume", y = "Residuals", linetype = "Class") +
+ geom_point

Programming 5a
> par(mfrow=c(1,2))
> hist(linear_model$residuals, main = "Histogram of Residuals", cex.main = 1.5, xlab = "Residuals", ylab = "Frequency", cex.lab=0.8, cex.axis = 0.8, panel.first = grid(10), col = "dodgerblue3")
> qqnorm(linear_model$residuals, main = "QQ Plot of Residuals", cex.main = 1.5, panel.first = grid(10), cex.axis = 0.8, cex.lab = 0.8, col = "dodgerblue3", pch = 18)
> qqline(linear_model$residuals, col = "firebrick3", lwd = 2)

> skewness(linear_model$residuals)
> kurtosis(linear_model$residuals)

Programming 5b
> grid.arrange(
+ ggplot(linear_model, aes(x=mydata$L_VOLUME, y= linear_model$residuals, color = factor(mydata$CLASS)))+
+ labs(color = "Class", title="Scatter Plot Residuals vs L Volume by Class",x="Abalone L Volume", y = "Residuals", linetype = "Class") +
+ geom_point(),

> ggplot(linear_model, aes(x=mydata$L_VOLUME, y= linear_model$residuals, color = factor(mydata$TYPE, labels = c("Infant", "Adult"))))+
+ labs(color = "Type", title="Scatter Plot Residuals vs L Volume by Type",x="Abalone L Volume", y = "Residuals", linetype = "Type")+
+ geom_point()

> grid.arrange(
+ ggplot(mydata, aes(x=mydata$CLASS, y=linear_model$residuals, color = factor(mydata$CLASS)))+
+ labs(color = "Class", title = "Boxplot of Residuals by Class", x = "Abalone Class", y= "Linear Model Residuals")+
+ geom_boxplot(),
+ ggplot(mydata, aes(x=mydata$TYPE, y=linear_model$residuals, color = factor(mydata$TYPE, labels = c("Infant", "Adult"))))+
+ labs(color = "Type", title = "Boxplot of Residuals by Type", x = "Abalone Type", y= "Linear Model Residuals")+
+ geom_boxplot()
+ )

> bartlett.test(linear_model$residuals ~ mydata$CLASS, data = mydata)

Programming 6a

> idxi <- mydata$TYPE=="I"
> idxa <- mydata$TYPE=="ADULT"
> max.v <- max(mydata$VOLUME)
> min.v <- min(mydata$VOLUME)
> delta <- (max.v - min.v)/1000
> prop.infants <- numeric(0)
> prop.adults <- numeric(0)
> volume.value <- numeric(0)
> total.infants <- length(mydata$TYPE[idxi])
> total.adults <- length(mydata$TYPE[idxa])
> for (k in 1:1000) {
+ value <- min.v + k*delta
+ volume.value[k] <- value
+ prop.infants[k] <- sum(mydata$VOLUME[idxi] <= value)/total.infants
+ prop.adults[k] <- sum(mydata$VOLUME[idxa] <= value)/total.adults
+ }
> n.infants <- sum(prop.infants <= 0.5)
> split.infants <- min.v + (n.infants + 0.5)*delta
> n.adults <- sum(prop.adults <= 0.5)
> split.adults <- min.v + (n.adults + 0.5)*delta
> head(prop.adults, 20)
> head(prop.infants, 20)
> head(volume.value, 20)

Programming 6b
> plot(c(0,1001), c(0.0,1.01), panel.first = grid(12), main = "Porportion of Adults and Infants protected", cex.main = 1.5, xlab = "Volume", ylab = "Proportions", cex.lab = 0.8, cex.axis = 0.8, col = "white")
> lines(volume.value, prop.infants, col = "dodgerblue3", lwd = 2)
> lines(volume.value, prop.adults, col = "firebrick3", lwd = 2)
> legend("bottomright", legend = c("Infants", "Adults"), col = c("dodgerblue3", "firebrick3"), cex=0.8, lty=1, box.lty = 0, bty = "n")
> abline(h=0.5, v= split.adults, lty = 2, lwd = 1.5)
> abline(v=split.infants, lty=2, lwd =1.5)
> text(x= 200, y=0.43, labels = " 147.95", cex = 0.7, pos = 3, offset = 0.5, font = 2)
> text(x= 420, y=0.43, labels = " 389.02", cex = 0.7, pos = 3, offset = 0.5, font = 2)

Programming 7a
> difference <- (1-prop.adults) - (1-prop.infants)
> y.loess.a <- loess(1-prop.adults ~ volume.value, span = 0.25, family = c("symmetric"))
> y.loess.i <- loess(1-prop.infants ~ volume.value, span = 0.25, family = c("symmetric"))
> smooth.difference <- predict(y.loess.a) - predict(y.loess.i)
> plot(c(0,1000), c(0.0,0.52), panel.first = grid(18), main = "Difference in Harvet Proportions", cex.main = 1.5, xlab = "Volume", ylab = "Difference in Proportions Harvested", cex.lab = 0.8, cex.axis = 0.8, col = "white")
> lines(smooth.difference, col = "dodgerblue3", lwd=2)
> lines(difference, col= "firebrick3", lwd =2)
> q7am <- volume.value[which.max(smooth.difference)]
> abline(v=q7am, lty=2, lwd =1.5)
> text(x=280, y= 0.43, labels = "Max Volume = 281.389", cex = 0.7, pos = 3, offset = 0.5, font = 2)

Programming 7c
> q7c <- (1-prop.infants)[which.max(smooth.difference)]

Programming 7d
> q7i <- (1-prop.infants)[which.max(smooth.difference)]
> q7a <- (1-prop.adults)[which.max(smooth.difference)]
> q7i
> q7a

Programming 8a
> vi <- volume.value[volume.value > max(mydata[mydata$CLASS == "A1" & mydata$TYPE == "I", "VOLUME"])][1]
> round(vi, digits = 0)
> svi <- sum(mydata[mydata$TYPE == "I", "VOLUME"] > 206.9844) / sum(mydata$TYPE == "I")
> sva <- sum(mydata[mydata$TYPE == "ADULT", "VOLUME"] > 206.9844) / sum(mydata$TYPE == "ADULT")
> svi
> sva
> q8m <- volume.value[which.min(abs(prop.adults - (1-prop.infants)))]
> q8m
> q8mi <- sum(mydata[mydata$TYPE == "I", "VOLUME"] > 253.6113) / sum(mydata$TYPE == "I")
> q8ma <- sum(mydata[mydata$TYPE == "ADULT", "VOLUME"] > 253.6113) / sum(mydata$TYPE == "ADULT")
> q8ma
> q8mi

Programming 8b
> vmx <- volume.value[volume.value > max(mydata[mydata$CLASS == "A1" & mydata$TYPE == "I", "VOLUME"])][1]
> ve <- volume.value[which.min(abs(prop.infants - (1-prop.adults)))]
> phi <- sum(mydata[mydata$TYPE == "I", "VOLUME"] > 206.9844) / sum(mydata$TYPE == "I")
> phe <- sum(mydata[mydata$TYPE == "I", "VOLUME"] > 253.6113) / sum(mydata$TYPE == "I")

Programming 9a
> library(flux)
> zero <- auc((1-prop.infants), (1-prop.adults))
> zero

> plot(c(0,1.005), c(0,1.005), col = "white", panel.first = grid(17), main = "ROC curve of Adult and Infant harvested proportions", xlab = "Infant proportion", ylab = "Adult Proportion", cex.lab = 0.8, cex.axis = 0.8)
> lines((1-prop.infants), (1-prop.adults), col = "dodgerblue3", lwd = 2)
> abline(a=0, b=1, col = "firebrick3", lwd =2, lty=2)
> points(svi, sva, type = "o", pch=19:25)
> text(svi, sva, labels = "Zero Harvest A1 = 207", cex = 0.7, pos = 4, offset = 0.5, font = 2)
> points(q7i, q7a, type = "o", pch=19:25)
> text(x=.210, y=0.699, labels= "Max. Diff = 281.4", cex = 0.7, pos = 4, offset = 0.5, font = 2)
> points(x=q8mi, y=q8ma, type = "o", pch=19:25)
> text(x=0.18, y=0.76, labels = "Equal harvest = 253.6", cex = 0.7, pos = 3, offset = 0.5, font = 2)

Programming 10
> pymh <- sum(mydata$VOLUME >= volume.value[which.max(smooth.difference)])/ (total.adults + total.infants)
> pyzh <- sum(mydata$VOLUME > 206.9844)/ (total.adults + total.infants)
> pyeh <- sum(mydata$VOLUME >= volume.value[which.min(abs(prop.adults - (1-prop.infants)))])/ (total.adults + total.infants)
> prop_yeild <- c(pymh)
> FPR <- c(svi, q8mi, q7i)
> TPR <- c(sva, q8ma, q7a)
> vol <- c(vi, q8m, q7am)
> prop_yeild <- c(pyzh, pyeh, pymh)
> df <- data.frame(vol, TPR, FPR, prop_yeild)
> colnames(df) <- c("Volume", "TPR", "FPR", "Prop_Yield")
> rownames(df) <- c("Zero Harvest", "Equal Harvest", "Max Harvest")
