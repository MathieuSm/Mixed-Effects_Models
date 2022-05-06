library(nlme)
library(ggplot2)

# Exercise 1
Theoph.df <- data.frame(Theoph)
Theoph.df$Subject <- factor(as.numeric(as.character(Theoph.df$Subject)), ordered = FALSE)

# a)
plot(Theoph.df$Time, Theoph.df$conc, xlab='Time [hours]', ylab='Concentration [mg/l]')

# b) single nls fit
Theoph.nls.1 <- nls(conc ~ Dose * exp(Phi1) * exp(Phi2) / (exp(Phi3) * (exp(Phi2) - exp(Phi1))) * (exp(-exp(Phi1)*Time) - exp(-exp(Phi2)*Time)), start = c(Phi1=-2.5 , Phi2=0.4, Phi3=-3), data=Theoph.df)
Theoph.nls.1

# or:
Theoph.nls.1 <- nls(conc ~ SSfol(Dose, Time, lKe, lKa, lCl), data=Theoph.df)

plot(Theoph.df$Time, Theoph.df$conc, xlab='Time [hours]', ylab='Concentration [mg/l]')
lines(0:131, predict(Theoph.nls.1, data.frame(Time=0:25)))

# variance of the residuals increases over time:
plot(orange.df$age, residuals(orange.nls.1))
abline(h=0, lty='dotted')


# c) fit nls for each subject individually
Theoph.nlsList.1 <- nlsList(conc ~ SSfol(Dose, Time, lKe, lKa, lCl) | Subject, data=Theoph.df)
Theoph.nlsList.1

Theoph.df$Pred <- predict(Theoph.nlsList.1)
ggplot(Theoph.df,aes(x=Time,y=conc,color=Subject)) + geom_point()+geom_line(aes(y=Pred))

plot(intervals(Theoph.nlsList.1))


# d) (starting from the nlsList fit ==> different fit...)
Theoph.nlme.1 <- nlme(Theoph.nlsList.1, random = lKa + lCl ~ 1 | Subject)
Theoph.nlme.1

# e) See latex

# f)
vc <- VarCorr(Theoph.nlme.1)
corr <- matrix(as.numeric(vc[-c(1, nrow(vc)),3:ncol(vc)]), nrow=nrow(vc)-2, ncol=nrow(vc)-2)
corr <- rbind(rep(0, ncol(corr)+1), cbind(corr, rep(0, nrow(corr))))
corr <- corr + t(corr)
diag(corr) <- 1
sdev <- as.numeric(vc[-nrow(vc),'StdDev'])
Psi <- outer(sdev, sdev) * corr

sigma <- as.numeric(vc[nrow(vc), 'StdDev'])
sigma^2

ranef(Theoph.nlme.1)

coef(Theoph.nlme.1)[1,]  # this is the same as phi-hat_1j = beta-hat + b-hat_1:
fixef(Theoph.nlme.1) + ranef(Theoph.nlme.1)[1,]

coef(Theoph.nlme.1)


# g)
Theoph.nlme.2 <- nlme(Theoph.nlsList.1, random = list(Subject = pdDiag(lKa + lCl ~ 1)))
anova(Theoph.nlme.1, Theoph.nlme.2, type='marginal')
# p-value very high -> simpler model prefered

# h)
# Fitting quality
plot(fitted(Theoph.nlme.2), Theoph.df$conc, xlab='Fitted values', ylab='Observed values')
abline(0,1)

# Within-group errors assumption
boxplot(resid(Theoph.nlme.2) ~ Theoph.df$Subject, xlab='Subject number', ylab='Residuals')
abline(h=0, lty='dotted')
plot(Theoph.nlme.2, resid(.) ~ fitted(.) | Subject, xlab='Fitted values', ylab='Redisuals', abline = 0, lty='dotted')
plot(Theoph.nlme.2, resid(.) ~ Time | Subject, xlab='Time', ylab='Redisuals', abline = 0, lty='dotted')

plot(Theoph.df$Time, residuals(Theoph.nlme.2))
abline(h=0, lty='dotted')

library(car)
qqPlot(residuals(Theoph.nlme.2))

# i)
Theoph.nlme.3 <- nlme(Theoph.nlsList.1, random = list(Subject = pdDiag(lKa + lCl ~ 1)),
                      weights = varConstPower(form = ~ fitted(.), const = 1, power = 0.5))

# j) see latex



# Exerercise 2
Pixel.df <- data.frame(Pixel)
Pixel.df$Dog <- factor(as.numeric(levels(Pixel.df$Dog))[Pixel.df$Dog], ordered = FALSE)
ggplot(Pixel.df,aes(x=day,y=pixel,color=Dog)) + geom_point()

# b)
Pixel.nls.1 <- nls(pixel ~ c + a * (b*day)^3 / (exp(b*day) - 0.999), start = c(a=-35 , b=1, c=1000), data=Pixel.df)

# c)
Pixel.nlsList.1 <- nlsList(pixel ~ c + a * (b*day)^3 / (exp(b*day) - 0.999) | Dog/Side,
                           start = c(a=26 , b=0.41, c=1068), data=Pixel.df)
Pixel.nlme.1 <- nlme(pixel ~ c + a * (b*day)^3 / (exp(b*day) - 0.999),
                     start = c(a=26 , b=0.41, c=1068), fixed = a + b + c ~ 1,
                     random = a + c ~ 1 | Dog/Side, data=Pixel.df,
                     control = list(returnObject=TRUE, msMaxIter=1000, eval.max=1000))

# d)
Pixel.nlme.2 <- nlme(pixel ~ c + a * (b*day)^3 / (exp(b*day) - 0.999),
                     start = c(a=26 , b=0.41, c=1068), fixed = a + b + c ~ 1,
                     random = list(Dog = a + b + c ~ 1, Side = a + c ~ 1), data=Pixel.df,
                     control = list(returnObject=TRUE, msMaxIter=1000, eval.max=1000))



# Exercise 3
Ovary.df <- data.frame(Ovary)
Ovary.df$Mare <- factor(as.numeric(levels(Ovary.df$Mare))[Ovary.df$Mare], ordered = FALSE)

# a)
Ovary.lme <- lme(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), data = Ovary.df,
                random = list(Mare = pdDiag(~sin(2*pi*Time))), corr = corARMA(p = 1, q = 1))
Ovary.nlme.1 <- nlme(follicles ~ A + B * sin(2 * pi * w * Time) + C * cos(2 * pi * w *Time),
                    data = Ovary.df, fixed = A + B + C + w ~ 1, random = list(Mare = pdDiag(A + B + w ~ 1)),
                    start = c(fixef(Ovary.lme), 1))

# Inspect for autocorrelation
plot( ACF(Ovary.nlme.1, maxLag = 10), alpha = 0.05 )

# Build models with correlation structure determined from ACF
Ovary.nlme.2 <- nlme(follicles ~ A + B * sin(2 * pi * w * Time) + C * cos(2 * pi * w *Time),
                    data = Ovary.df, fixed = A + B + C + w ~ 1, random = list(Mare = pdDiag(A + B + w ~ 1)),
                    start = c(fixef(Ovary.lme), 1), corr = corAR1(0.311))
Ovary.nlme.3 <- nlme(follicles ~ A + B * sin(2 * pi * w * Time) + C * cos(2 * pi * w *Time),
                    data = Ovary.df, fixed = A + B + C + w ~ 1, random = list(Mare = pdDiag(A + B + w ~ 1)),
                    start = c(fixef(Ovary.lme), 1), corr = corARMA(p=0, q=2))

# Compare models with appropriate test
anova(Ovary.nlme.2, Ovary.nlme.3, test = F)

# Compute 95% confidence intervals
intervals(Ovary.nlme.2, which = 'fixed')



## Continue from page 398 (410 in pdf)