library(nlme)

# create a data frame from the grouped data object, omitting the ordering of rats
bodyweight.df <- data.frame(BodyWeight)
bodyweight.df$Rat <- factor(as.numeric(levels(bodyweight.df$Rat))[bodyweight.df$Rat], ordered = FALSE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/bodyweight.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(bodyweight.df$Time, bodyweight.df$weight, xlab='day', ylab='weight', type='n', xlim=c(1, 64), ylim=c(200, 700), axes=FALSE)
axis(1, at=seq(1, 64, 7))
axis(2)
box()
for (r.index in 1:length(unique(bodyweight.df$Rat))) {
  r <- unique(bodyweight.df$Rat)[r.index]
  lines(bodyweight.df$Time[bodyweight.df$Rat==r], bodyweight.df$weight[bodyweight.df$Rat==r], 
    lty=c(2, 1, 3)[as.numeric(bodyweight.df$Diet[bodyweight.df$Rat==r])[1]], lwd=2, 
    type='o', pch=c(4, 1, 3)[as.numeric(bodyweight.df$Diet[bodyweight.df$Rat==r])[1]])
}
legend('topleft', pch=c(3, 1, 4), lty=c(3, 1, 2), paste('diet', c(3, 2, 1)), lwd=2)
dev.off()
} # end if (FALSE)


# homoscedastic model

bodyweight.lme.1 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  data=bodyweight.df)

# residuals vs. fitted values
if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/bodyweight.lme.1.resid.vs.fitted.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(fitted(bodyweight.lme.1), resid(bodyweight.lme.1), xlim=c(200, 700), ylim=c(-15, 15), 
  pch=c(4, 1, 3)[as.numeric(bodyweight.df$Diet)],
  xlab='Fitted values', ylab='Residuals')
abline(h=0, lty='dotted')
legend('topright', pch=c(3, 1, 4), paste('diet', c(3, 2, 1)))
dev.off()
} # end if (FALSE)


# heteroscedastic models

# variance proportional to a power of the fitted values
bodyweight.lme.2 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varPower(form = ~ fitted(.)), data=bodyweight.df)
summary(bodyweight.lme.2)
anova(bodyweight.lme.1, bodyweight.lme.2)

plot(fitted(bodyweight.lme.2), resid(bodyweight.lme.2, type='pearson'), xlim=c(200, 700), ylim=c(-3, 3), 
  pch=c(4, 1, 3)[as.numeric(bodyweight.df$Diet)],
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')
legend('topright', pch=c(3, 1, 4), paste('diet', c(3, 2, 1)))

# different variances for the three diet groups
bodyweight.lme.2a <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varIdent(form = ~ 1 | Diet), data=bodyweight.df)
summary(bodyweight.lme.2a)
anova(bodyweight.lme.1, bodyweight.lme.2a)

plot(fitted(bodyweight.lme.2a), resid(bodyweight.lme.2a, type='pearson'), xlim=c(200, 700), ylim=c(-3, 3), 
  pch=c(4, 1, 3)[as.numeric(bodyweight.df$Diet)],
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')
legend('topright', pch=c(3, 1, 4), paste('diet', c(3, 2, 1)))

# different variance for the third diet group only
bodyweight.lme.2b <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varIdent(form = ~ 1 | Diet==3), data=bodyweight.df)
summary(bodyweight.lme.2b)
anova(bodyweight.lme.1, bodyweight.lme.2b, bodyweight.lme.2a)

plot(fitted(bodyweight.lme.2b), resid(bodyweight.lme.2b, type='pearson'), xlim=c(200, 700), ylim=c(-3, 3), 
  pch=c(4, 1, 3)[as.numeric(bodyweight.df$Diet)],
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')
legend('topright', pch=c(3, 1, 4), paste('diet', c(3, 2, 1)))



# serial correlation for all measurements

Variogram(bodyweight.lme.2, form = ~ Time)
plot(Variogram(bodyweight.lme.2, form = ~ Time))
plot(Variogram(bodyweight.lme.2, form = ~ Time, maxDist=42))
plot(Variogram(bodyweight.lme.2, form = ~ Time, maxDist=42, robust=TRUE))
# using circles with area proportional to n.pairs
Variog <- Variogram(bodyweight.lme.2, form = ~ Time, maxDist=42)
plot(Variog, cex=sqrt(Variog$n.pairs/16))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/skript_folien/bodyweight.lme.2.Variogram.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(Variogram(bodyweight.lme.2, form = ~ Time, maxDist=42), ylim=c(-0.05, 2.05),
  par.settings=list(axis.text=list(cex=1.2), par.xlab.text=list(cex=1.2), par.ylab.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)


# exponential, without nugget effect
bodyweight.lme.3 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varPower(form = ~ fitted(.)), 
  correlation = corExp(form = ~ Time | Rat), data=bodyweight.df)
bodyweight.lme.3
intervals(bodyweight.lme.3)
anova(bodyweight.lme.2, bodyweight.lme.3)
plot(Variogram(bodyweight.lme.3, form = ~ Time, maxDist=42))
plot(Variogram(bodyweight.lme.3, resType='normalized', 
  form = ~ Time, maxDist=42))
plot(Variogram(bodyweight.lme.3, resType='normalized', 
  form = ~ Time, maxDist=42, robust=TRUE))

# nugget effect added - result is different from that in the book
bodyweight.lme.4 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varPower(form = ~ fitted(.)), 
  correlation = corExp(form = ~ Time | Rat, nugget=TRUE), data=bodyweight.df)
intervals(bodyweight.lme.4)
anova(bodyweight.lme.3, bodyweight.lme.4)
plot(Variogram(bodyweight.lme.4, form = ~ Time, maxDist=42))
plot(Variogram(bodyweight.lme.4, resType='normalized', 
  form = ~ Time, maxDist=42))
plot(Variogram(bodyweight.lme.4, resType='normalized', 
  form = ~ Time, maxDist=42, robust=TRUE))


# further correlation structures, without nugget effect
bodyweight.lme.5 <- update(bodyweight.lme.3, 
  correlation=corRatio(form = ~ Time | Rat))
bodyweight.lme.6 <- update(bodyweight.lme.3, 
  correlation=corSpher(form = ~ Time | Rat))
bodyweight.lme.7 <- update(bodyweight.lme.3, 
  correlation=corLin(form = ~ Time | Rat))
bodyweight.lme.8 <- update(bodyweight.lme.3, 
  correlation=corGaus(form = ~ Time | Rat))
anova(bodyweight.lme.3, bodyweight.lme.5, bodyweight.lme.6, bodyweight.lme.7, bodyweight.lme.8)


# further correlation structures, with nugget effect
bodyweight.lme.5n <- update(bodyweight.lme.4, 
  correlation=corRatio(form = ~ Time | Rat, nugget=TRUE))
bodyweight.lme.6n <- update(bodyweight.lme.4, 
  correlation=corSpher(form = ~ Time | Rat, nugget=TRUE))
bodyweight.lme.7n <- update(bodyweight.lme.4, 
  correlation=corLin(form = ~ Time | Rat, nugget=TRUE))
bodyweight.lme.8n <- update(bodyweight.lme.4, 
  correlation=corGaus(form = ~ Time | Rat, nugget=TRUE))
anova(bodyweight.lme.4, bodyweight.lme.5n, bodyweight.lme.6n, bodyweight.lme.7n, bodyweight.lme.8n)


# time series - days 1, 8, ..., 64 only, renamed as time 1, ..., 10
# (not discussed in the lecture)

bodyweight.ts <- bodyweight.df[bodyweight.df$Time!=44,]
bodyweight.ts$Time <- (bodyweight.ts$Time - 1) / 7 + 1
bodyweight.ts.lme.2 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varPower(form = ~ fitted(.)), data=bodyweight.ts)

plot(bodyweight.ts.lme.2, resid(., type='normalized') ~ Time | Rat)

bodyweight.ts.lme.3 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varPower(form = ~ fitted(.)), 
  correlation = corAR1(form = ~ Time | Rat), data=bodyweight.ts)

anova(bodyweight.ts.lme.2, bodyweight.ts.lme.3)

plot(bodyweight.ts.lme.3, resid(., type='normalized') ~ Time | Rat)

bodyweight.ts.lme.4 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varPower(form = ~ fitted(.)), 
  correlation = corARMA(form = ~ Time | Rat, p=0, q=1), data=bodyweight.ts)

anova(bodyweight.ts.lme.2, bodyweight.ts.lme.4)




###########
# Sheet 3 #
###########

bodyweight.lme.3 <- lme(weight ~ Time * Diet, random = ~ Time | Rat, 
  weights = varPower(form = ~ fitted(.)), 
  correlation = corExp(form = ~ Time | Rat), data=bodyweight.df)

# Exercise 5.1 (d) from P&B (p. 267-268)

bodyweight.gls.1 <- gls(weight ~ Time * Diet, 
  weights = varPower(form = ~ fitted(.)),
  correlation = corCAR1(form = ~ Time | Rat), data = bodyweight.df)

anova(bodyweight.gls.1, bodyweight.lme.3)





# model equation etc. for bodyweight.lme.3

bodyweight.lme.3

bodyweight.df[bodyweight.df$Rat==16,]

# X_16:
model.matrix(weight ~ Time * Diet, data=bodyweight.df)[bodyweight.df$Rat==16,]

# Z_16:
model.matrix(weight ~ Time, data=bodyweight.df)[bodyweight.df$Rat==16,]

