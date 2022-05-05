library(nlme)

pixel.df <- data.frame(Pixel)
pixel.df$Dog <- factor(as.numeric(levels(pixel.df$Dog))[pixel.df$Dog], ordered = FALSE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/pixel.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(pixel.df$day, pixel.df$pixel, type='n', xlab='Time post injection (days)', ylab='Pixel intensity')
for (d in 1:max(as.numeric(pixel.df$Dog))) {
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], pixel.df$pixel[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], pixel.df$pixel[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], as.character(d), cex=0.8)
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], pixel.df$pixel[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], lty=2, type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], pixel.df$pixel[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], as.character(d), cex=0.8)
}
legend('topright', lty=c(1,2), c('left', 'right'))
dev.off()
}


pixel.lme.1 <- lme(pixel ~ day + I(day^2), random = list(Dog = ~ day, Side = ~ 1), data=pixel.df)
summary(pixel.lme.1)
VarCorr(pixel.lme.1)


detach(package:nlme)
library(lme4)

pixel.lmer.1 <- lmer(pixel ~ day + I(day^2) + (day | Dog) + (1 | Side:Dog), data=pixel.df)
pixel.lmer.1

detach(package:lme4)
library(nlme)




if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/pixel.lme.1.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(pixel.df$day, pixel.df$pixel, type='n', xlab='Time post injection (days)', ylab='Pixel intensity')
for (d in 1:max(as.numeric(pixel.df$Dog))) {
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.lme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.lme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], as.character(d), cex=0.8)
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.lme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], lty=2, type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.lme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], as.character(d), cex=0.8)
}
legend('topright', lty=c(1,2), c('left', 'right'))
x <- seq(min(pixel.df$day), max(pixel.df$day), length=100)
y <- t(fixed.effects(pixel.lme.1)) %*% rbind(rep(1, length(x)), x, x^2)
lines(x, y, lwd=3, lty='11')
dev.off()
}

# LRT for the comparison of models with different fixed effects, based on ML fits
pixel.lme.1.ml <- lme(pixel ~ day + I(day^2), random = list(Dog = ~ day, Side = ~ 1), data=pixel.df, method='ML')
pixel.lme.2.ml <- lme(pixel ~ day, random = list(Dog = ~ day, Side = ~ 1), data=pixel.df, method='ML')
anova(pixel.lme.1.ml, pixel.lme.2.ml)
# conditional test for the day^2 effect
anova(pixel.lme.1.ml)



####################
# nonlinear models #
####################

# combination of a constant and an SSfol model 

# nls doesn't converge normally
pixel.nls.0 <- nls(
  pixel ~ c + exp(phi1) * exp(phi2) / 
                          ( exp(phi3)*(exp(phi2)-exp(phi1)) ) * 
                          ( exp(-exp(phi1)*day) - exp(-exp(phi2)*day) ),
  start = c(c=1000, phi1=log(0.3), phi2=log(0.1), phi3=log(0.0005)),
  data=pixel.df, control=list(minFactor=1e-10, warnOnly=TRUE))
d <- seq(0, 22, 0.1)
plot(d, predict(pixel.nls.0, data.frame(day=d)), type='l')

# nlme doesn't converge normally
pixel.nlme.0 <- nlme(
  pixel ~ c + exp(phi1) * exp(phi2) / 
              ( exp(phi3)*(exp(phi2)-exp(phi1)) ) * 
              ( exp(-exp(phi1)*day) - exp(-exp(phi2)*day) ),
  fixed = c + phi1 + phi2 + phi3 ~ 1,
  random = c ~ 1 | Dog/Side,
  start = coef(pixel.nls.0),
  data=pixel.df, control=list(returnObject=TRUE))
ranef(pixel.nlme.0) # all 0, even if their estimated sd's are > 0

par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(pixel.df$day, pixel.df$pixel, ylim=c(1020, max(pixel.df$pixel)), type='n', xlab='Time post injection (days)', ylab='Pixel intensity')
for (d in 1:max(as.numeric(pixel.df$Dog))) {
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.nlme.0)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.nlme.0)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], as.character(d), cex=0.8)
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.nlme.0)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], lty=2, type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.nlme.0)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], as.character(d), cex=0.8)
}
legend('topright', lty=c(1,2), c('left', 'right'))
x <- seq(min(pixel.df$day), max(pixel.df$day), length=100)
y <- predict(pixel.nlme.0, data.frame(Dog=rep(1, length(x)), Side=rep('L', length(x)), day=x), level=0)
lines(x, y, lwd=3, lty='11')



# combination of a constant and two SSlogis models - nlme doesn't 
# converge normally...

pixel.nls.1 <- nls(pixel ~ c + Asym/(1 + exp(-(day-xmid1)/scal1)) 
                             - Asym/(1 + exp(-(day-xmid2)/scal2)),
  start = c(c=1000, Asym=100, xmid1=4, scal1=2, xmid2=17, scal2=5),
  data=pixel.df)

d <- seq(0, 22, 0.1)
plot(d, predict(pixel.nls.1, data.frame(day=d)), type='l')

pixel.nlme.1 <- nlme(pixel ~ c + Asym/(1 + exp(-(day-xmid1)/scal1)) 
                               - Asym/(1 + exp(-(day-xmid2)/scal2)),
  fixed = c + Asym + xmid1 + scal1 + xmid2 + scal2 ~ 1,
  random = c ~ 1 | Dog/Side,
  start = coef(pixel.nls.1),
  data=pixel.df, control=list(returnObject=TRUE))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/pixel.nlme.1.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(pixel.df$day, pixel.df$pixel, ylim=c(1020, max(pixel.df$pixel)), type='n', xlab='Time post injection (days)', ylab='Pixel intensity')
for (d in 1:max(as.numeric(pixel.df$Dog))) {
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.nlme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.nlme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], as.character(d), cex=0.8)
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.nlme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], lty=2, type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.nlme.1)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], as.character(d), cex=0.8)
}
legend('topright', lty=c(1,2), c('left', 'right'))
x <- seq(min(pixel.df$day), max(pixel.df$day), length=100)
y <- predict(pixel.nlme.1, data.frame(Dog=rep(1, length(x)), Side=rep('L', length(x)), day=x), level=0)
lines(x, y, lwd=3, lty='11')
dev.off()
}

# the fitted curve (fixed effects only) does not really have the expected shape:
x <- seq(-30, 30, length=200)
y <- predict(pixel.nlme.1, data.frame(Dog=rep(1, length(x)), Side=rep('L', length(x)), day=x), level=0)
plot(x, y, type='l')
abline(v=0, h=pixel.nlme.1$coefficients$fixed['c'], lty='dotted')

# adding a random effect for Asym at the Dog level ==> singularity...
pixel.nlme.2 <- nlme(pixel ~ c + Asym/(1 + exp(-(day-xmid1)/scal1)) 
                               - Asym/(1 + exp(-(day-xmid2)/scal2)),
  fixed = c + Asym + xmid1 + scal1 + xmid2 + scal2 ~ 1,
  random = list(Dog = c + Asym ~ 1, Side = c ~ 1),
  start = coef(pixel.nls.1),
  data=pixel.df, control=list(msMaxIter=1000, returnObject=TRUE))

pixel.nlme.2 <- update(pixel.nlme.1, 
  random = list(Dog = c + Asym ~ 1, Side = c ~ 1), 
  control=list(msMaxIter=1000))


# try independent random effects - 
# terminates with warnings, resulting model is obviously inappropriate...
pixel.nlme.3 <- update(pixel.nlme.1, 
  random = list(Dog = pdDiag(c + Asym ~ 1), Side = c ~ 1))
  # (control=list(minFactor=1e-10) avoids one of the warnings, 
  # but does not improve the fit)

par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(pixel.df$day, pixel.df$pixel, ylim=c(1020, max(pixel.df$pixel)), type='n', xlab='Time post injection (days)', ylab='Pixel intensity')
for (d in 1:max(as.numeric(pixel.df$Dog))) {
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.nlme.3)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], fitted.values(pixel.nlme.3)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='L'], as.character(d), cex=0.8)
  lines(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.nlme.3)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], lty=2, type='b', pch='')
  text(pixel.df$day[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], fitted.values(pixel.nlme.3)[as.numeric(pixel.df$Dog)==d & pixel.df$Side=='R'], as.character(d), cex=0.8)
}
legend('topright', lty=c(1,2), c('left', 'right'))
x <- seq(min(pixel.df$day), max(pixel.df$day), length=100)
y <- predict(pixel.nlme.3, data.frame(Dog=rep(1, length(x)), Side=rep('L', length(x)), day=x), level=0)
lines(x, y, lwd=3, lty='11')
