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

