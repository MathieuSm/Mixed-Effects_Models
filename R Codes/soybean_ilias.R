library(nlme)

soybean.df <- data.frame(Soybean)
soybean.df$Plot <- factor(as.character(soybean.df$Plot), ordered = FALSE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/soybean.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(soybean.df$Time, soybean.df$weight, xlim=c(0, 84), type='n', xlab='Time since planting', ylab='Weight')
ltys <- c('solid', '33', '11')
for (p in rev(unique(soybean.df$Plot))) {
  with(soybean.df[soybean.df$Plot==p,], 
    lines(Time, weight, pch=tolower(as.character(Variety)), type='b', cex=0.6, lwd=1, col=grey((as.numeric(Year)-1)/4), lty=ltys[4-as.numeric(Year)])
  )
}
legend('topleft', lty=ltys[3:1], col=grey((0:2)/4), legend=levels(soybean.df$Year))
dev.off()
} # end if (FALSE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/soybean_col.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(soybean.df$Time, soybean.df$weight, xlim=c(0, 84), type='n', xlab='Time since planting', ylab='Weight')
cols <- c('darkgreen', 'blue', 'darkorange')# rainbow(3, v=0.8, start=2/6, end=5/6)
for (p in unique(soybean.df$Plot)) {
  with(soybean.df[soybean.df$Plot==p,], 
    lines(Time, weight, pch=tolower(as.character(Variety)), type='b', cex=0.6, lwd=1, col=cols[as.numeric(Year)])
  )
}
legend('topleft', lty='solid', col=cols, legend=levels(soybean.df$Year))
dev.off()
} # end if (FALSE)


nlsList(weight ~ SSlogis(Time, Asym, xmid, scal) | Plot, data=soybean.df)
# doesn't converge for Plots 1989P5, 1989P8
with(soybean.df[soybean.df$Plot=='1989P5',], plot(Time, weight, type='l'))
with(soybean.df[soybean.df$Plot=='1989P8',], plot(Time, weight, type='l'))


# explore the influence of Variety and Year using nlsList
plot(coef(nlsList(weight ~ SSlogis(Time, Asym, xmid, scal) | Variety/Year, data=soybean.df)))


soybean.nls.1 <- nls(weight ~ SSlogis(Time, Asym, xmid, scal), 
  data=soybean.df)

soybean.nlme.1 <- nlme(weight ~ SSlogis(Time, Asym, xmid, scal), 
  fixed = Asym + xmid + scal ~ 1,
  random = Asym + xmid + scal ~ 1 | Plot,
  start = coef(soybean.nls.1),
  data=soybean.df)

plot(augPred(soybean.nlme.1, primary = ~ Time))

plot(ranef(soybean.nlme.1, augFrame=TRUE), form = ~ Year * Variety)


soybean.nlme.1a <- nlme(weight ~ SSlogis(Time, Asym, xmid, scal), 
  fixed = list(Asym ~ Variety * Year, xmid + scal ~ 1),
  random = Asym + xmid + scal ~ 1 | Plot,
  start = c(coef(soybean.nls.1)[1], 0, 0, 0, 0, 0, coef(soybean.nls.1)[2:3]),
  data=soybean.df, control = list(msMaxIter=1000, eval.max=1000))

# conditional t/F tests
summary(soybean.nlme.1a)
anova(soybean.nlme.1a)
anova(soybean.nlme.1a, type='marginal')

# LRT - not recommended
anova(soybean.nlme.1, soybean.nlme.1a)

plot(augPred(soybean.nlme.1a, primary = ~ Time))

t <- seq(14, 84, 0.1)
newdata <- soybean.df[c(),-5]
newdata <- rbind(newdata, data.frame(Plot = rep(soybean.df$Plot[1], 6*length(t)), Variety = rep(c('F', 'P'), each=3*length(t)), Year = as.factor(rep(1988:1990, each=length(t), 2)), Time=t))
#newdata$Plot <- paste(newdata$Year, newdata$Variety, '1', sep='')
newdata$weight <- predict(soybean.nlme.1a, newdata)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/soybean.nlme.1a.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(soybean.df$Time, soybean.df$weight, xlim=c(0, 90), type='n', xlab='Time since planting', ylab='Weight')
ltys <- c('solid', '33', '11')
for (y in rev(unique(newdata$Year))) {
  for (v in unique(newdata$Variety)) {
    with(newdata[(newdata$Variety==v) & (newdata$Year==y),], {
      lines(Time, weight, cex=0.6, lwd=2, col=grey((as.numeric(Year)-1)/4), lty=ltys[4-as.numeric(Year)])
      text(85, rev(weight)[1] + 0.1 - 0.5*((Variety=='F') & (Year==1990)) + 0.7*((Variety=='P') & (Year==1989)), paste(Year, Variety, sep=''), adj=0, cex=0.8)
    })
  }
}
dev.off()
} # end if (FALSE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/soybean.nlme.1a_col.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(soybean.df$Time, soybean.df$weight, xlim=c(0, 90), type='n', xlab='Time since planting', ylab='Weight')
cols <- c('darkgreen', 'blue', 'darkorange')# rainbow(3, v=0.8, start=2/6, end=5/6)
for (v in unique(newdata$Variety)) {
  for (y in unique(newdata$Year)) {
    with(newdata[(newdata$Variety==v) & (newdata$Year==y),], {
      lines(Time, weight, cex=0.6, lwd=2, col=cols[as.numeric(Year)])
      text(85, rev(weight)[1] + 0.1 - 0.5*((Variety=='F') & (Year==1990)) + 0.7*((Variety=='P') & (Year==1989)), paste(Year, Variety, sep=''), adj=0, cex=0.8, col=cols[as.numeric(Year)])
    })
  }
}
dev.off()
} # end if (FALSE)


plot(soybean.nlme.1a, grid=FALSE, lty='dotted')

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/skript_folien/soybean.nlme.1a_resid.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(fitted(soybean.nlme.1a), resid(soybean.nlme.1a, type='pearson'), 
  ylim=c(-1, 1)*max(abs(resid(soybean.nlme.1a, type='pearson'))), 
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')
dev.off()
} # end if (FALSE)


soybean.nlme.1b <- nlme(weight ~ SSlogis(Time, Asym, xmid, scal), 
  fixed = list(Asym ~ Variety * Year, xmid + scal ~ 1),
  random = Asym + xmid + scal ~ 1 | Plot,
  start = c(coef(soybean.nls.1)[1], 0, 0, 0, 0, 0, coef(soybean.nls.1)[2:3]),
  weights = varPower(form = ~ fitted(.)),
  data=soybean.df, control = list(msMaxIter=1000, eval.max=1000))
anova(soybean.nlme.1a, soybean.nlme.1b)

plot(soybean.nlme.1b, grid=FALSE, lty='dotted')

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/skript_folien/soybean.nlme.1b_resid.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(fitted(soybean.nlme.1b), resid(soybean.nlme.1b, type='pearson'), 
  ylim=c(-1, 1)*max(abs(resid(soybean.nlme.1b, type='pearson'))), 
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')
dev.off()
} # end if (FALSE)




####################
# diagnostic plots #
####################

# resid ~ fitted, zoomed
if (FALSE) {
pdf(file = "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_resid_zoomed.pdf", 
  width=7, height=5, family="Times")
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(fitted(soybean.nlme.1b), resid(soybean.nlme.1b, type='pearson'), 
  xlim=c(0, 1), ylim=c(-1, 1)*max(abs(resid(soybean.nlme.1b, type='pearson'))), 
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')
dev.off()
} # end if (FALSE)


# resid ~ fitted, by Year * Variety
plot(soybean.nlme.1b, resid(., type='pearson') ~ fitted(.) | Year*Variety, grid=FALSE, abline=0, lty='dotted')


# resid ~ Year * Variety
# plot(soybean.nlme.1b, paste(Variety, Year) ~ resid(., type='pearson'))
if (FALSE) {
pdf(file = "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_boxplot_resid_by_year_variety.pdf",
  width=7, height=5, family="Times")
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
boxplot(resid(soybean.nlme.1b, type='pearson') ~ paste(soybean.df$Year, soybean.df$Variety, sep=''), 
  ylim=c(-1,1)*max(abs(resid(soybean.nlme.1b, type='pearson'))), 
  xlab='Year * Variety', ylab='Standardized residual', outcex=0.7, col=NULL)
abline(h=0, lty='dotted')
dev.off()
} # end if (FALSE)


# resid ~ Plot
if (FALSE) {
pdf(file = "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_boxplot_resid_by_plot.pdf",
  width=7, height=5, family="Times")
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
b <- boxplot(resid(soybean.nlme.1b, type='pearson') ~ soybean.df$Plot, 
  ylim=c(-1,1)*max(abs(resid(soybean.nlme.1b, type='pearson'))), xlab='Plot', 
  ylab='Standardized residual', axes=FALSE, frame=TRUE, col='grey', outcex=0.7, xlim=c(2, 47))
#axis(1, at=1:length(b$names), labels=b$names, las=3, cex.axis=0.7)
#axis(1, at=1:length(b$names), labels=substr(b$names, 6, 6), las=3, cex.axis=0.7)
axis(1, at=2*(1:(length(b$names)/2))-1, labels=substr(b$names, 6, 6)[2*(1:(length(b$names)/2))-1], cex.axis=0.7, mgp=c(1.8, 0, 0), tick=FALSE)
axis(1, at=2*(1:(length(b$names)/2)), labels=substr(b$names, 6, 6)[2*(1:(length(b$names)/2))], cex.axis=0.7, mgp=c(1.8, 0, 0), tick=FALSE)
axis(1, at=(1:6)*8 - 3.5, labels=paste(substr(b$names[(1:6)*8], 1, 5), ' .', sep=''), cex.axis=0.7, tick=FALSE)
axis(2)
abline(h=0, v=(1:5)*8 + 0.5, lty='dotted')
dev.off()
} # end if (FALSE)

depth.plot <- function(x, y, lwd.ratio=8, lwd=1, ...) {
  plot(x, y, ...)
  for (x0 in unique(x)) {
    y0 <- sort(y[x==x0])
    n0 <- length(y0)
    if (n0 > 1) {
      for (i in 1:floor(n0/2)) {
        lines(rep(x0, 2), y0[c(i, n0+1-i)], 
              lwd = lwd+(i-1)/(floor(n0/2)-1) * (lwd.ratio-1)*lwd)
      }
    }
  }
}

if (FALSE) {
pdf(file = "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_depthplot_resid_by_plot.pdf",
  width=7, height=5, family="Times")
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
depth.plot(as.numeric(soybean.df$Plot), resid(soybean.nlme.1b, type='pearson'), 
  ylim=c(-1,1)*max(abs(resid(soybean.nlme.1b, type='pearson'))), xlab='Plot', 
  ylab='Standardized residual', axes=FALSE, frame=TRUE, xlim=c(2, 47), pch=18, lwd.ratio=6)
#axis(1, at=1:length(b$names), labels=substr(b$names, 6, 6), las=3, cex.axis=0.7)
axis(1, at=2*(1:(length(b$names)/2))-1, labels=substr(b$names, 6, 6)[2*(1:(length(b$names)/2))-1], cex.axis=0.7, mgp=c(1.8, 0, 0), tick=FALSE)
axis(1, at=2*(1:(length(b$names)/2)), labels=substr(b$names, 6, 6)[2*(1:(length(b$names)/2))], cex.axis=0.7, mgp=c(1.8, 0, 0), tick=FALSE)
axis(1, at=(1:6)*8 - 3.5, labels=paste(substr(b$names[(1:6)*8], 1, 5), ' .', sep=''), cex.axis=0.7, tick=FALSE)
axis(2)
abline(h=0, v=(1:5)*8 + 0.5, lty='dotted')
dev.off()
} # end if (FALSE)


# qqnorm(resid)
if (FALSE) {
pdf(file = "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_qqnorm_resid.pdf", 
  width=7, height=5, family="Times")
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
qq <- qqnorm(resid(soybean.nlme.1b, type='pearson'), main='')
text(qq$x[abs(qq$y)>2.5], qq$y[abs(qq$y)>2.5], names(qq$y)[abs(qq$y)>2.5], cex=0.5, pos=c(3, 2, 4, 3, 2, 1, 2, 4, 4))
dev.off()
} # end if (FALSE)


# ranef ~ Year * Variety
re <- ranef(soybean.nlme.1b)
boxplot(re$'Asym.(Intercept)' ~ substr(rownames(re), 1, 5))
abline(h=0, lty='dotted')
boxplot(re$xmid ~ substr(rownames(re), 1, 5))
abline(h=0, lty='dotted')
boxplot(re$scal ~ substr(rownames(re), 1, 5))
abline(h=0, lty='dotted')

# roughly the same using augFrame=TRUE:
re <- ranef(soybean.nlme.1b, augFrame=TRUE)
boxplot(re$'Asym.(Intercept)' ~ re$Variety * re$Year)
abline(h=0, lty='dotted')
boxplot(re$xmid ~ re$Variety * re$Year)
abline(h=0, lty='dotted')
boxplot(re$scal ~ re$Variety * re$Year)
abline(h=0, lty='dotted')

plot(ranef(soybean.nlme.1b, augFrame=TRUE), form = ~ Year * Variety)

if (FALSE) {
pdf(file = "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_ranef.pdf", 
  width=7, height=5, family="Times", colormodel="grey")
plot(ranef(soybean.nlme.1b, augFrame=TRUE), form = ~ Year * Variety, 
  scales=list(x=list(relation='free', at=list(-2:2, 2*(-2:2), c(-0.4, 0, 0.4)))), 
  xlim=list(1.4*c(-1, 1)*max(abs(re[,1])), 1.4*c(-1, 1)*max(abs(re[,2])), 1.4*c(-1, 1)*max(abs(re[,3]))), 
#  xlim=list(c(-2.5, 2.5), c(-4.5, 4.5), c(-0.5, 0.5)), 
  par.settings=list(axis.text=list(cex=1.2), par.xlab.text=list(cex=1.2), par.ylab.text=list(cex=1.2), add.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)


# qqnorm(ranef)
qqnorm(soybean.nlme.1b, form = ~ranef(.))

library(lattice)
if (FALSE) {
pdf(file = "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_qqnorm_ranef.pdf", 
  width=7, height=5, family="Times", colormodel="grey")
re <- ranef(soybean.nlme.1b)
xyplot(as.numeric(apply(re, 2, function(y) {qqnorm(y, plot.it=FALSE)$x})) ~ as.numeric(as.matrix(re)) | factor(rep(names(re), each=nrow(re)), levels=names(re)),
  scales=list(x=list(relation='free', at=list(-2:2, 2*(-2:2), c(-0.4, 0, 0.4)))), 
  xlab='Random effects', ylab='Quantiles of standard normal',
  xlim=list(1.4*c(-1, 1)*max(abs(re[,1])), 1.4*c(-1, 1)*max(abs(re[,2])), 1.4*c(-1, 1)*max(abs(re[,3]))), 
#  xlim=list(c(-2.5, 2.5), c(-4.5, 4.5), c(-0.5, 0.5)), 
  par.settings=list(axis.text=list(cex=1.2), par.xlab.text=list(cex=1.2), par.ylab.text=list(cex=1.2), add.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)


# covariance structure of the random effects
re <- ranef(soybean.nlme.1b, augFrame=TRUE)
pairs(re[,1:3], col=re$Year)

# roughly the same (with ellipses added when uncommented)
if (FALSE) {
color <- TRUE
pdf(file = ifelse(color, "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_cor_ranef_col.pdf", "U:/vock/lehre/mixed_models/skript_folien/soybean_nlme_1b_cor_ranef.pdf"),
  width=7, height=5, family="Times")
par(mfrow=c(2,2), oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
t <- seq(0, 2*pi, 0.01)
circle.mat <- cbind(cos(t), sin(t))
#pchs <- if (color) {rep(16, 3)} else {c(18, 16, 17)}
pchs <- if (color) {rep(1, 3)} else {c(5, 1, 2)}
cols <- if (color) {c('darkgreen', 'blue', 'darkorange')} else {grey(((1:3)-1)/4)}
ltys <- if (color) {rep('solid', 3)} else {c('solid', '33', '11')}
plot(re$xmid, re$Asym, col=cols[as.numeric(re$Year)], pch=pchs[as.numeric(re$Year)], xlab='xmid', ylab='Asym.(Intercept)', axes=FALSE)
axis(1)
axis(2, at=seq(-1.5, 1, 0.5), labels=rep('', 6))
axis(2, at=-1:1, tick=FALSE)
box()
#for (i in length(unique(re$Year)):1) {
#  y <- unique(re$Year)[i]
#  lines(matrix(apply(re[re$Year==y,1:3], 2, mean)[c(2,1)], ncol=2, nrow=length(t), byrow=TRUE) + 
##    circle.mat %*% chol(diag(sd(re[re$Year==y,1:3])[c(2,1)]) %*% cor(re[re$Year==y,1:3])[c(2,1), c(2,1)] %*% diag(sd(re[re$Year==y,1:3])[c(2,1)])), 
#    circle.mat %*% chol(diag(sapply(re[re$Year==y,1:3], sd)[c(2,1)]) %*% cor(re[re$Year==y,1:3])[c(2,1), c(2,1)] %*% diag(sapply(re[re$Year==y,1:3], sd)[c(2,1)])), 
#    col=cols[i], lty=ltys[i], lwd=2)
#}
plot(re$scal, re$Asym, col=cols[as.numeric(re$Year)], pch=pchs[as.numeric(re$Year)], xlab='scal', ylab='Asym.(Intercept)', axes=FALSE)
axis(1, at=seq(-0.3, 0.1, 0.2))
axis(1, at=seq(-0.2, 0.2, 0.2))
axis(2, at=seq(-1.5, 1, 0.5), labels=rep('', 6))
axis(2, at=-1:1, tick=FALSE)
box()
#for (i in length(unique(re$Year)):1) {
#  y <- unique(re$Year)[i]
#  lines(matrix(apply(re[re$Year==y,1:3], 2, mean)[c(3,1)], ncol=2, nrow=length(t), byrow=TRUE) + 
##    circle.mat %*% chol(diag(sd(re[re$Year==y,1:3])[c(3,1)]) %*% cor(re[re$Year==y,1:3])[c(3,1), c(3,1)] %*% diag(sd(re[re$Year==y,1:3])[c(3,1)])), 
#    circle.mat %*% chol(diag(sapply(re[re$Year==y,1:3], sd)[c(3,1)]) %*% cor(re[re$Year==y,1:3])[c(2,1), c(2,1)] %*% diag(sapply(re[re$Year==y,1:3], sd)[c(3,1)])), 
#    col=cols[i], lty=ltys[i], lwd=2)
#}
plot(0, 0, type='n', xlab='', ylab='', axes=FALSE)
if (color) {
  legend('bottomleft', pch=pchs, col=cols, legend=unique(re$Year))
} else {
#  legend('bottomleft', pch=pchs, col=cols, lwd=2, lty=ltys, legend=unique(re$Year), merge=FALSE)
  legend('bottomleft', pch=pchs, col=cols, legend=unique(re$Year))
}
plot(re$scal, re$xmid, col=cols[as.numeric(re$Year)], pch=pchs[as.numeric(re$Year)], xlab='scal', ylab='xmid', axes=FALSE)
axis(1, at=seq(-0.3, 0.1, 0.2))
axis(1, at=seq(-0.2, 0.2, 0.2))
axis(2)
box()
#for (i in length(unique(re$Year)):1) {
#  y <- unique(re$Year)[i]
#  lines(matrix(apply(re[re$Year==y,1:3], 2, mean)[c(3,2)], ncol=2, nrow=length(t), byrow=TRUE) + 
##    circle.mat %*% chol(diag(sd(re[re$Year==y,1:3])[c(3,2)]) %*% cor(re[re$Year==y,1:3])[c(3,2), c(3,2)] %*% diag(sd(re[re$Year==y,1:3])[c(3,2)])), 
#    circle.mat %*% chol(diag(sapply(re[re$Year==y,1:3], sd)[c(3,2)]) %*% cor(re[re$Year==y,1:3])[c(2,1), c(2,1)] %*% diag(sapply(re[re$Year==y,1:3], sd)[c(3,2)])), 
#    col=cols[i], lty=ltys[i], lwd=2)
#}
dev.off()
} # end if (FALSE)


soybean.nlme.1b



# predicted curves for each plot
cols <- c('darkgreen', 'blue', 'darkorange')
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(soybean.df$Time, soybean.df$weight, xlim=c(0, 90), ylim=c(0, 21), type='n', xlab='Time', ylab='weight')
Times <- 14:84
for (p in unique(soybean.df$Plot)) {
  lines(Times, predict(soybean.nlme.1b, 
    data.frame(Plot=factor(rep(p, length(Times)), levels=levels(soybean.df$Plot)), 
      Variety=factor(rep(substring(p, 5, 5), length(Times)), levels=levels(soybean.df$Variety)), 
      Year=factor(rep(substring(p, 1, 4), length(Times)), levels=c('1988', '1989', '1990')), Time=Times) 
  ), col=cols[substring(p, 1, 4)==1988:1990], lty=c('dashed', 'dotted')[substring(p, 5, 5)==c('F', 'P')])
}
legend('topleft', lty=c('solid', 'solid', 'solid', 'blank', 'dashed', 'dotted'),
  col=c(cols, 'white', 'black', 'black'), c('1988', '1989', '1990', '', 'F', 'P'))




###########################
# try to fix the problems #
###########################


# omit unnecessary random effects

# try to drop one of the three random effects
anova(soybean.nlme.1b, update(soybean.nlme.1b, random = xmid + scal ~ 1 | Plot))
anova(soybean.nlme.1b, update(soybean.nlme.1b, random = Asym + scal ~ 1 | Plot))
anova(soybean.nlme.1b, update(soybean.nlme.1b, random = Asym + xmid ~ 1 | Plot))
# ==> drop the one for scal first, then try to drop another one
soybean.nlme.1c <- update(soybean.nlme.1b, random = Asym + xmid ~ 1 | Plot)
anova(soybean.nlme.1c, update(soybean.nlme.1c, random = xmid ~ 1 | Plot))
anova(soybean.nlme.1c, update(soybean.nlme.1c, random = Asym ~ 1 | Plot))
# ==> also drop the one for Asym, 
# only keep a random intercept for xmid
soybean.nlme.1d <- update(soybean.nlme.1c, random = xmid ~ 1 | Plot)

plot(ranef(soybean.nlme.1d, augFrame=TRUE), form = ~ Year * Variety)

plot(soybean.nlme.1d, resid(., type='pearson') ~ fitted(.), grid=FALSE, abline=0, lty='dotted')

boxplot(resid(soybean.nlme.1d, type='pearson') ~ paste(soybean.df$Year, soybean.df$Variety, sep=''), 
  ylim=c(-1,1)*max(abs(resid(soybean.nlme.1d, type='pearson'))), 
  xlab='Year * Variety', ylab='Standardized residual', outcex=0.7)
abline(h=0, lty='dotted')

qqnorm(resid(soybean.nlme.1d, type='pearson'), main='')


# include fixed effects of Year for xmid and scal
soybean.nlme.1e <- nlme(weight ~ SSlogis(Time, Asym, xmid, scal), 
  fixed = list(Asym ~ Variety * Year, xmid + scal ~ Year),
  random = xmid ~ 1 | Plot,
  start = c(coef(soybean.nls.1)[1], 0, 0, 0, 0, 0, 
            coef(soybean.nls.1)[2], 0, 0, coef(soybean.nls.1)[3], 0, 0),
  weights = varPower(form = ~ fitted(.)),
  data=soybean.df)
anova(soybean.nlme.1e, type='marginal')
# ==> both newly added Year effects are significant

plot(ranef(soybean.nlme.1e, augFrame=TRUE), form = ~ Year * Variety)

plot(soybean.nlme.1e, resid(., type='pearson') ~ fitted(.), grid=FALSE, abline=0, lty='dotted')

boxplot(resid(soybean.nlme.1e, type='pearson') ~ paste(soybean.df$Year, soybean.df$Variety, sep=''), 
  ylim=c(-1,1)*max(abs(resid(soybean.nlme.1e, type='pearson'))), 
  xlab='Year * Variety', ylab='Standardized residual', outcex=0.7)
abline(h=0, lty='dotted')

qqnorm(resid(soybean.nlme.1e, type='pearson'), main='')


# different variances per year
soybean.nlme.1f <- nlme(weight ~ SSlogis(Time, Asym, xmid, scal), 
  fixed = list(Asym ~ Variety * Year, xmid + scal ~ Year),
  random = xmid ~ 1 | Plot,
  start = c(coef(soybean.nls.1)[1], 0, 0, 0, 0, 0, 
            coef(soybean.nls.1)[2], 0, 0, coef(soybean.nls.1)[3], 0, 0),
  weights = varComb(varPower(form = ~ fitted(.)), varIdent(form = ~ 1 | Year)),
  data=soybean.df)

anova(soybean.nlme.1e, soybean.nlme.1f)


# diagnostics for this model

plot(ranef(soybean.nlme.1f, augFrame=TRUE), form = ~ Year * Variety)

plot(soybean.nlme.1f, resid(., type='pearson') ~ fitted(.), grid=FALSE, abline=0, lty='dotted')
plot(fitted(soybean.nlme.1f), resid(soybean.nlme.1f, type='pearson'), 
  xlim=c(0, 1), ylim=c(-1, 1)*max(abs(resid(soybean.nlme.1f, type='pearson'))), 
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')

boxplot(resid(soybean.nlme.1f, type='pearson') ~ paste(soybean.df$Year, soybean.df$Variety, sep=''), 
  ylim=c(-1,1)*max(abs(resid(soybean.nlme.1f, type='pearson'))), 
  xlab='Year * Variety', ylab='Standardized residual', outcex=0.7)
abline(h=0, lty='dotted')

qqnorm(resid(soybean.nlme.1f, type='pearson'), main='')

qq <- qqnorm(ranef(soybean.nlme.1f)$xmid)
text(qq$x[abs(qq$y)>1], qq$y[abs(qq$y)>1], dimnames(ranef(soybean.nlme.1f))[[1]][abs(qq$y)>1], pos=c(2, 4))



# try a four-parameter logistic growth curve

soybean.nlme.1g <- nlme(weight ~ SSfpl(Time, A, Asym, xmid, scal), 
  fixed = list(Asym ~ Variety * Year, A + xmid + scal ~ Year),
  random = xmid ~ 1 | Plot,
  start = c(coef(soybean.nls.1)[1], 0, 0, 0, 0, 0, 
            0, 0, 0, coef(soybean.nls.1)[2], 0, 0, coef(soybean.nls.1)[3], 0, 0),
  weights = varComb(varPower(form = ~ fitted(.)), varIdent(form = ~ 1 | Year)),
  data=soybean.df)

anova(soybean.nlme.1f, soybean.nlme.1g)

plot(ranef(soybean.nlme.1g, augFrame=TRUE), form = ~ Year * Variety)

plot(soybean.nlme.1g, resid(., type='pearson') ~ fitted(.), grid=FALSE, abline=0, lty='dotted')
plot(fitted(soybean.nlme.1g), resid(soybean.nlme.1g, type='pearson'), 
  xlim=c(0, 1), ylim=c(-1, 1)*max(abs(resid(soybean.nlme.1g, type='pearson'))), 
  xlab='Fitted values', ylab='Standardized residuals')
abline(h=0, lty='dotted')

boxplot(resid(soybean.nlme.1g, type='pearson') ~ paste(soybean.df$Year, soybean.df$Variety, sep=''), 
  ylim=c(-1,1)*max(abs(resid(soybean.nlme.1g, type='pearson'))), 
  xlab='Year * Variety', ylab='Standardized residual', outcex=0.7)
abline(h=0, lty='dotted')

qqnorm(resid(soybean.nlme.1g, type='pearson'), main='')

qq <- qqnorm(ranef(soybean.nlme.1g)$xmid)
text(qq$x[abs(qq$y)>0.3], qq$y[abs(qq$y)>0.3], dimnames(ranef(soybean.nlme.1g))[[1]][abs(qq$y)>0.3], pos=c(4, 2, 4, 3, 2))
