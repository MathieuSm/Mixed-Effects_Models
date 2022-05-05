library(nlme)

orange.df <- data.frame(Orange)
orange.df$Tree <- factor(as.character(orange.df$Tree), ordered = FALSE)


if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/orange.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(orange.df$age, orange.df$circumference, xlim=c(0, 1600), ylim=c(0, 220), type='n', xlab='age (days since December 31, 1968)', ylab='circumference')
for (t in unique(orange.df$Tree)) {
  with(orange.df[orange.df==t,], 
    lines(age, circumference, pch=t, type='b', cex=0.8, lwd=2)
  )
}
text(c(0, 365, 730, 1095, 1381)+182.5, 0, 1969:1973)
abline(v=c(0, 365, 730, 1095, 1461), lty='dotted')
dev.off()
} # end if (FALSE)


# single nls fit

orange.nls.1 <- nls(circumference ~ Asym/(1 + exp(-(age-xmid)/scal)), 
  start = c(Asym=170, xmid=700, scal=500), data=orange.df)
orange.nls.1

# or:
nls(circumference ~ SSlogis(age, Asym, xmid, scal), data=orange.df)

plot(orange.df$age, orange.df$circumference, xlim=c(0, 1600), ylim=c(0, 220), xlab='age (days since December 31, 1968)', ylab='circumference')
lines(1:1600, predict(orange.nls.1, data.frame(age=1:1600)))

# variance of the residuals increases over time:
plot(orange.df$age, residuals(orange.nls.1))
abline(h=0, lty='dotted')


orange.nlsList.1 <- nlsList(circumference ~ SSlogis(age, Asym, xmid, scal) | Tree, 
  data=orange.df)
orange.nlsList.1


if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/orange.nlsList.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(orange.df$age, orange.df$circumference, xlim=c(0, 1600), ylim=c(0, 220), type='n', xlab='age (days since December 31, 1968)', ylab='circumference')
ages <- 118:1582
for (t in unique(orange.df$Tree)) {
  lines(ages, predict(orange.nlsList.1[[as.numeric(t)]], data.frame(age=ages)), lwd=2)
  text(1600, predict(orange.nlsList.1[[as.numeric(t)]], data.frame(age=1600)) + 2.5*(t=='1') - 2.5*(t=='3'), t, cex=0.8)
}
lines(ages, predict(orange.nls.1, data.frame(age=ages)), lwd=4, lty='22')
text(c(0, 365, 730, 1095, 1381)+182.5, 0, 1969:1973)
abline(v=c(0, 365, 730, 1095, 1461), lty='dotted')
dev.off()
} # end if (FALSE)


plot(intervals(orange.nlsList.1))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/orange.nlsList.1.intervals.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
plot(intervals(orange.nlsList.1), par.settings=list(axis.text=list(cex=1.2), par.ylab.text=list(cex=1.2), add.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)



orange.nlme.1 <- nlme(circumference ~ SSlogis(age, Asym, xmid, scal),
  fixed = Asym + xmid + scal ~ 1, random = Asym + xmid + scal ~ 1 | Tree,
  start = c(170, 700, 500), data=orange.df)
# Warning message:
#   Iteration 1, LME step: nlminb() did not converge (code = 1). Do increase 'msMaxIter'!

orange.nlme.1 <- nlme(circumference ~ SSlogis(age, Asym, xmid, scal),
  fixed = Asym + xmid + scal ~ 1, random = Asym + xmid + scal ~ 1 | Tree,
  start = c(170, 700, 500), data=orange.df, 
  control = list(msMaxIter=1000))
# Warning message:
#   Iteration 1, LME step: nlminb() did not converge (code = 1). PORT message: function evaluation limit reached without convergence (9)

orange.nlme.1 <- nlme(circumference ~ SSlogis(age, Asym, xmid, scal),
  fixed = Asym + xmid + scal ~ 1, random = Asym + xmid + scal ~ 1 | Tree,
  start = c(170, 700, 500), data=orange.df, 
  control = list(msMaxIter=1000, eval.max=1000))

# or (starting from the nlsList fit ==> different fit...)
nlme(orange.nlsList.1, random = Asym + xmid + scal ~ 1 | Tree, 
  control = list(msMaxIter=1000, eval.max=1000))
nlme(orange.nlsList.1,  # random can be omitted
  control = list(msMaxIter=1000, eval.max=1000))

# or (using the groups parameter and the default random effects structure)
nlme(circumference ~ SSlogis(age, Asym, xmid, scal),
  fixed = Asym + xmid + scal ~ 1, groups = ~ Tree,
  start = c(170, 700, 500), data=orange.df, 
  control = list(msMaxIter=1000, eval.max=1000))

# start still cannot be omitted in version 3.1-148:
nlme(circumference ~ SSlogis(age, Asym, xmid, scal),
  fixed = Asym + xmid + scal ~ 1, random = Asym + xmid + scal ~ 1 | Tree,
  data=orange.df)
# ==> Error in nlsList.formula(model = circumference ~ SSlogis(age, Asym, xmid,  : 
#       'data' must be a "groupedData" object if 'formula' does not include groups

# using groupedData object Orange
nlme(circumference ~ SSlogis(age, Asym, xmid, scal),
  fixed = Asym + xmid + scal ~ 1, data=Orange, 
  control = list(msMaxIter=1000, eval.max=1000))  # start can be omitted

orange.nlme.1


vc <- VarCorr(orange.nlme.1)
corr <- matrix(as.numeric(vc[-c(1, nrow(vc)),3:ncol(vc)]), nrow=nrow(vc)-2, ncol=nrow(vc)-2)
corr <- rbind(rep(0, ncol(corr)+1), cbind(corr, rep(0, nrow(corr))))
corr <- corr + t(corr)
diag(corr) <- 1
sdev <- as.numeric(vc[-nrow(vc),'StdDev'])
Psi <- outer(sdev, sdev) * corr

sigma <- as.numeric(vc[nrow(vc), 'StdDev'])
sigma^2

ranef(orange.nlme.1)

coef(orange.nlme.1)[1,]  # this is the same as phi-hat_1j = beta-hat + b-hat_1:
fixef(orange.nlme.1) + ranef(orange.nlme.1)[1,]

coef(orange.nlme.1)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/skript_folien/orange.nlme.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(orange.df$age, orange.df$circumference, xlim=c(0, 1600), ylim=c(0, 220), type='n', xlab='age (days since December 31, 1968)', ylab='circumference')
ages <- 118:1582
for (t in unique(orange.df$Tree)) {
  lines(ages, predict(orange.nlme.1, data.frame(Tree=rep(t, length(ages)), age=ages)), lwd=2)
  text(1600, predict(orange.nlme.1, data.frame(Tree=t, age=1600)) + 2.5*(t=='1') - 2.5*(t=='3'), t, cex=0.8)
}
lines(ages, predict(orange.nlme.1, data.frame(Tree=rep('1', length(ages)), age=ages), level=0), lwd=4, lty='22')
text(c(0, 365, 730, 1095, 1381)+182.5, 0, 1969:1973)
abline(v=c(0, 365, 730, 1095, 1461), lty='dotted')
dev.off()
} # end if (FALSE)


# only one random effect (for Asym)
orange.nlme.2 <- nlme(circumference ~ SSlogis(age, Asym, xmid, scal),
  fixed = Asym + xmid + scal ~ 1, random = Asym ~ 1 | Tree,
  start = c(170, 700, 500), data=orange.df)

anova(orange.nlme.1, orange.nlme.2)
