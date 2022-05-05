library(nlme)

indometh.df <- data.frame(Indometh)
indometh.df$Subject <- factor(as.character(indometh.df$Subject), ordered = FALSE)



if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/indometh.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(indometh.df$time, indometh.df$conc, xlab='time', ylab='concentration', type='n')
for (s.index in 1:length(unique(indometh.df$Subject))) {
  s <- unique(indometh.df$Subject)[s.index]
  lines(indometh.df$time[indometh.df$Subject==s], indometh.df$conc[indometh.df$Subject==s], type='b', pch=as.character(s), cex=0.8, lwd=2)
}
dev.off()
} # end if (FALSE)



indometh.nls <- nls(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), 
  data=indometh.df)
summary(indometh.nls)

plot(indometh.nls, Subject ~ resid(.), abline=0)

indometh.nlsList <- nlsList(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2) | Subject, 
  data=indometh.df)
indometh.nlsList

plot(intervals(indometh.nlsList))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/indometh.nlsList.intervals.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
plot(intervals(indometh.nlsList), par.settings=list(axis.text=list(cex=1.2), par.ylab.text=list(cex=1.2), add.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)


# self-starting function without start values - works in S-Plus, but not in R...
nlme(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), 
  fixed = A1 + lrc1 + A2 + lrc2 ~ 1,
  random = list(Subject = pdDiag(A1 + lrc1 + A2 + lrc2 ~ 1)), 
  data=indometh.df)

# self-starting function with start values
indometh.nlme.1 <- nlme(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), 
  fixed = A1 + lrc1 + A2 + lrc2 ~ 1, start = c(3, 1, 0.5, -1.5),
  random = list(Subject = pdDiag(A1 + lrc1 + A2 + lrc2 ~ 1)), 
  data=indometh.df)
indometh.nlme.1

# different start values
nlme(conc ~ SSbiexp(time, A1, lrc1, A2, lrc2), 
  fixed = A1 + lrc1 + A2 + lrc2 ~ 1, start = c(3, 1, 0.5, -1),
  random = list(Subject = pdDiag(A1 + lrc1 + A2 + lrc2 ~ 1)), 
  data=indometh.df)

# explicit model formula
nlme(conc ~ A1 * exp(-exp(lrc1)*time)  +  A2 * exp(-exp(lrc2)*time), 
  fixed = A1 + lrc1 + A2 + lrc2 ~ 1, start = c(3, 1, 0.5, -1.5),
  random = list(Subject = pdDiag(A1 + lrc1 + A2 + lrc2 ~ 1)), 
  data=indometh.df)

# using indometh.nlsList
nlme(indometh.nlsList, 
  random = list(Subject = pdDiag(A1 + lrc1 + A2 + lrc2 ~ 1)))

# grouping structure derived from indometh.nlsList
nlme(indometh.nlsList, random = pdDiag(A1 + lrc1 + A2 + lrc2 ~ 1))

indometh.nlme.1


# full model, including all random effects and correlations
indometh.nlme.0 <- nlme(indometh.nlsList,
  random = A1 + lrc1 + A2 + lrc2 ~ 1, 
  control=list(maxIter=1000, msMaxIter=1000, eval.max=1000))
# takes about 30 seconds on my laptop (HP EB 840 G6); many warnings
summary(indometh.nlme.0)
anova(indometh.nlme.0, indometh.nlme.1)


# dropping the random effect for lrc2
indometh.nlme.2 <- update(indometh.nlme.1, 
  random = list(Subject = pdDiag(A1 + lrc1 + A2 ~ 1)))
anova(indometh.nlme.2, indometh.nlme.1)


# adding correlations between the remaining random effects
indometh.nlme.3 <- update(indometh.nlme.2, 
  random = list(Subject = A1 + lrc1 + A2 ~ 1),
  control = list(msMaxIter=1000, eval.max=1000))
# or
indometh.nlme.3 <- update(indometh.nlme.2, 
  random = A1 + lrc1 + A2 ~ 1 | Subject,
  control = list(msMaxIter=1000, eval.max=1000))
anova(indometh.nlme.2, indometh.nlme.3)

indometh.nlme.3


# correlation between the random effects for A1 and lrc1 only
indometh.nlme.4 <- update(indometh.nlme.3, 
  random = list(Subject = pdBlocked(list(A1 + lrc1 ~ 1, A2 ~ 1))))


anova(indometh.nlme.2, indometh.nlme.4, indometh.nlme.3)

summary(indometh.nlme.4)


# could also consider dropping the random intercept for lrc1:
indometh.nlme.5 <- update(indometh.nlme.4, 
  random = list(Subject = pdBlocked(list(A1 ~ 1, A2 ~ 1))))
anova(indometh.nlme.4, indometh.nlme.5)

