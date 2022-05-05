library(nlme)

oats.df <- data.frame(Oats)
oats.df$Block <- factor(as.character(oats.df$Block))

plot(oats.df$nitro, oats.df$yield, type='n', xlab='nitro', ylab='yield', axes=FALSE)
axis(1, at=seq(0, 0.6, 0.2))
axis(2)
box()
for (b.index in 1:length(unique(oats.df$Block))) {
  b <- unique(oats.df$Block)[b.index]
  for (v.index in 1:length(unique(oats.df$Variety))) {
    v <- unique(oats.df$Variety)[v.index]
    lines(oats.df$nitro[oats.df$Block==b & oats.df$Variety==v],
          oats.df$yield[oats.df$Block==b & oats.df$Variety==v],
          type='b', pch='', lty=v.index, lwd=2)
    text(oats.df$nitro[oats.df$Block==b & oats.df$Variety==v],
         oats.df$yield[oats.df$Block==b & oats.df$Variety==v],
         b, cex=0.6)
  }
}
legend('topleft', lty=1:3, lwd=2, legend=unique(oats.df$Variety))



###########
# Sheet 1 #
###########

oats.lme.1 <- lme(yield ~ ordered(nitro) * Variety, 
  random = ~ 1 | Block/Variety, data = oats.df)
anova(oats.lme.1)

oats.lme.2 <- lme(yield ~ ordered(nitro) + Variety, 
  random = ~ 1 | Block/Variety, data = oats.df)
#oats.lme.2 <- update(oats.lme.1, yield ~ ordered(nitro) + Variety)
anova(oats.lme.2)
summary(oats.lme.2)

oats.lme.3 <- lme(yield ~ ordered(nitro), 
  random = ~ 1 | Block/Variety, data = oats.df)
summary(oats.lme.3)

oats.lme.4 <- lme(yield ~ nitro, 
  random = ~ 1 | Block/Variety, data = oats.df)
summary(oats.lme.4)


# polynomial contrasts for ordered(nitro):
matplot(1:4, contr.poly(4), type='b', lwd=3, lty='solid', cex=1.3, 
  xaxt='n', xlab='', pch=c('L', 'Q', 'C'))



###########
# Sheet 2 #
###########

fm1Oats <- lme(yield ~ ordered(nitro) * Variety, 
  random = ~ 1 | Block/Variety, data = oats.df)
fm2Oats <- lme(yield ~ ordered(nitro) + Variety, 
  random = ~ 1 | Block/Variety, data = oats.df)
fm3Oats <- lme(yield ~ ordered(nitro), 
  random = ~ 1 | Block/Variety, data = oats.df)
fm4Oats <- lme(yield ~ nitro, 
  random = ~ 1 | Block/Variety, data = oats.df)


# Exercise 1

# (a), verification
anova(fm1Oats)
anova(fm2Oats)
anova(fm3Oats)
anova(fm4Oats)

# (b)
qt(0.975, 53)
73.66667 + c(-1, 1) * qt(0.975, 53) * 6.781485
intervals(fm4Oats)

# (c), verification
fm4Oats.0 <- lme(yield ~ nitro, 
  random = ~ 1 | Block, data = oats.df)
anova(fm4Oats.0)

# (d)
summary(fm4Oats)
summary(fm4Oats.0)
1 - pchisq(2 * ((-296.5209)-(-302.3518)), 1)
anova(fm4Oats.0, fm4Oats)


# Exercise 2

windows(record=TRUE)

# observed vs. fitted

plot(fm4Oats, yield ~ fitted(.), 
  main='fm4Oats', grid=FALSE, abline=c(0,1), lty='dotted')
plot(fm4Oats.0, yield ~ fitted(.), 
  main='fm4Oats.0', grid=FALSE, abline=c(0,1), lty='dotted')

# residuals

plot(fm4Oats, resid(.) ~ fitted(.) | Block, 
  main='fm4Oats', grid=FALSE, abline=0, lty='dotted')
plot(fm4Oats.0, resid(.) ~ fitted(.) | Block, 
  main='fm4Oats.0', grid=FALSE, abline=0, lty='dotted')

plot(fm4Oats, resid(.) ~ fitted(.) | Variety, 
  main='fm4Oats', grid=FALSE, abline=0, lty='dotted')
plot(fm4Oats.0, resid(.) ~ fitted(.) | Variety, 
  main='fm4Oats.0', grid=FALSE, abline=0, lty='dotted')

plot(fm4Oats, resid(.) ~ nitro | Variety, 
  main='fm4Oats', grid=FALSE, abline=0, lty='dotted')
plot(fm4Oats.0, resid(.) ~ nitro | Variety, 
  main='fm4Oats.0', grid=FALSE, abline=0, lty='dotted')

plot(fm4Oats, resid(.) ~ as.numeric(Variety) | Block, 
  main='fm4Oats', grid=FALSE, abline=0, lty='dotted')
plot(fm4Oats.0, resid(.) ~ as.numeric(Variety) | Block, 
  main='fm4Oats.0', grid=FALSE, abline=0, lty='dotted')
# or
plot(fm4Oats, Variety ~ resid(.) | Block, 
  main='fm4Oats', grid=FALSE, abline=0, lty='dotted')
plot(fm4Oats.0, Variety ~ resid(.) | Block, 
  main='fm4Oats.0', grid=FALSE, abline=0, lty='dotted')

qqnorm(fm4Oats, ~ resid(.), main='fm4Oats')
qqnorm(fm4Oats.0, ~ resid(.), main='fm4Oats.0')

# random effects

qqnorm(fm4Oats, ~ ranef(., level=1), main='fm4Oats, Block level')
qqnorm(fm4Oats, ~ ranef(., level=2), main='fm4Oats, Variety %in% Block level')
qqnorm(fm4Oats.0, ~ ranef(.), main='fm4Oats.0')

re <- ranef(fm4Oats, level=2)
# augFrame=TRUE wouldn't add the blocks/varieties 
# ==> extract these from the rownames
re$Block <- factor(substring(rownames(re), 1, regexpr('/', rownames(re))-1))
re$Variety <- factor(substring(rownames(re), regexpr('/', rownames(re))+1, nchar(rownames(re))))
with(re, plot(as.numeric(Block), `(Intercept)`))
abline(h=0, lty='dotted')
with(re, plot(as.numeric(Variety), `(Intercept)`))
abline(h=0, lty='dotted')

# it's almost impossible to do anything useful at the block level 
# (with only 6 predicted random effects)


###########
# Sheet 3 #
###########

# Exercise 3 (exercise 5.2 from the book)

# (a)
oats.lme.5 <- lme(yield ~ nitro, 
  random = ~ 1 | Block, 
  correlation = corSymm(form = ~ 1 | Block/Variety), 
  data = oats.df)
oats.lme.5

# (b)
intervals(oats.lme.5)

cor.intervals <- intervals(oats.lme.5)$corStruct
plot(rep(1:6, 2), c(cor.intervals[,'lower'], cor.intervals[,'upper']), type='n', xlab='', ylab='correlation', axes=FALSE) 
axis(1, at=1:6, labels=dimnames(cor.intervals)[[1]])
axis(2)
box()
points(1:6, cor.intervals[,'est.'])
segments(1:6, cor.intervals[,'lower'], 1:6, cor.intervals[,'upper'], lwd=2)
abline(h=0, lty='dotted')
polygon(c(0, 7, 7, 0, 0), c(rep(c(max(cor.intervals[,'lower']), min(cor.intervals[,'upper'])), each=2), max(cor.intervals[,'lower'])), border=NA, density=20, col='blue')
	
oats.lme.6 <- lme(yield ~ nitro, 
  random = ~ 1 | Block, 
  correlation = corCompSymm(form = ~ 1 | Block/Variety), 
  data = oats.df)
oats.lme.6

abline(h=0.4224608, col='blue', lwd=2)

anova(oats.lme.6, oats.lme.5)

# (c)
anova(oats.lme.4, oats.lme.6)  # logLik is the same

VarCorr(oats.lme.4)
VarCorr(oats.lme.6)
121.1029 + 165.5587
121.1029 / (121.1029 + 165.5587)


# (d)
oats.gls.1 <- gls(yield ~ nitro, 
  correlation = corCompSymm(form = ~ 1 | Block/Variety), 
  data = oats.df)

anova(oats.gls.1, oats.lme.4)  # random intercept at the block level is significant

