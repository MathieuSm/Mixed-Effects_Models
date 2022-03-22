library(nlme)

orthodont.df <- data.frame(Orthodont)
orthodont.df$Subject <- factor(as.character(orthodont.df$Subject), ordered = FALSE)

par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(orthodont.df$age, orthodont.df$distance, xlab='age', ylab='distance', type='n', axes=FALSE)
axis(1, at=seq(8, 14, 2))
axis(2)
box()
for (s.index in 1:length(unique(orthodont.df$Subject))) {
  s <- unique(orthodont.df$Subject)[s.index]
  lines(orthodont.df$age[orthodont.df$Subject==s], orthodont.df$distance[orthodont.df$Subject==s], lty=1*(unique(orthodont.df$Sex[orthodont.df$Subject==s])=='Female')+1, lwd=2)
}
legend('bottomright', lty=c(1, 2), c('Male', 'Female'), lwd=2)


##############
# girls only #
##############

ortho.fem <- orthodont.df[orthodont.df$Sex=='Female',]

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.fem.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(ortho.fem$age, ortho.fem$distance, xlab='age', ylab='distance', type='n', ylim=c(16, 28), axes=FALSE)
axis(1, at=seq(8, 14, 2))
axis(2)
box()
for (s.index in 1:length(unique(ortho.fem$Subject))) {
  s <- unique(ortho.fem$Subject)[s.index]
  lines(ortho.fem$age[ortho.fem$Subject==s], ortho.fem$distance[ortho.fem$Subject==s], lty=3*(s.index%%2)+2, lwd=2)
}
dev.off()
} # end if (FALSE)


# separate linear regression models per subject
ortho.fem.lmlist.1 <- lmList(distance ~ age | Subject, data=ortho.fem)
coef(ortho.fem.lmlist.1)
lapply(ortho.fem.lmlist.1, summary) # sigma-hat is also different
cbind(coef(ortho.fem.lmlist.1), 
  sigma=sapply(ortho.fem.lmlist.1, function(m) {summary(m)$sigma}))
plot(intervals(ortho.fem.lmlist.1))

# same as before, but with centered data
ortho.fem.lmlist.2 <- lmList(distance ~ I(age-11) | Subject, data=ortho.fem)
coef(ortho.fem.lmlist.2)
plot(intervals(ortho.fem.lmlist.2))

# a similar (common) model using lm
summary(lm(distance ~ age * Subject, data=ortho.fem))

# plot of the fitted regression lines
if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.fem.lmlist.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(ortho.fem$age, ortho.fem$distance, xlab='age', ylab='distance', type='n', ylim=c(16, 28), axes=FALSE)
axis(1, at=seq(8, 14, 2))
axis(2)
box()
for (s.index in 1:length(unique(ortho.fem$Subject))) {
  s <- unique(ortho.fem$Subject)[s.index]
  lines(ortho.fem$age[ortho.fem$Subject==s], ortho.fem$distance[ortho.fem$Subject==s], lty=3*(s.index%%2)+2, lwd=2, col='grey')
}
for (s.index in 1:length(unique(ortho.fem$Subject))) {
#  abline(rbind(c(1,-11),c(0,1)) %*% as.numeric(coef(ortho.fem.lmlist.2)[s.index,]), lwd=2)
  abline(as.numeric(coef(ortho.fem.lmlist.1)[s.index,]), lwd=2)
}
dev.off()
} # end if (FALSE)

# random intercept model
ortho.fem.lme.1 <- lme(distance ~ age, random = ~ 1 | Subject, data=ortho.fem)
ortho.fem.lme.1

# comparison to the individual regression lines
plot(ortho.fem$age, ortho.fem$distance, xlab='age', ylab='distance', type='n', ylim=c(16, 28), axes=FALSE)
axis(1, at=seq(8, 14, 2))
axis(2)
box()
for (s.index in 1:length(unique(ortho.fem$Subject))) {
  abline(as.numeric(coef(ortho.fem.lmlist.1)[s.index,]), lwd=2, col='grey')
}
for (s.index in 1:length(unique(ortho.fem$Subject))) {
  abline(as.numeric(coef(ortho.fem.lme.1)[s.index,]), lwd=2)
}
legend('bottomright', lty=1, lwd=2, col=c('grey', 'black'), c('individual regressions', 'random intercept model'))


# random intercept and slope model
ortho.fem.lme.2 <- lme(distance ~ age, random = ~ age | Subject, data=ortho.fem)
ortho.fem.lme.2

# comparison to the individual regression lines
plot(ortho.fem$age, ortho.fem$distance, xlab='age', ylab='distance', type='n', ylim=c(16, 28), axes=FALSE)
axis(1, at=seq(8, 14, 2))
axis(2)
box()
for (s.index in 1:length(unique(ortho.fem$Subject))) {
  abline(as.numeric(coef(ortho.fem.lmlist.1)[s.index,]), lwd=2, col='grey')
}
for (s.index in 1:length(unique(ortho.fem$Subject))) {
  abline(as.numeric(coef(ortho.fem.lme.2)[s.index,]), lwd=2)
}
legend('bottomright', lty=1, lwd=2, col=c('grey', 'black'), c('individual regressions', 'random intercept & slope model'))


library(lme4)
ortho.fem.lmer.1 <- lmer(distance ~ age + (1 | Subject), data=ortho.fem)
ortho.fem.lmer.1
ortho.fem.lmer.2 <- lmer(distance ~ age + (age | Subject), data=ortho.fem)
ortho.fem.lmer.2
detach(package:lme4)


# model comparison: LRT
anova(ortho.fem.lme.1, ortho.fem.lme.2)

detach(package:nlme)
library(lme4)
ortho.fem.lmer.1 <- lmer(distance ~ age + (1 | Subject), data=ortho.fem)
ortho.fem.lmer.2 <- lmer(distance ~ age + (age | Subject), data=ortho.fem)
anova(ortho.fem.lmer.1, ortho.fem.lmer.2) # switches to ML estimation
anova(ortho.fem.lmer.1, ortho.fem.lmer.2, refit=FALSE)
detach(package:lme4)
library(nlme)



##################
# girls and boys #
##################

ortho.lme.1 <- lme(distance ~ I(age-11), random = ~ I(age-11) | Subject, data=orthodont.df)
ortho.lme.1

ortho.lme.2 <- lme(distance ~ Sex * I(age-11), random = ~ I(age-11) | Subject, data=orthodont.df)
summary(ortho.lme.2)


fitted(ortho.lme.2, level=0:1)


# observed vs. fitted
plot(ortho.lme.2, distance ~ fitted(.), abline=c(0, 1))
# or:
plot(fitted(ortho.lme.2), orthodont.df$distance)
abline(0, 1)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.lme.2.observed.vs.fitted.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(fitted(ortho.lme.2), orthodont.df$distance, 
  xlim=range(orthodont.df$distance), ylim=range(orthodont.df$distance),
  xlab='Fitted value', ylab='Observed value')
abline(0, 1)
dev.off()
} # end if (FALSE)


##########################
# within-group residuals #
##########################

# boxplot of residuals by subject
plot(ortho.lme.2, Subject ~ resid(.), abline=0)
# or:
boxplot(resid(ortho.lme.2) ~ orthodont.df$Subject)
abline(h=0, lty='dotted')

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.lme.2.residuals.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3.5,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
boxplot(resid(ortho.lme.2) ~ orthodont.df$Subject, axes=FALSE, ylab='Residual', ylim=c(-1, 1)*max(abs(resid(ortho.lme.2))))
axis(1, at=1:length(levels(orthodont.df$Subject)), labels=levels(orthodont.df$Subject), las=2)
axis(2)
mtext(side=1, line=2.3, 'Subject', cex=1.2)
box()
abline(h=0, lty='dotted')
dev.off()
}

# alternative to the boxplot
plot(resid(ortho.lme.2) ~ as.numeric(orthodont.df$Subject), axes=FALSE, ylab='Residual', xlab='Subject', ylim=c(-1, 1)*max(abs(resid(ortho.lme.2))), pch=16)
segments(1:length(levels(orthodont.df$Subject)), sapply(split(resid(ortho.lme.2), orthodont.df$Subject), min), 1:length(levels(orthodont.df$Subject)), sapply(split(resid(ortho.lme.2), orthodont.df$Subject), max))
segments(1:length(levels(orthodont.df$Subject)), sapply(split(resid(ortho.lme.2), orthodont.df$Subject), function(x) {sort(x)[2]}), 1:length(levels(orthodont.df$Subject)), sapply(split(resid(ortho.lme.2), orthodont.df$Subject), function(x) {sort(x)[3]}), lwd=5)
axis(1, at=1:length(levels(orthodont.df$Subject)), labels=levels(orthodont.df$Subject), las=2)
axis(2)
box()
abline(h=0, lty='dotted')

# scatter plots of residuals vs. fitted values, by sex
plot(ortho.lme.2, resid(.) ~ fitted(.) | Sex, grid=FALSE, abline=0, lty='dotted')
# or:
par(mfrow=c(1,2))
plot(fitted(ortho.lme.2)[orthodont.df$Sex=='Female'], resid(ortho.lme.2)[orthodont.df$Sex=='Female'], ylim=c(-1, 1)*max(abs(resid(ortho.lme.2))), xlim=range(fitted(ortho.lme.2)), xlab='Fitted value', ylab='Residual', main='Sex == Female')
abline(h=0, lty='dotted')
plot(fitted(ortho.lme.2)[orthodont.df$Sex=='Male'], resid(ortho.lme.2)[orthodont.df$Sex=='Male'], ylim=c(-1, 1)*max(abs(resid(ortho.lme.2))), xlim=range(fitted(ortho.lme.2)), xlab='Fitted value', ylab='Residual', main='Sex == Male')
abline(h=0, lty='dotted')
par(mfrow=c(1,1))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.lme.2.res.by.sex.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1)
plot(ortho.lme.2, resid(.) ~ fitted(.) | Sex, grid=FALSE, abline=0, lty='dotted', scales=list(cex=1.2), 
  par.settings=list(axis.text=list(cex=1.2), par.xlab.text=list(cex=1.2), par.ylab.text=list(cex=1.2), add.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)

# residuals vs. age by subject
plot(ortho.lme.2, resid(.) ~ age | Subject, grid=FALSE, abline=0, lty='dotted')
# girls and boys in separate rows
plot(ortho.lme.2, resid(.) ~ age | Subject, grid=FALSE, abline=0, lty='dotted', layout=c(6,5,1), skip=c(rep(FALSE, 11), TRUE, rep(FALSE, 16)))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.lme.2.res.vs.age.by.subject.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=0.8)
plot(ortho.lme.2, resid(.) ~ age | Subject, grid=FALSE, abline=0, lty='dotted', layout=c(8,4,1), skip=c(rep(FALSE, 11), rep(TRUE, 5), rep(FALSE, 16)),
  scales=list(x=list(limits=c(7, 15), at=c(8, 10, 12, 14)), y=list(at=c(-3, 0, 3))),
  par.settings=list(par.xlab.text=list(cex=1.2), par.ylab.text=list(cex=1.2)))
#plot(ortho.lme.2, resid(.) ~ fitted(.) | Sex, grid=FALSE, abline=0, lty='dotted', scales=list(cex=1.2), 
#  par.settings=list(axis.text=list(cex=1.2), par.xlab.text=list(cex=1.2), par.ylab.text=list(cex=1.2), add.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)

# normal QQ plots
qqnorm(resid(ortho.lme.2))
# or:
qqnorm(ortho.lme.2, ~ resid(.))

# identifying the outliers: (ESC to stop)
identify(qqnorm(resid(ortho.lme.2)), labels=orthodont.df$Subject)

# separate normal QQ plots by sex
par(mfrow=c(1,2))
qqnorm(resid(ortho.lme.2)[orthodont.df$Sex=='Male'], xlim=c(-2.5, 2.5), ylim=range(resid(ortho.lme.2)), main='Sex == Male')
qqnorm(resid(ortho.lme.2)[orthodont.df$Sex=='Female'], xlim=c(-2.5, 2.5), ylim=range(resid(ortho.lme.2)), main='Sex == Female')
par(mfrow=c(1,1))
# this is not the same as
qqnorm(ortho.lme.2, ~ resid(.) | Sex)
# which roughly corresponds to:
qqnorm(resid(ortho.lme.2), col=as.numeric(orthodont.df$Sex)+1, datax=TRUE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.lme.2.qqnorm.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
qqnorm(resid(ortho.lme.2), main='')
dev.off()
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.lme.2.qqnorm.by.sex.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,1.5,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
par(mfrow=c(1,2))
qqnorm(resid(ortho.lme.2)[orthodont.df$Sex=='Male'], xlim=c(-2.5, 2.5), ylim=range(resid(ortho.lme.2)), main='Sex == Male', cex.lab=1.2)
qqnorm(resid(ortho.lme.2)[orthodont.df$Sex=='Female'], xlim=c(-2.5, 2.5), ylim=range(resid(ortho.lme.2)), main='Sex == Female', cex.lab=1.2)
par(mfrow=c(1,1))
dev.off()
} # end if (FALSE)

# attempts to assess the independence of residuals and random effects
# residuals vs. single random effects
plot(ortho.lme.2, resid(.) ~ rep(ranef(.)$'(Intercept)', each=4), grid=FALSE, abline=0, lty='dotted', xlab='Random intercepts')
plot(ortho.lme.2, resid(.) ~ rep(ranef(.)$'I(age - 11)', each=4), grid=FALSE, abline=0, lty='dotted', xlab='Random coefficients for age')
# residuals vs. random-effects part of the predictions
plot(ortho.lme.2, resid(.) ~ fitted(., level=1) - fitted(., level=0), grid=FALSE, abline=0, lty='dotted', xlab='Random-effects part of the prediction')
plot(ortho.lme.2, resid(.) ~ fitted(., level=1) - fitted(., level=0) | Subject, grid=FALSE, abline=0, lty='dotted', xlab='Random-effects part of the prediction')


##################
# random effects #
##################

# QQ plots
qqnorm(ortho.lme.2, ~ ranef(.))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/ortho.lme.2.qqnorm.ranef.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
qqnorm(ortho.lme.2, ~ ranef(.), id=0.1, cex=0.8, adj=c(0.5, -0.6),
#qqnorm(ortho.lme.2, ~ ranef(.), 
  par.settings=list(axis.text=list(cex=1.2), par.xlab.text=list(cex=1.2), par.ylab.text=list(cex=1.2), add.text=list(cex=1.2)))
dev.off()
} # end if (FALSE)

# boxplots of random intercepts/age effects by sex
re <- ranef(ortho.lme.2, augFrame=TRUE)
boxplot(re$'(Intercept)' ~ re$Sex)
abline(h=0, lty='dotted')
boxplot(re$'I(age - 11)' ~ re$Sex)
abline(h=0, lty='dotted')

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/skript_folien/ortho.lme.2.boxplot.ranef.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
par(mfrow=c(1,2))
boxplot(re$'(Intercept)' ~ re$Sex, 
  ylim=c(-1, 1)*max(abs(re$'(Intercept)')), xlab='Sex', ylab='Intercept', cex.lab=1.2)
abline(h=0, lty='dotted')
boxplot(re$'I(age - 11)' ~ re$Sex, 
  ylim=c(-1, 1)*max(abs(re$'I(age - 11)')), xlab='Sex', ylab='I(age - 11)', cex.lab=1.2)
abline(h=0, lty='dotted')
par(mfrow=c(1,1))
dev.off()
} # end if (FALSE)

# homogeneity of Psi across sexes (covariance structure of the random effects):
# scatter plots of the predicted random effects, separately for girls and boys
re <- ranef(ortho.lme.2, augFrame=TRUE)
par(mfrow=c(1, 2))
with(re[re$Sex=='Male',], plot(`(Intercept)`, `I(age - 11)`, 
  xlim=c(-1, 1)*max(abs(re$`(Intercept)`)), ylim=c(-1, 1)*max(abs(re$`I(age - 11)`)),
  main='Sex == Male', xlab='Intercept'))
with(re[re$`I(age - 11)` > 0.2,], text(`(Intercept)`, `I(age - 11)`, 
  dimnames(re)[[1]][re$`I(age - 11)` > 0.2], cex=0.8, adj=c(0.5, 1.6)))
abline(h=0, v=0, lty='dotted')
with(re[re$Sex=='Female',], plot(`(Intercept)`, `I(age - 11)`, 
  xlim=c(-1, 1)*max(abs(re$`(Intercept)`)), ylim=c(-1, 1)*max(abs(re$`I(age - 11)`)),
  main='Sex == Female', xlab='Intercept'))
abline(h=0, v=0, lty='dotted')
par(mfrow=c(1, 1))

# similar plot, using lattice::xyplot
with(re, lattice::xyplot(`I(age - 11)` ~ `(Intercept)` | Sex, abline=list(h=0, v=0, lty='dotted')))

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/skript_folien/ortho.lme.2.scatter.ranef.by.sex.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,1.5,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
re <- ranef(ortho.lme.2, augFrame=TRUE)
par(mfrow=c(1, 2))
with(re[re$Sex=='Male',], plot(`(Intercept)`, `I(age - 11)`, 
  xlim=c(-1, 1)*max(abs(re$`(Intercept)`)), ylim=c(-1, 1)*max(abs(re$`I(age - 11)`)),
  main='Sex == Male', xlab='Intercept', cex.lab=1.2))
with(re[re$`I(age - 11)` > 0.2,], text(`(Intercept)`, `I(age - 11)`, 
  dimnames(re)[[1]][re$`I(age - 11)` > 0.2], cex=0.8, adj=c(0.5, 1.6)))
abline(h=0, v=0, lty='dotted')
with(re[re$Sex=='Female',], plot(`(Intercept)`, `I(age - 11)`, 
  xlim=c(-1, 1)*max(abs(re$`(Intercept)`)), ylim=c(-1, 1)*max(abs(re$`I(age - 11)`)),
  main='Sex == Female', xlab='Intercept', cex.lab=1.2))
abline(h=0, v=0, lty='dotted')
par(mfrow=c(1, 1))
dev.off()
} # end if (FALSE)



# extracting fitted values/residuals/predicted random effects using lme4:
library(lme4)
ortho.lmer.2 <- lmer(distance ~ Sex * I(age-11) + (I(age-11) | Subject), data=orthodont.df)
fitted(ortho.lmer.2)
predict(ortho.lmer.2, re.form = ~ 0) # fixed effects only
resid(ortho.lmer.2)
resid(ortho.lmer.2, type='pearson', scaled=TRUE) # standardized residuals
ranef(ortho.lmer.2)
detach(package:lme4)




##############################
# independent random effects #
##############################

ortho.lme.2a <- lme(distance ~ Sex * I(age-11), random = list(Subject = pdDiag(~ I(age-11))), data=orthodont.df)
anova(ortho.lme.2, ortho.lme.2a)


library(lme4)
ortho.lmer.2a <- lmer(distance ~ Sex * I(age-11) + (I(age-11) || Subject), data=orthodont.df)
anova(ortho.lmer.2, ortho.lmer.2a, refit=FALSE)
detach(package:lme4)



