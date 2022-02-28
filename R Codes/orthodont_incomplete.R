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
