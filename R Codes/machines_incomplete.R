library(nlme)

# create a data frame from the grouped data object, omitting the ordering of workers
machines.df <- data.frame(Machines)
machines.df$Worker <- factor(as.numeric(levels(machines.df$Worker))[machines.df$Worker], ordered = FALSE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/machines.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(as.numeric(machines.df$Worker), machines.df$score, pch=as.numeric(machines.df$Machine), xlab='Worker', ylab='Productivity score')
legend('bottomleft', pch=1:3, paste('Machine', levels(machines.df$Machine)))
dev.off()
}

# interaction plot - two possibilities
with(machines.df, interaction.plot(Worker, Machine, score))
with(machines.df, interaction.plot(Machine, Worker, score))
if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/machines.interaction.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
with(machines.df, interaction.plot(Worker, Machine, score, lwd=2, legend=FALSE))
legend('bottomleft', lty=c(3, 2, 1), lwd=2, paste('Machine', c('A', 'B', 'C')))
dev.off()
}

# model without interaction
machines.lme.1 <- lme(score ~ Machine, random = ~ 1 | Worker, data=machines.df)
summary(machines.lme.1)
# results in the book are based on Helmert contrasts:
machines.lme.1.helmert <- lme(score ~ Machine, random = ~ 1 | Worker, contrasts=list(Machine=contr.helmert), data=machines.df)
machines.lme.1.helmert

# model with general interaction for Machine x Worker
machines.lme.3 <- lme(score ~ Machine, random = ~ Machine - 1 | Worker, data=machines.df)
summary(machines.lme.3)
# results in the book are based on Helmert contrasts:
machines.lme.3.helmert <- lme(score ~ Machine, random = ~ Machine - 1 | Worker, data=machines.df, contrasts=list(Machine=contr.helmert))
summary(machines.lme.3.helmert)
# models are equivalent:
fitted.values(machines.lme.3)-fitted.values(machines.lme.3.helmert)
ranef(machines.lme.3)-ranef(machines.lme.3.helmert)


library(lme4)
machines.lmer.1 <- lmer(score ~ Machine + (1 | Worker), data=machines.df)
machines.lmer.1
machines.lmer.3 <- lmer(score ~ Machine + (Machine - 1 | Worker), data=machines.df)
machines.lmer.3  # slightly different estimate of sd compared to lme
detach(package:lme4)


# two-level model - less general interaction
machines.lme.2 <- lme(score ~ Machine, random = ~ 1 | Worker/Machine, data=machines.df)
# equivalent:     lme(score ~ Machine, random = list(Worker = ~ 1, Machine = ~ 1), data=machines.df)
machines.lme.2

library(lme4)
machines.lmer.2 <- lmer(score ~ Machine + (1 | Worker/Machine), data=machines.df)
machines.lmer.2
# or: lmer(score ~ Machine + (1 | Worker) + (1 | Machine:Worker), data=machines.df)
detach(package:lme4)


# LRT
anova(machines.lme.1, machines.lme.2, machines.lme.3)

library(lme4)
anova(machines.lmer.1, machines.lmer.2, machines.lmer.3) # switches to ML estimation
anova(machines.lmer.1, machines.lmer.2, machines.lmer.3, refit=FALSE)
detach(package:lme4)

anova(machines.lme.1, machines.lme.3) # direct comparison, not included above


# conditional t and F tests
summary(machines.lme.2)
anova(machines.lme.2, type='marginal')
anova(machines.lme.2)  # sequential

# LRT for effects of Machine (not recommended)
machines.lme.2.ml <- lme(score ~ Machine, random = ~ 1 | Worker/Machine, 
  method='ML', data=machines.df)
machines.lme.2.0.ml <- lme(score ~ 1, random = ~ 1 | Worker/Machine, 
  method='ML', data=machines.df)
anova(machines.lme.2.0.ml, machines.lme.2.ml)

# conditional F test in the more general model
anova(machines.lme.3)  # sequential


# lme4 / lmerTest
library(lme4)
summary(machines.lmer.2)  # t statistics, but no degrees of freedom / p values
library(lmerTest)
# refit the model after loading lmerTest:
machines.lmer.2 <- lmer(score ~ Machine + (1 | Worker/Machine), data=machines.df)
summary(machines.lmer.2)  # df, p value added (Satterthwaite)
summary(machines.lmer.2, ddf='Kenward-Roger')
anova(machines.lmer.2)
drop1(machines.lmer.2)
machines.lmer.3 <- lmer(score ~ Machine + (Machine - 1 | Worker), data=machines.df)
drop1(machines.lmer.3)
drop1(machines.lmer.3, ddf='Kenward-Roger')
detach(package:lmerTest)
detach(package:lme4)




# confidence intervals
intervals(machines.lme.1)
intervals(machines.lme.2)
intervals(machines.lme.3)

library(lme4)
confint(machines.lmer.1)
confint(machines.lmer.2)
confint(machines.lmer.2, oldNames=FALSE) # easier to interpret
confint(machines.lmer.3, oldNames=FALSE)
confint(machines.lmer.3, method='Wald') # for fixed effects only
confint(machines.lmer.3, method='boot')
detach(package:lme4)



# within-group residuals
plot(machines.lme.1, Worker ~ resid(.), abline=0)
boxplot(resid(machines.lme.1) ~ machines.df$Worker)
abline(h=0)

plot(machines.lme.1, Machine ~ resid(.) | Worker, abline=0)
boxplot(resid(machines.lme.1) ~ machines.df$Worker * machines.df$Machine, cex.axis=0.7)
abline(h=0)


plot(machines.lme.1, resid(.) ~ fitted(.) | Machine, abline=0, grid=FALSE)




# obtain the two-level model by restrictions on Psi

VarCorr(machines.lme.2)

machines.lme.2a <- lme(score ~ Machine, 
  random = list(Worker = pdBlocked(list(pdIdent(~ 1), pdIdent(~ Machine - 1)))),
  data=machines.df)
VarCorr(machines.lme.2a)

machines.lme.2b <- lme(score ~ Machine, 
  random = list(Worker = pdCompSymm(~ Machine - 1)), data=machines.df)
VarCorr(machines.lme.2b)

