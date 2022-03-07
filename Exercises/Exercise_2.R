require(mgcv)
library(nlme)

Machines.df <- data.frame(Machines)
Machines.lme1 <- lme(score ~ Machine, random = ~ 1 | Worker, data=Machines.df)
Machines.lme2 <- lme(score ~ Machine, random = ~ 1 | Worker/Machine, data=Machines.df)
Machines.lme3 <- lme(score ~ Machine, random = ~ Machine - 1 | Worker, data=Machines.df)

summary(Machines.lme1)
VarCorr(Machines.lme1)
VarY.lme1 = extract.lme.cov(Machines.lme1)
VarY.lme2 = extract.lme.cov(Machines.lme2)
VarY.lme3 = extract.lme.cov(Machines.lme3)

Worker1 = c(as.numeric(rownames(Machines.df[Machines.df$Worker == 1, ])))
VarY.lme1[Worker1,Worker1]

extract.lme.cov2(Machines.lme1)
VarCorr(Machines.lme1)

extract.lme.cov2(Machines.lme2)
VarCorr(Machines.lme2)

extract.lme.cov2(Machines.lme3)
VarCorr(Machines.lme3)



library(lme4)
Machines.lmer1 <- lmer(score ~ Machine + ( 1 | Worker ), data=Machines.df)
Machines.lmer2 <- lmer(score ~ Machine + ( 1 | Worker/Machine ), data=Machines.df)
Machines.lmer3 <- lmer(score ~ Machine + ( Machine - 1 | Worker ), data=Machines.df)

Z = getME(Machines.lmer1, name = 'Z')
X = getME(Machines.lmer1, name= 'X')

getME(Machines.lmer3, name= 'm_i')

summary(Machines.lmer3)
