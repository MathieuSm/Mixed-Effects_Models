# Use nlme library for non-linear mixed-effect models
library(nlme)

# Import Oats data into a dataframe
Oats.df <- data.frame(Oats)
Oats.df$Block <- factor(as.character(Oats.df$Block))

# Fit data using linear mixed-effects model
Oats.lme1 <- lme(yield ~ ordered(nitro) * Variety, data = Oats.df, random = ~ 1 | Block/Variety)

# Perform analysis-of-variance to observe effects significance
anova(Oats.lme1)

# -> Refit without Variety*nitro interaction because non-significant
Oats.lme2 <- lme(yield ~ ordered(nitro) + Variety, data = Oats.df, random = ~ 1 | Block/Variety)
anova(Oats.lme2)
summary(Oats.lme2)

# -> Refit without Variety fixed-effect because non-significant
Oats.lme3 <- lme(yield ~ ordered(nitro), data = Oats.df, random = ~ 1 | Block/Variety)
anova(Oats.lme3)
summary(Oats.lme3)

# -> Refit using nitro as numeric variable as quadratic (Q) and cubic (C) terms are not significant
Oats.lme4 <- lme(yield ~ nitro, data = Oats, random = ~ 1 | Block/Variety)
# NB: Use Oats as Groupd Data to avoid plot error
anova(Oats.lme4)
summary(Oats.lme4)

# Examine variance components and confidence intervals
VarCorr(Oats.lme4)
intervals(Oats.lme4)

# Plot original data and fitted curves
plot( augPred(Oats.lme4), aspect = 0.7, layout = c(6,3), between = list( x = c(0, 0, 0.5)))

# write.csv(Machines.df,"Exercises/Machines.csv", row.names = FALSE)
library(lme4)
Oats.lme5 <- lmer(yield ~ nitro + (1 | Block:Variety), data = Oats)
summary(Oats.lme5)





# Exercise 2
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
