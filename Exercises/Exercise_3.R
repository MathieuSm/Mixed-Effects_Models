library(nlme)
library(ggplot2)

Machines.df <- data.frame(Machines)
plot(Machines.df)

# LMM with 2 levels. Random effect for Worker and for Machines within Worker
Machines.lme2 <- lme(score ~ Machine, random = ~ 1 | Worker/Machine, data=Machines.df)
summary(Machines.lme2)

# Equivalent model can be fitted as a single level LMM (See lecture notes P17)
Machines.lme.2a <- lme(score ~ Machine, random = list(Worker = pdBlocked(list(pdIdent(~ 1), pdIdent(~ Machine - 1)))), data=Machines.df)
summary(Machines.lme.2a)

# Comparison of the 2 models specification
VarCorr(Machines.lme2)
VarCorr(Machines.lme.2a)

# Second way of specifying model as single level:
# Make use of the fact that Worker (1st level) is alway observed together with Machine (2nd level)
# Perform this by adding restriction on the structure of variance/covariance matrix Psi* (Compound symmetry)
Machines.lme.2b <- lme(score ~ Machine, random = list(Worker = pdCompSymm(~ Machine - 1)), data=Machines.df)
VC <- VarCorr(Machines.lme.2b)
S1S2 <- as.numeric(VC[1])
Corr <- as.numeric(VC[10])
S1 <- S1S2 * Corr
S2 <- S1S2 - S1
print(S1)
print(S2)


# Exercise 1
Assay.df <- data.frame(Assay)
Assay.df$Block <- factor(as.numeric(levels(Assay.df$Block))[Assay.df$Block], ordered = FALSE)

# Plot data
plot(Assay.df)
ggplot(subset(Assay.df, Assay.df$Block==1), aes(dilut, logDens)) + geom_line(aes(group=sample))

# Fit model according to answer a (see exercise sheet)
Assay.lme.2b <- lme(logDens ~ sample*dilut, random = list(Block = pdBlocked(list(pdCompSymm(~ dilut - 1), pdCompSymm(~ sample - 1)))), data=Assay.df)
summary(Assay.lme.2b)

# Verify with simple definition (doesn't work)
Assay.lme.2a <- lme(logDens ~ sample*dilut, random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ dilut - 1), pdIdent(~ sample - 1)))), data=Assay.df)
summary(Assay.lme.2a)
VarCorr(Assay.lme.2a)

VC <- VarCorr(Assay.lme.2b)
S1S2 <- as.numeric(VC[1])
Corr <- as.numeric(VC[6])
S1 <- S1S2 * Corr
S2 <- S1S2 - S1
print(S1)
print(S2)

# Part c
Assay.lme.3 <- lme(logDens ~ sample * dilut, random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdIdent(~ dilut - 1)))), weights = varIdent(form = ~ 1 | sample * dilut), data=Assay.df)
summary(Assay.lme.3)

# Exercise 2
Dialyzer.df <- data.frame(Dialyzer)
Dialyzer.df$Subject <- factor(as.numeric(levels(Dialyzer.df$Subject))[Dialyzer.df$Subject], ordered = FALSE)

fm1Dial.lme <- lme(rate ~(pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB,
                   random = ~ pressure + I(pressure^2) | Subject, data = Dialyzer.df)
summary(fm1Dial.lme)

# Within group heteroscedasticity
plot(fm1Dial.lme, resid(.) ~pressure, abline = 0)

# Heteroscedastic model
fm2Dial.lme <- lme(rate ~(pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB,
                   random = ~ pressure + I(pressure^2) | Subject,
                   weights = varPower(form = ~ pressure) , data = Dialyzer.df)

summary(fm2Dial.lme)
plot(fm2Dial.lme, resid(., type = 'p') ~ pressure, abline = 0)
plot(augPred(fm2Dial.lme, primary = ~ pressure), grid = T)

# To understand, redo example 2.22
Orthodont.df <- data.frame(Orthodont)
ortho.lme.3 <- lme(distance ~ Sex * I(age-11), random = ~ I(age-11) | Subject, weights = varPower(form = ~ age|Sex), data=Orthodont.df)
summary(ortho.lme.3)

# Part c
fm3Dial.lme <- lme(rate ~(pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB,
                   random = ~ pressure + I(pressure^2) | Subject,
                   weights = varExp(form = ~ pressure) , data = Dialyzer.df)
summary(fm3Dial.lme)


# Exercise 3 (correspond to exercise 2 P268 of the book)
# (a)
Oats.df <- data.frame(Oats)
Oats.df$Block <- factor(as.character(Oats.df$Block))

Oats.lme5 <- lme(yield ~ nitro, random = ~1|Block, corr = corSymm(form = ~1|Block/Variety), data=Oats.df)

# (b)
intervals(Oats.lme5)
# All intervals overlap & similar magnitude -> compound symmetry structure suggested
Oats.lme6 <- lme(yield ~ nitro, random = ~1|Block, corr = corCompSymm(form = ~1|Block/Variety), data=Oats.df)
anova(Oats.lme5,Oats.lme6) # -> p=0.7: corCompSymm is better (less degrees of freedom)

# (c)
Oats.lme4 <- lme(yield ~ nitro, random = ~1|Block/Variety, data=Oats.df)
anova(Oats.lme6,Oats.lme4)

S1 <- 14.506
S.lme4 <- 12.867
S2.lme4 <- 11.005
S.lme6 <- 16.931
Corr <- 0.422

print(S.lme6^2)
print(S2.lme4^2 + S.lme4^2)
print(S2.lme4^2 / (S2.lme4^2 + S.lme4^2))
print(Corr)

# (d)
Oats.gls1 <- gls(yield ~ nitro, correlation = corCompSymm(form = ~1 | Block/Variety), data=Oats.df)
anova(Oats.lme6,Oats.gls1)


# Exercise 4 (correspond to exercise 1 P267 of the book)
BodyWeight.df <- data.frame(BodyWeight)
fm1BW.lme1 <- lme( weight ~ Time * Diet, random = ~ Time | Rat, data = BodyWeight.df)

# (a)
plot(fm1BW.lme, resid(.) ~ as.integer(Diet), abline = 0)

# (b)
fm1BW.lme2 <- lme( weight ~ Time * Diet, random = ~ Time | Rat, weights = varIdent(form = ~1|Diet), contrasts = contr.helmert, data = BodyWeight.df)
plot(fm2BW.lme, resid(.) ~ as.integer(Diet), abline = 0)
intervals(fm2BW.lme)

# (c)
fm2BW.lme <- lme( weight ~ Time * Diet, random = ~ Time | Rat, weights = varPower(), data = BodyWeight.df)
anova(fm1BW.lme1, fm1BW.lme2)
anova(fm2BW.lme, fm1BW.lme2)


# (d)
fm3BW.lme <- lme( weight ~ Time * Diet, random = ~ Time | Rat, weights = varPower(), corr = corExp(form = ~ Time), data = BodyWeight.df)
fm3BW.gls <- gls( weight ~ Time * Diet, weights = varPower(), corr = corCAR1(form = ~ Time | Rat), data = BodyWeight.df)
anova(fm3BW.lme,fm3BW.gls)

# Non-nested models example
Orthodont.df <- data.frame(Orthodont)
fm4Orth.gls <- gls( distance ~ Sex * I(age - 11), weights = varIdent(form = ~ 1 | Sex), corr = corCompSymm(form = ~ 1 | Subject), data =  Orthodont.df)
fm3Orth.lme <- lme(distance ~ Sex * I(age-11), random = ~ I(age - 11) | Subject, weights = varIdent(form = ~ 1 | Sex), data =  Orthodont.df )
anova(fm3Orth.lme,fm4Orth.gls)

