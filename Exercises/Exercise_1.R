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

