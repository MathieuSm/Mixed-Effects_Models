library(lme4)

# Example 2.12
Machines.df <- data.frame(Machines)
plot(Machines.df)

Machines.lmer <- lmer(score ~ Machine + ( 1 | Worker/Machine ), data=Machines.df)

# To see the number of parameters
X = getME(Machines.lmer, name = 'X')

library(nlme)
Machines.lme <- lme(score ~ Machine, random = ~ 1 | Worker/Machine, data=Machines.df)
summary(Machines.lme)
anova(Machines.lme, type='marginal')

intervals(Machines.lme)


# Exercise 1
Oats.df <- data.frame(Oats)
Oats.df$Block <- factor(as.character(Oats.df$Block))

plot(Oats.df)

Oats.lme1 <- lme(yield ~ ordered(nitro) * Variety, random = ~1|Block/Variety, data=Oats.df)
Oats.lme2 <- lme(yield ~ ordered(nitro) + Variety, random = ~1|Block/Variety, data=Oats.df)
Oats.lme3 <- lme(yield ~ ordered(nitro), random = ~1|Block/Variety, data=Oats.df)
Oats.lme4 <- lme(yield ~ nitro, random = ~1|Block/Variety, data=Oats.df)
Oats.lme4.0 <- lme(yield ~ nitro, random = ~1|Block, data=Oats.df)


anova(Oats.lme4.0)

intervals(Oats.lme4)

73.666666666 - qt(1-0.05/2, 53) * 6.781485

2*(-296.52 - (- 302.35))
anova(Oats.lme4, Oats.lme4.0)


Oats.lmer1 <- lmer(yield ~ ordered(nitro) * Variety + (1|Block/Variety), data=Oats.df)
anova(Oats.lmer1)


# To see the number of parameters
X = getME(Oats.lmer1, name = 'X')
matrix(X)


# Exercise 2
re4 = ranef(Oats.lme4)
boxplot(re4$Block$`(Intercept)`)
boxplot(re4$Variety$`(Intercept)`)
qqnorm(Oats.lme4, ~resid(.)|Block)
qqnorm(Oats.lme4, ~ranef(.,level = 2))

re4.0 = ranef(Oats.lme4.0, augFrame = TRUE)
boxplot(resid(Oats.lme4.0) ~ Oats.df$Block)
boxplot(re4.0$'(Intercept)' ~ re4.0$Variety)
qqnorm(Oats.lme4.0, ~resid(.)|Block)
qqnorm(Oats.lme4.0, ~ranef(.))

