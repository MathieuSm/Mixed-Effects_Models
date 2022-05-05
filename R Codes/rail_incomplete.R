library(nlme)


# create a data frame from the grouped data object, omitting the ordering of rails
rail.df <- data.frame(Rail)
rail.df$Rail <- factor(as.numeric(levels(rail.df$Rail))[rail.df$Rail], ordered = FALSE)


# model with fixed effects

rail.lm.1 <- lm(travel ~ Rail - 1, data = rail.df)
rail.lm.1
summary(rail.lm.1)

rail.lm.0 <- lm(travel ~ 1, data = rail.df)
anova(rail.lm.0, rail.lm.1)
# same global test of the rail effect:
summary(lm(travel ~ Rail, data = rail.df))
anova(lm(travel ~ Rail, data = rail.df))
summary(aov(travel ~ Rail, data = rail.df))


# model with random effects

rail.lme <- lme(travel ~ 1, random = ~ 1 | Rail, data = rail.df)
summary(rail.lme)


detach(package:nlme)
library(lme4)
rail.lmer <- lmer(travel ~ 1 + (1 | Rail), data = rail.df)
summary(rail.lmer)
detach(package:lme4)
library(nlme)


# limiting values in Fig. 2.2 from P&B (middle part)
log(sqrt(sum(residuals(rail.lm.1)^2)))
log(sqrt(sum(residuals(rail.lm.0)^2)))

# limiting line in Fig. 2.2 from P&B (lower part)
theta <- seq(-5, 2, 0.1)
plot(theta, 6*theta-3*log(3), type='l')

# initial estimate, theta_0
abline(h=0, v=log(3)/2, lty='dotted')

# ML estimates
rail.lme.ml <- lme(travel ~ 1, random = ~ 1 | Rail, data = rail.df, method='ML')
summary(rail.lme.ml)
# "Delta hat" - maximizer in Fig. 2.1 from P&B (upper part):
rail.lme.ml$sigma/as.numeric(VarCorr(rail.lme.ml)[1,2])
# "theta hat" - maximizer in Fig. 2.2 from P&B (upper part):
log(rail.lme.ml$sigma/as.numeric(VarCorr(rail.lme.ml)[1,2]))

# shrinkage
coefficients(rail.lm.1) - mean(rail.df$travel)
ranef(rail.lme.ml)$'(Intercept)'

library(lme4)
lmer(travel ~ 1 + (1 | Rail), data = rail.df, REML=FALSE)
detach(package:lme4)


# calculate predictions of the random effects using the formula
Z <- matrix(rep(c(1, 0), c(3, 18)), nrow=18, ncol=6)
G.hat <- diag(22.62435^2, 6)
sigma.hat <- 4.020779
X <- matrix(1, nrow=18, ncol=1)
beta.hat <- 66.5
solve(t(Z) %*% Z + 1/sigma.hat^(-2) * solve(G.hat)) %*% t(Z) %*% (rail.df$travel - X %*% beta.hat)
solve(t(Z) %*% Z + sigma.hat^2 * solve(G.hat)) %*% t(Z) %*% (rail.df$travel - X %*% beta.hat)


library(lme4)
ranef(rail.lmer)
detach(package:lme4)



# gls

rail.gls.1 <- gls(travel ~ 1, correlation = corCompSymm(form = ~ 1 | Rail), data=rail.df)
summary(rail.gls.1)

rail.gls.2 <- gls(travel ~ 1, correlation = corSymm(form = ~ 1 | Rail), data=rail.df)
summary(rail.gls.2)

anova(rail.gls.1, rail.gls.2)

plot(as.numeric(levels(rail.df$Rail))[rail.df$Rail], rail.df$travel, ylim=c(20, 100), xlab='Rail', ylab='travel', pch=rep(c('A', 'B', 'C'), 6))
abline(h=rail.gls.2$coefficients)

residuals(rail.gls.2)
