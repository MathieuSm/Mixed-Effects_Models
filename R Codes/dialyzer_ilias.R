library(nlme)

dialyzer.df <- data.frame(Dialyzer)
dialyzer.df$Subject <- factor(as.numeric(levels(dialyzer.df$Subject))[dialyzer.df$Subject], ordered = FALSE)

plot(dialyzer.df$pressure, dialyzer.df$rate, type='n')
for (s.index in 1:length(unique(dialyzer.df$Subject))) {
  s <- unique(dialyzer.df$Subject)[s.index]
  lines(dialyzer.df$pressure[dialyzer.df$Subject==s], dialyzer.df$rate[dialyzer.df$Subject==s], 
    col=ifelse(dialyzer.df$QB[dialyzer.df$Subject==s][1]==200, 'blue', 'red'), type='o')
}
legend('bottomright', lty=1, col=c('blue', 'red'), c('QB = 200', 'QB = 300'))


###################
# P&B, p. 214-221 #
###################

dialyzer.lme.1 <- lme(rate ~ (pressure + I(pressure^2) + 
                              I(pressure^3) + I(pressure^4))*QB, 
  random = ~ pressure + I(pressure^2) | Subject, data=dialyzer.df)
# for the results in the book:
# contrasts=list(QB='contr.sum')
dialyzer.lme.1
plot(dialyzer.lme.1, resid(.) ~ pressure, grid=FALSE, abline=0, lty='dotted')


# introduce heteroscedasticity (some power of pressure)
dialyzer.lme.2 <- lme(rate ~ (pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB, 
  random = ~ pressure + I(pressure^2) | Subject, 
  weights = varPower(form = ~ pressure), data=dialyzer.df)
dialyzer.lme.2

anova(dialyzer.lme.1, dialyzer.lme.2)

plot(dialyzer.lme.2, resid(., type='pearson') ~ pressure, grid=FALSE, abline=0, lty='dotted')

intervals(dialyzer.lme.2)  # different from P&B p. 218


plot(dialyzer.lme.2, resid(.) ~ pressure | QB, grid=FALSE, abline=0, lty='dotted')

# more flexible heteroscedastic model (some power of pressure, strata: QB)
dialyzer.lme.3 <- lme(rate ~ (pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB, 
  random = ~ pressure + I(pressure^2) | Subject, 
  weights = varPower(form = ~ pressure | QB), data=dialyzer.df)
dialyzer.lme.3

anova(dialyzer.lme.2, dialyzer.lme.3)  # stratification not needed


# pressure may be close to 0 ==> try varConstPower instead
dialyzer.lme.4 <- lme(rate ~ (pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB, 
  random = ~ pressure + I(pressure^2) | Subject, 
  weights = varConstPower(form = ~ pressure), data=dialyzer.df)

anova(dialyzer.lme.2, dialyzer.lme.4)  # constant not needed



plot(augPred(dialyzer.lme.2, primary = ~ pressure))



anova(dialyzer.lme.2)

anova(dialyzer.lme.2, Terms = 8:10)



###########################
# Sheet 3, exercise 2 (b) #
###########################

dialyzer.lme.2
1.262599^2 * 2^(2*0.7493284)

# comparison of the fitted variance function to the residuals 
# and to their mean +/- sd per "group" of pressure
plot(dialyzer.df$pressure, resid(dialyzer.lme.2), xlim=c(0, 3.3), xaxs='i')
p <- seq(0, 3.5, length=200)
lines(p, sqrt(1.262599^2 * p^(2*0.7493284)))
lines(p, -sqrt(1.262599^2 * p^(2*0.7493284)))
lapply(split(cbind(dialyzer.df, resid=resid(dialyzer.lme.2)), cut(dialyzer.df$pressure, c(0, 0.4, seq(0.75, 3.25, 0.5)))), function(df) {
  points(rep(mean(df$pressure), 2), mean(df$resid) + c(-1, 1) * sd(df$resid), 
  pch='-', cex=2, col='red')
})


###########################
# Sheet 3, exercise 2 (c) #
###########################

dialyzer.lme.5 <- lme(rate ~ (pressure + I(pressure^2) + I(pressure^3) + I(pressure^4))*QB, 
  random = ~ pressure + I(pressure^2) | Subject, 
  weights = varExp(form = ~ pressure), data=dialyzer.df)
dialyzer.lme.5

# comparison of the fitted variance function to the residuals 
# and to their mean +/- sd per "group" of pressure
plot(dialyzer.df$pressure, resid(dialyzer.lme.5), xlim=c(0, 3.3), xaxs='i')
p <- seq(0, 3.5, length=100)
lines(p, sqrt(0.5937779^2 * exp(2*0.5975527*p)))
lines(p, -sqrt(0.5937779^2 * exp(2*0.5975527*p)))
lapply(split(cbind(dialyzer.df, resid=resid(dialyzer.lme.5)), cut(dialyzer.df$pressure, c(0, 0.4, seq(0.75, 3.25, 0.5)))), function(df) {
  points(rep(mean(df$pressure), 2), mean(df$resid) + c(-1, 1) * sd(df$resid), 
  pch='-', cex=2, col='red')
})

anova(dialyzer.lme.2, dialyzer.lme.5)

