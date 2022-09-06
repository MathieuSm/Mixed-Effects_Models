library(nlme)

pixel.df <- data.frame(Pixel)
pixel.df$Dog <- factor(as.numeric(levels(pixel.df$Dog))[pixel.df$Dog], ordered = FALSE)

pixel.lme.1 <- lme(pixel ~ day + I(day^2), random = list(Dog = ~ day, Side = ~ 1), data=pixel.df)
summary(pixel.lme.1)
VarCorr(pixel.lme.1)

library(lme4)
pixel.lmer <- lmer(pixel ~ day + I(day^2) + (day|Dog) + (1|Side:Dog), data=pixel.df)
X = getME(pixel.lmer,"X")
Z = getME(pixel.lmer,"Z")

printSpMatrix(Z)
