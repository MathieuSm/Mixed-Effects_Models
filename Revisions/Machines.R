library(nlme)

# create a data frame from the grouped data object, omitting the ordering of workers
machines.df <- data.frame(Machines)
machines.df$Worker <- factor(as.numeric(levels(machines.df$Worker))[machines.df$Worker], ordered = FALSE)

library(lme4)
machines.lmer.2 <- lmer(score ~ Machine + (1 | Worker/Machine), data=machines.df)
machines.lmer.2

X = getME(machines.lmer.2,"X")
Z = getME(machines.lmer.2,"Z")

printSpMatrix(X)