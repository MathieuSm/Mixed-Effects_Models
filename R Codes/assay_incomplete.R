library(nlme)

# create a data frame from the grouped data object, omitting the ordering of blocks
assay.df <- data.frame(Assay)
assay.df$Block <- factor(as.numeric(levels(assay.df$Block))[assay.df$Block], ordered = FALSE)

if (FALSE) {
postscript(file = "U:/vock/lehre/mixed_models/tex/assay.eps", onefile=TRUE,
  paper="special", width=7, height=5, family="Times", horizontal=FALSE)
par(oma=c(0,0,0,0), mar=c(3,3,0.2,0.2), mgp=c(1.8, 0.7, 0), cex=1.2)
plot(as.numeric(assay.df$dilut), assay.df$logDens, type='n', xlab='dilut', ylab='logDens')
for (b in levels(assay.df$Block)) {
  for (s in levels(assay.df$sample)) {
    with(assay.df[assay.df$sample==s & assay.df$Block==b,], 
      points(as.numeric(dilut), logDens, type='b', pch=as.character(sample), lty=as.numeric(b), cex=0.8)
    )
  }
}
legend('bottomright', lty=as.numeric(levels(assay.df$Block)), paste('Block', levels(assay.df$Block)))
dev.off()
} # end if (FALSE)



# illustration: plate, wells, blocks, rows, columns

# plate with 96 wells
par(mar=c(0.5, 5, 3, 5))
plot(0, 0, type='n', xlab='', ylab='', axes=FALSE, xlim=c(0.5, 12.5), ylim=c(0.5, 8.5))
axis(3, at=1:12, tick=FALSE)
axis(2, at=1:8, rev(LETTERS[1:8]), tick=FALSE, las=2)
points(rep(1:12, each=8), rep(1:8, 12), cex=3)
box()

# unused wells
points(rep(1:12, c(8, rep(2, 10), 8)), c(1:8, rep(c(1, 8), 10), 1:8), cex=3, col='grey')

# blocks
for (i in 1:2) {
  rect(1.55 + (i-1)*5, 1.55, 6.45 + (i-1)*5, 7.45, border='blue')
  text(4 + (i-1)*5, 7.55, paste('block', i), col='blue', adj=c(0.5, 0))
}

# rows / samples
for (i in 1:2) {
  for (j in 1:6) {
    rect(1.6 + (i-1)*5, 0.6 + j, 6.4 + (i-1)*5, 1.4 + j, border='darkgreen')
  }
}
for (i in 1:2) {
  text(0.7 + (i-1)*11.6, 4.5, 'samples a, ..., f', xpd=TRUE, col='darkgreen', adj=c(2-i, 0.5))
  arrows(0.8 + (i-1)*11.4, seq(4.25, 4.75, 0.1), 1.4 + (i-1)*10.2, seq(2, 7, 1), length=0.1, col='darkgreen')
}

# columns / dilutions
for (i in 1:2) {
  for (k in 1:5) {
    rect(1.65 + (i-1)*5 + k-1, 1.65, 2.35 + (i-1)*5 + k-1, 7.35, border='red')
  }
}
for (i in 1:2) {
  text(4 + (i-1)*5, 0.7, 'dilutions 1, ..., 5', xpd=TRUE, col='red', adj=c(0.5, 1))
  arrows(seq(3.8, 4.2, 0.1) + (i-1)*5, 0.8, seq(2, 6, 1) + (i-1)*5, 1.4, length=0.1, col='red')
}




# model fitting

assay.lme.1 <- lme(logDens ~ sample * dilut, 
  random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdIdent(~ dilut - 1)))),
  data=assay.df)

anova(assay.lme.1)


# can't specify this directly in lme...:
assay.lme.1a <- lme(logDens ~ sample * dilut, 
  random = list(Block = ~ 1, sample = ~ 1, dilut = ~ 1),
  data=assay.df)



# simplify the fixed part

assay.lme.2 <- lme(logDens ~ sample + dilut, 
  random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdIdent(~ dilut - 1)))),
  data=assay.df)
assay.lme.3 <- lme(logDens ~ sample + as.numeric(dilut) + I(dilut==5) + as.factor(pmax(pmin(as.numeric(dilut), 4), 2)), 
  random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdIdent(~ dilut - 1)))),
  data=assay.df)
assay.lme.4 <- lme(logDens ~ sample + as.numeric(dilut) + I(dilut==5), 
  random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdIdent(~ dilut - 1)))),
  data=assay.df)

assay.lme.5 <- lme(logDens ~ sample + as.numeric(dilut) + I(dilut==5), 
  random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdCompSymm(~ dilut - 1)))),
  data=assay.df)




assay.lme.1.ml <- lme(logDens ~ sample * dilut, 
  random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdIdent(~ dilut - 1)))),
  method='ML', data=assay.df)
assay.lme.2.ml <- lme(logDens ~ sample + dilut, 
  random = list(Block = pdBlocked(list(pdIdent(~ 1), pdIdent(~ sample - 1), pdIdent(~ dilut - 1)))),
  method='ML', data=assay.df)
anova(assay.lme.1.ml, assay.lme.2.ml)
# gives a much lower p value than the conditional F test




########
# lme4 #
########

library(lme4)

assay.lmer.1 <- lmer(logDens ~ sample * dilut + (1 | Block) + (1 | Block:sample) + (1 | Block:dilut), 
  data=assay.df)
anova(assay.lmer.1)

detach('package:lme4')
