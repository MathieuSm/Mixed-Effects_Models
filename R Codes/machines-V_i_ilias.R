library(nlme)

machines.df <- data.frame(Machines)
machines.df$Worker <- factor(as.numeric(levels(machines.df$Worker))[machines.df$Worker], ordered = FALSE)

machines.lme.1 <- lme(score ~ Machine, random = ~ 1 | Worker, 
  data=machines.df)
machines.lme.2 <- lme(score ~ Machine, random = ~ 1 | Worker/Machine, 
  data=machines.df)
machines.lme.3 <- lme(score ~ Machine, random = ~ Machine - 1 | Worker, 
  data=machines.df)



# individual variance/covariance matrix of the single-level model machines.lme.1
V.i.1 <- matrix(rep(5.146552^2, 81), nrow=9) + 
  diag(rep(3.161647^2), 9)

round(V.i.1, 1)
# or: getVarCov(machines.lme.1, type='marginal')
# or: mgcv::extract.lme.cov2(machines.lme.1)$V[[1]]

# individual variance/covariance matrix of the two-level model machines.lme.2
V.i.2 <- matrix(rep(4.78105^2, 81), nrow=9) + 
  kronecker(diag(rep(3.729532^2, 3)), matrix(rep(1,9), nrow=3)) +
  diag(rep(0.9615771^2), 9)

round(V.i.2, 1)
# or: mgcv::extract.lme.cov2(machines.lme.2)$V[[1]]


# individual variance/covariance matrix for machines.lme.3

vc <- VarCorr(machines.lme.3)
corr <- matrix(as.numeric(vc[-c(1, nrow(vc)),3:ncol(vc)]), nrow=nrow(vc)-2, ncol=nrow(vc)-2)
corr <- rbind(rep(0, ncol(corr)+1), cbind(corr, rep(0, nrow(corr))))
corr <- corr + t(corr)
diag(corr) <- 1
sdev <- as.numeric(vc[-nrow(vc),'StdDev'])
Psi <- outer(sdev, sdev) * corr  # some rounding error, mainly from the correlations...
sigma <- as.numeric(vc[nrow(vc), 'StdDev'])
Z.i <- model.matrix(score ~ Machine - 1, data=machines.df[machines.df$Worker==1,])
V.i.3 <- Z.i %*% Psi %*% t(Z.i) + diag(rep(sigma^2, nrow(Z.i)))
dimnames(V.i.3) <- NULL

round(V.i.3, 1)
# or: getVarCov(machines.lme.3, type='marginal')
# or: mgcv::extract.lme.cov2(machines.lme.3)$V[[1]]


# variance-covariance matrix of y_i - X_i beta-hat
V.i.0 <- var(matrix((machines.df$score-fitted.values(lm(score ~ Machine, data=machines.df)))[order(machines.df$Worker)], ncol=9, byrow=TRUE))

round(V.i.0, 1)




round(V.i.3 - V.i.0, 1)



if (FALSE) {
pdf('c:/vock/mixed_models/machines-V_i.pdf', width=7, height=6, family='Times')
#b <- sort(unique(c(as.numeric(V.i.1), as.numeric(V.i.2), as.numeric(V.i.3), as.numeric(V.i.0), 17, 20, 37, 70)))
b <- exp(seq(log(min(V.i.0)), log(max(V.i.0)), length=100))
#b <- seq(min(V.i.0), max(V.i.0), length=100)
b <- (c(min(b)-1, b) + c(b, max(b)+1))/2
co <- rev(heat.colors(length(b)-1))
#co <- rainbow(length(b)-1, start=0, end=4/6)
#co <- rgb(c(rep(0,29), seq(0, 1, length=30)), c(seq(1, 0.5, length=30), seq(0.53, 1, length=29)), c(seq(1, 0, length=30), rep(0, 29)))
#co <- rgb(seq(1, 0, length=63), seq(1, 0, length=63), seq(1, 0, length=63))
#par(mfrow=c(2,2), mar=c(0, 2, 3.5, 0), mgp=c(1.8, 0.7, 0))
for (borders in 0:1) {
par(fig=c(0, 0.42, 0.5, 1), mar=c(0.5, 2, 3, 0), mgp=c(1.8, 0.7, 0), cex=0.75)
image(1:9, 1:9, V.i.1, zlim=range(V.i.0), ylim=c(9.5, 0.5), col=co, breaks=b, axes=FALSE, xlab='', ylab='')
axis(2, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
axis(3, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
if (borders) {
  segments(0.5:8.5, 0.5:8.5, 1.5:9.5, 0.5:8.5)
  segments(0.5:8.5, 1.5:9.5, 1.5:9.5, 1.5:9.5)
  segments(0.5:8.5, 0.5:8.5, 0.5:8.5, 1.5:9.5)
  segments(1.5:9.5, 0.5:8.5, 1.5:9.5, 1.5:9.5)
}
box()
mtext('machines.lme.1', side = 3, line = 1.8)
par(fig=c(0.42, 0.84, 0.5, 1), new=TRUE)
image(1:9, 1:9, V.i.2, zlim=range(V.i.0), ylim=c(9.5, 0.5), col=co, breaks=b, axes=FALSE, xlab='', ylab='')
axis(2, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
axis(3, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
if (borders) {
  segments(0.5:8.5, 0.5:8.5, 1.5:9.5, 0.5:8.5)
  segments(0.5:8.5, 1.5:9.5, 1.5:9.5, 1.5:9.5)
  segments(0.5:8.5, 0.5:8.5, 0.5:8.5, 1.5:9.5)
  segments(1.5:9.5, 0.5:8.5, 1.5:9.5, 1.5:9.5)
  segments(seq(0.5, 6.5, 3), seq(0.5, 6.5, 3), seq(3.5, 9.5, 3), seq(0.5, 6.5, 3))
  segments(seq(0.5, 6.5, 3), seq(3.5, 9.5, 3), seq(3.5, 9.5, 3), seq(3.5, 9.5, 3))
  segments(seq(0.5, 6.5, 3), seq(0.5, 6.5, 3), seq(0.5, 6.5, 3), seq(3.5, 9.5, 3))
  segments(seq(3.5, 9.5, 3), seq(0.5, 6.5, 3), seq(3.5, 9.5, 3), seq(3.5, 9.5, 3))
}
box()
mtext('machines.lme.2', side = 3, line = 1.8)
par(fig=c(0, 0.42, 0, 0.5), new=TRUE)
image(1:9, 1:9, V.i.3, zlim=range(V.i.0), ylim=c(9.5, 0.5), col=co, breaks=b, axes=FALSE, xlab='', ylab='')
axis(2, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
axis(3, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
if (borders) {
  segments(0.5:8.5, 0.5:8.5, 1.5:9.5, 0.5:8.5)
  segments(0.5:8.5, 1.5:9.5, 1.5:9.5, 1.5:9.5)
  segments(0.5:8.5, 0.5:8.5, 0.5:8.5, 1.5:9.5)
  segments(1.5:9.5, 0.5:8.5, 1.5:9.5, 1.5:9.5)
  segments(seq(0.5, 9.5, 3), 0.5, seq(0.5, 9.5, 3), 9.5)
  segments(0.5, seq(0.5, 9.5, 3), 9.5, seq(0.5, 9.5, 3))
}
box()
mtext('machines.lme.3', side = 3, line = 1.8)
par(fig=c(0.42, 0.84, 0, 0.5), new=TRUE)
image(1:9, 1:9, V.i.0, zlim=range(V.i.0), ylim=c(9.5, 0.5), col=co, breaks=b, axes=FALSE, xlab='', ylab='')
axis(2, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
axis(3, at=1:9, labels=c('A1', 'A2', 'A3', 'B1', 'B2', 'B3', 'C1', 'C2', 'C3'))
if (borders) {
  segments(seq(0.5, 9.5, 1), 0.5, seq(0.5, 9.5, 1), 9.5)
  segments(0.5, seq(0.5, 9.5, 1), 9.5, seq(0.5, 9.5, 1))
}
box()
mtext('unstructured', side = 3, line = 1.8)
par(fig=c(0.84, 1, 0, 1), new=TRUE,  mar=c(0.5, 2, 3, 3))
plot(0, 0, type='n', xlim=c(0,1), ylim=c(0, 100), xlab='', ylab='', axes=FALSE, xaxs='i', yaxs='i')
axis(4)
mtext('Covariance', side=4, line=1.5)
for (i in 1:length(co)) {
  rect(0, b[i], 1, b[i+1], col=co[i], border=NA)
}
rect(0, b[length(b)], 1, 100, col=co[length(co)], border=NA)
box()
} # for borders
dev.off()
}
