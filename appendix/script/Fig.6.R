#setwd("~appendix")
ms <-readRDS(file = "result_Model_para.obj")
pdf("fig/Fig.6.pdf",width=11,height=6.8)
par(mfrow=c(2,3),lwd=1.5,mar = c(4.5,4.5,1,1),oma=c(2,2,2,2),ps = 16)
library(MASS)
min <- 0.025
max <- 0.975
N_mcmc <- length(ms$lp__)

r_abef <- matrix(0,length(ms$lp__),4)
for(n in 1:length(ms$lp__))
{
	r_abef[n,] <- mvrnorm(1,mu=ms$abef0[n,], Sigma=ms$cov[n,,])
}


pH <- 3.3
set.seed(123)
rnum <- round(runif(10000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0,80,1)){
  a <- r_abef[,1]
  b <- r_abef[,2]
  e <- r_abef[,3]
  f <- r_abef[,4]
  d <- exp(a*pH+b)
  p <- exp(e*pH+f)
  I <- rnorm(10000, ms$logN0[rnum], ms$sigma_logN0[rnum])
  y_base <- I -(i/d)^p
  y <- rnorm(10000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs=min)
  min_y <- quantile(y, probs=min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs=max)
  max_y_base <- quantile(y_base, probs=max)
  max_y <- as.numeric(max_y)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y_base)
  d_mcmc <- data.frame(time=i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
}

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d,d$pH==3.3)
num <- c(0,15,1,16,2,17,5,18,3,4,6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[,1], alld_mcmc[,4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,80), yaxt="n",main="pH 3.3")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)



r_abef <- matrix(0,length(ms$lp__),4)
for(n in 1:length(ms$lp__))
{
	r_abef[n,] <- mvrnorm(1,mu=ms$abef0[n,], Sigma=ms$cov[n,,])
}


pH <- 3.5
set.seed(123)
rnum <- round(runif(10000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0,150,1)){
  a <- r_abef[,1]
  b <- r_abef[,2]
  e <- r_abef[,3]
  f <- r_abef[,4]
  d <- exp(a*pH+b)
  p <- exp(e*pH+f)
  I <- rnorm(10000, ms$logN0[rnum], ms$sigma_logN0[rnum])
  y_base <- I -(i/d)^p
  y <- rnorm(10000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs=min)
  min_y <- quantile(y, probs=min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs=max)
  max_y_base <- quantile(y_base, probs=max)
  max_y <- as.numeric(max_y)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y_base)
  d_mcmc <- data.frame(time=i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
}

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d,d$pH==3.5)
num <- c(0,15,1,16,2,17,5,18,3,4,6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[,1], alld_mcmc[,4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,150), yaxt="n",main="pH 3.5")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)
dev.off()
