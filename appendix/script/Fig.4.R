#setwd("~appendix")
ms <-readRDS(file = "result_Model_para.obj")
pdf("fig/Fig.4.pdf",width=11,height=6.8)
par(mfrow=c(2,3),lwd=1.5,mar = c(4.5,4.5,1,1),oma=c(2,2,2,2),ps = 16)
library(MASS)
min <- 0.025
max <- 0.975
N_mcmc <- length(ms$lp__)



pH <- 3.0
set.seed(123)
rnum <- round(runif(110000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0,15,1)){
  a <- c(ms$a[,1],ms$a[,2],ms$a[,3],ms$a[,4],ms$a[,5],ms$a[,6],ms$a[,7],ms$a[,8],ms$a[,9],ms$a[,10],ms$a[,11])
  b <- c(ms$b[,1],ms$b[,2],ms$b[,3],ms$b[,4],ms$b[,5],ms$b[,6],ms$b[,7],ms$b[,8],ms$b[,9],ms$b[,10],ms$b[,11])
  e <- c(ms$e[,1],ms$e[,2],ms$e[,3],ms$e[,4],ms$e[,5],ms$e[,6],ms$e[,7],ms$e[,8],ms$e[,9],ms$e[,10],ms$e[,11])
  f <- c(ms$f[,1],ms$f[,2],ms$f[,3],ms$f[,4],ms$f[,5],ms$f[,6],ms$f[,7],ms$f[,8],ms$f[,9],ms$f[,10],ms$f[,11])
  d <- exp(a*pH+b)
  p <- exp(e*pH+f)
  I <- c(ms$logN0[,1],ms$logN0[,2],ms$logN0[,3],ms$logN0[,4],ms$logN0[,5],ms$logN0[,6],ms$logN0[,7],ms$logN0[,8],ms$logN0[,9],ms$logN0[,10],ms$logN0[,11])
  y_base <- I -(i/d)^p
  y <- rnorm(110000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs=min)
  min_y <- quantile(y, probs=min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs=max)
  max_y_base <- quantile(y_base, probs=max)
  max_y <- as.numeric(max_y)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y)
  d_mcmc <- data.frame(time=i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
}

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d,d$pH==3.0)
num <- c(0,15,1,16,2,17,5,18,3,4,6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[,1], alld_mcmc[,4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,15), yaxt="n",main="pH 3.0")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)


pH <- 3.2
set.seed(123)
rnum <- round(runif(110000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0,30,2)){
  a <- c(ms$a[,1],ms$a[,2],ms$a[,3],ms$a[,4],ms$a[,5],ms$a[,6],ms$a[,7],ms$a[,8],ms$a[,9],ms$a[,10],ms$a[,11])
  b <- c(ms$b[,1],ms$b[,2],ms$b[,3],ms$b[,4],ms$b[,5],ms$b[,6],ms$b[,7],ms$b[,8],ms$b[,9],ms$b[,10],ms$b[,11])
  e <- c(ms$e[,1],ms$e[,2],ms$e[,3],ms$e[,4],ms$e[,5],ms$e[,6],ms$e[,7],ms$e[,8],ms$e[,9],ms$e[,10],ms$e[,11])
  f <- c(ms$f[,1],ms$f[,2],ms$f[,3],ms$f[,4],ms$f[,5],ms$f[,6],ms$f[,7],ms$f[,8],ms$f[,9],ms$f[,10],ms$f[,11])
  d <- exp(a*pH+b)
  p <- exp(e*pH+f)
  I <- c(ms$logN0[,1],ms$logN0[,2],ms$logN0[,3],ms$logN0[,4],ms$logN0[,5],ms$logN0[,6],ms$logN0[,7],ms$logN0[,8],ms$logN0[,9],ms$logN0[,10],ms$logN0[,11])
  y_base <- I -(i/d)^p
  y <- rnorm(110000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs=min)
  min_y <- quantile(y, probs=min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs=max)
  max_y_base <- quantile(y_base, probs=max)
  max_y <- as.numeric(max_y)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y)
  d_mcmc <- data.frame(time=i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
}

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d,d$pH==3.2)
num <- c(0,15,1,16,2,17,5,18,3,4,6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[,1], alld_mcmc[,4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,30), yaxt="n",main="pH 3.2")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)


pH <- 3.4
set.seed(123)
rnum <- round(runif(110000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL


for(i in seq(0,120,8)){
  a <- c(ms$a[,1],ms$a[,2],ms$a[,3],ms$a[,4],ms$a[,5],ms$a[,6],ms$a[,7],ms$a[,8],ms$a[,9],ms$a[,10],ms$a[,11])
  b <- c(ms$b[,1],ms$b[,2],ms$b[,3],ms$b[,4],ms$b[,5],ms$b[,6],ms$b[,7],ms$b[,8],ms$b[,9],ms$b[,10],ms$b[,11])
  e <- c(ms$e[,1],ms$e[,2],ms$e[,3],ms$e[,4],ms$e[,5],ms$e[,6],ms$e[,7],ms$e[,8],ms$e[,9],ms$e[,10],ms$e[,11])
  f <- c(ms$f[,1],ms$f[,2],ms$f[,3],ms$f[,4],ms$f[,5],ms$f[,6],ms$f[,7],ms$f[,8],ms$f[,9],ms$f[,10],ms$f[,11])
  d <- exp(a*pH+b)
  p <- exp(e*pH+f)
  I <- c(ms$logN0[,1],ms$logN0[,2],ms$logN0[,3],ms$logN0[,4],ms$logN0[,5],ms$logN0[,6],ms$logN0[,7],ms$logN0[,8],ms$logN0[,9],ms$logN0[,10],ms$logN0[,11])
  y_base <- I -(i/d)^p
  y <- rnorm(110000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs=min)
  min_y <- quantile(y, probs=min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs=max)
  max_y_base <- quantile(y_base, probs=max)
  max_y <- as.numeric(max_y)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y)
  d_mcmc <- data.frame(time=i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
}

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d,d$pH==3.4)
num <- c(0,15,1,16,2,17,5,18,3,4,6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[,1], alld_mcmc[,4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,120), yaxt="n",main="pH 3.4")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)


pH <- 3.6
set.seed(123)
rnum <- round(runif(110000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0,200,10)){
  a <- c(ms$a[,1],ms$a[,2],ms$a[,3],ms$a[,4],ms$a[,5],ms$a[,6],ms$a[,7],ms$a[,8],ms$a[,9],ms$a[,10],ms$a[,11])
  b <- c(ms$b[,1],ms$b[,2],ms$b[,3],ms$b[,4],ms$b[,5],ms$b[,6],ms$b[,7],ms$b[,8],ms$b[,9],ms$b[,10],ms$b[,11])
  e <- c(ms$e[,1],ms$e[,2],ms$e[,3],ms$e[,4],ms$e[,5],ms$e[,6],ms$e[,7],ms$e[,8],ms$e[,9],ms$e[,10],ms$e[,11])
  f <- c(ms$f[,1],ms$f[,2],ms$f[,3],ms$f[,4],ms$f[,5],ms$f[,6],ms$f[,7],ms$f[,8],ms$f[,9],ms$f[,10],ms$f[,11])
  d <- exp(a*pH+b)
  p <- exp(e*pH+f)
  I <- c(ms$logN0[,1],ms$logN0[,2],ms$logN0[,3],ms$logN0[,4],ms$logN0[,5],ms$logN0[,6],ms$logN0[,7],ms$logN0[,8],ms$logN0[,9],ms$logN0[,10],ms$logN0[,11])
  y_base <- I -(i/d)^p
  y <- rnorm(110000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs=min)
  min_y <- quantile(y, probs=min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs=max)
  max_y_base <- quantile(y_base, probs=max)
  max_y <- as.numeric(max_y)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y)
  d_mcmc <- data.frame(time=i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
}

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d,d$pH==3.6)
num <- c(0,15,1,16,2,17,5,18,3,4,6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[,1], alld_mcmc[,4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,200), yaxt="n",main="pH 3.6")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)


pH <- 3.8
set.seed(123)
rnum <- round(runif(110000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0,400,20)){
  a <- c(ms$a[,1],ms$a[,2],ms$a[,3],ms$a[,4],ms$a[,5],ms$a[,6],ms$a[,7],ms$a[,8],ms$a[,9],ms$a[,10],ms$a[,11])
  b <- c(ms$b[,1],ms$b[,2],ms$b[,3],ms$b[,4],ms$b[,5],ms$b[,6],ms$b[,7],ms$b[,8],ms$b[,9],ms$b[,10],ms$b[,11])
  e <- c(ms$e[,1],ms$e[,2],ms$e[,3],ms$e[,4],ms$e[,5],ms$e[,6],ms$e[,7],ms$e[,8],ms$e[,9],ms$e[,10],ms$e[,11])
  f <- c(ms$f[,1],ms$f[,2],ms$f[,3],ms$f[,4],ms$f[,5],ms$f[,6],ms$f[,7],ms$f[,8],ms$f[,9],ms$f[,10],ms$f[,11])
  d <- exp(a*pH+b)
  p <- exp(e*pH+f)
  I <- c(ms$logN0[,1],ms$logN0[,2],ms$logN0[,3],ms$logN0[,4],ms$logN0[,5],ms$logN0[,6],ms$logN0[,7],ms$logN0[,8],ms$logN0[,9],ms$logN0[,10],ms$logN0[,11])
  y_base <- I -(i/d)^p
  y <- rnorm(110000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs=min)
  min_y <- quantile(y, probs=min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs=max)
  max_y_base <- quantile(y_base, probs=max)
  max_y <- as.numeric(max_y)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y)
  d_mcmc <- data.frame(time=i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
}

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d,d$pH==3.8)
num <- c(0,15,1,16,2,17,5,18,3,4,6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[,1], alld_mcmc[,4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,400), yaxt="n",main="pH 3.8")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)

dev.off()