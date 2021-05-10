#setwd("~appendix")
ms <-readRDS(file = "result_nonparametric.obj")
pdf("fig/Fig.4.pdf", width = 11, height = 6.8)
par(mfrow = c(2, 3), lwd = 1.5, mar = c(4.5, 4.5, 1, 1), oma = c(2, 2, 2, 2), ps = 16)
library(MASS)
min <- 0.025
max <- 0.975
N_mcmc <- length(ms$lp__)



pH <- 3.0
set.seed(123)
rnum <- round(runif(110000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0, 15, 1)){
  d <- c(ms$d[,1,1], ms$d[,1,2], ms$d[,1,3], ms$d[,1,4], ms$d[,1,5], ms$d[,1,6], ms$d[,1,7], ms$d[,1,8], ms$d[,1,9], ms$d[,1,10], ms$d[,1,11])
  p <- c(ms$p[,1,1], ms$p[,1,2], ms$p[,1,3], ms$p[,1,4], ms$p[,1,5], ms$p[,1,6], ms$p[,1,7], ms$p[,1,8] ,ms$p[,1,9], ms$p[,1,10], ms$p[,1,11])
  I <- c(ms$logN0[,1], ms$logN0[,2], ms$logN0[,3], ms$logN0[,4], ms$logN0[,5], ms$logN0[,6], ms$logN0[,7], ms$logN0[,8], ms$logN0[,9], ms$logN0[,10], ms$logN0[,11])
  y_base <- I -(i/d)^p
  y <- rnorm(110000, y_base, ms$sigma_logNt[rnum])           
  min_y_base <- quantile(y_base, probs = min)
  min_y_base <- as.numeric(min_y_base)
  min_y <- quantile(y, probs = min)
  min_y <- as.numeric(min_y)
  max_y <- quantile(y, probs = max)
  max_y <- as.numeric(max_y)
  max_y_base <- quantile(y_base, probs = max)
  max_y_base <- as.numeric(max_y_base)
  med_y <- median(y)
  d_mcmc <- data.frame(time = i, min_y, max_y, med_y, min_y_base, max_y_base)
  alld_mcmc <- rbind(alld_mcmc, d_mcmc)
  }

#data
d <- read.csv("data/All_dataset0.csv")
d <- subset(d, d$pH == 3.0)
num <- c(0, 15, 1, 16, 2, 17, 5, 18, 3, 4, 6)

#plot
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc[, 1], alld_mcmc[, 4], type="l", xlab="Time (min)", ylab="Survival cell count (log CFU/mL)",
     lwd = 2, cex.lab = 1.2, cex.axis = 1.1, ylim = c(0, 6.2), xlim = c(0, 15), yaxt = "n", main = "pH 3.0")
axis(2, at = seq(-1, 7, 1), las = 1, cex.lab = 2, cex.axis = 1.1)
points(d$time, d$logN, pch = num[d$strain], cex = 2, lwd = 2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)




pH <- 3.2
set.seed(123)
rnum <- round(runif(110000, 0.5, N_mcmc+0.4999999))
alld_mcmc <- NULL
for(i in seq(0,30,2)){
  d <- c(ms$d[,2,1],ms$d[,2,2],ms$d[,2,3],ms$d[,2,4],ms$d[,2,5],ms$d[,2,6],ms$d[,2,7],ms$d[,2,8],ms$d[,2,9],ms$d[,2,10],ms$d[,2,11])
  p <- c(ms$p[,2,1],ms$p[,2,2],ms$p[,2,3],ms$p[,2,4],ms$p[,2,5],ms$p[,2,6],ms$p[,2,7],ms$p[,2,8],ms$p[,2,9],ms$p[,2,10],ms$p[,2,11])  
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
  d <- c(ms$d[,3,1],ms$d[,3,2],ms$d[,3,3],ms$d[,3,4],ms$d[,3,5],ms$d[,3,6],ms$d[,3,7],ms$d[,3,8],ms$d[,3,9],ms$d[,3,10],ms$d[,3,11])
  p <- c(ms$p[,3,1],ms$p[,3,2],ms$p[,3,3],ms$p[,3,4],ms$p[,3,5],ms$p[,3,6],ms$p[,3,7],ms$p[,3,8],ms$p[,3,9],ms$p[,3,10],ms$p[,3,11])  
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
  d <- c(ms$d[,4,1],ms$d[,4,2],ms$d[,4,3],ms$d[,4,4],ms$d[,4,5],ms$d[,4,6],ms$d[,4,7],ms$d[,4,8],ms$d[,4,9],ms$d[,4,10],ms$d[,4,11])
  p <- c(ms$p[,4,1],ms$p[,4,2],ms$p[,4,3],ms$p[,4,4],ms$p[,4,5],ms$p[,4,6],ms$p[,4,7],ms$p[,4,8],ms$p[,4,9],ms$p[,4,10],ms$p[,4,11])  
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
  d <- c(ms$d[,5,1],ms$d[,5,2],ms$d[,5,3],ms$d[,5,4],ms$d[,5,5],ms$d[,5,6],ms$d[,5,7],ms$d[,5,8],ms$d[,5,9],ms$d[,5,10],ms$d[,5,11])
  p <- c(ms$p[,5,1],ms$p[,5,2],ms$p[,5,3],ms$p[,5,4],ms$p[,5,5],ms$p[,5,6],ms$p[,5,7],ms$p[,5,8],ms$p[,5,9],ms$p[,5,10],ms$p[,5,11])  
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
     lwd=2, cex.lab=1.2, cex.axis=1.1, ylim=c(0, 6.2), xlim=c(0,250), yaxt="n",main="pH 3.8")
axis(2,at=seq(-1,7,1),las=1,cex.lab=2,cex.axis=1.1)
points(d$time, d$logN, pch=num[d$strain], cex=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,2], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,3], type="l", lty=2, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,5], type="l", lty=3, lwd=2)
points(alld_mcmc[,1], alld_mcmc[,6], type="l", lty=3, lwd=2)

dev.off()