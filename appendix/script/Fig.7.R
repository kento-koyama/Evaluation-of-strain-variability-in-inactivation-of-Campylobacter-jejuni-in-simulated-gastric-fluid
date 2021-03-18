#setwd("~appendix")
ms <-readRDS(file = "result_Model_para.obj")

library(MASS)
min <- 0.05
max <- 0.95
#ms <- rstan::extract(cam_allpH_stan)
N_mcmc <- length(ms$lp__)
set.seed(123)
rnum <- round(runif(3000, 0.5, N_mcmc+0.4999999))
alld_mcmc_ta <- NULL
alld_mcmc_tb <- NULL
alld_mcmc_te <- NULL
alld_mcmc_tf <- NULL
for(j in seq(-20.5, 20.4, 0.1)){
  ta <- NULL
  tb <- NULL
  te <- NULL
  tf <- NULL
  for(i in rnum){
    a <- dnorm(j,ms$abef0[,1][i],ms$sigma_vec[,1][i])
    b <- dnorm(j,ms$abef0[,2][i],ms$sigma_vec[,2][i])
    e <- dnorm(j,ms$abef0[,3][i],ms$sigma_vec[,3][i])
    f <- dnorm(j,ms$abef0[,4][i],ms$sigma_vec[,4][i])
    ta <- rbind(ta, a)
    tb <- rbind(tb, b)
    te <- rbind(te, e)
    tf <- rbind(tf, f)
  }
  min_ta <- quantile(ta, probs=0.025)
  min_ta <- as.numeric(min_ta)
  max_ta <- quantile(ta, probs=0.975)
  max_ta <- as.numeric(max_ta)
  med_ta <- median(ta)
  d_mcmc_ta <- data.frame(j, min_ta, max_ta, med_ta)
  alld_mcmc_ta <- rbind(alld_mcmc_ta, d_mcmc_ta)
  min_tb <- quantile(tb, probs=0.025)
  min_tb <- as.numeric(min_tb)
  max_tb <- quantile(tb, probs=0.975)
  max_tb <- as.numeric(max_tb)
  med_tb <- median(tb)
  d_mcmc_tb <- data.frame(j, min_tb, max_tb, med_tb)
  alld_mcmc_tb <- rbind(alld_mcmc_tb, d_mcmc_tb)
  min_te <- quantile(te, probs=0.025)
  min_te <- as.numeric(min_te)
  max_te <- quantile(te, probs=0.975)
  max_te <- as.numeric(max_te)
  med_te <- median(te)
  d_mcmc_te <- data.frame(j, min_te, max_te, med_te)
  alld_mcmc_te <- rbind(alld_mcmc_te, d_mcmc_te)
  min_tf <- quantile(tf, probs=0.025)
  min_tf <- as.numeric(min_tf)
  max_tf <- quantile(tf, probs=0.975)
  max_tf <- as.numeric(max_tf)
  med_tf <- median(tf)
  d_mcmc_tf <- data.frame(j, min_tf, max_tf, med_tf)
  alld_mcmc_tf <- rbind(alld_mcmc_tf, d_mcmc_tf)
}

#plot
pdf("fig/Fig.7.pdf",width=11,height=6.8)
par(mfrow=c(2,3),lwd=1.5,mar = c(4.5,4.5,1,1),oma=c(2,2,2,2),ps = 16)
par(mar=c(5,5,2,2))
par(mgp=c(3.2,1,0))
plot(alld_mcmc_ta[,1], alld_mcmc_ta[,4], type="l", lwd=2, xlim=c(2,8),ylim=c(0,2.5),las=1,ylab="Density [-]", xlab="Parameter a")
points(alld_mcmc_ta[,1], alld_mcmc_ta[,2], type="l", lty=2, lwd=2)
points(alld_mcmc_ta[,1], alld_mcmc_ta[,3], type="l", lty=2, lwd=2)
plot(alld_mcmc_tb[,1], alld_mcmc_tb[,4], type="l", lwd=2, xlim=c(-20,-5),ylim=c(0,0.8),las=1,ylab="Density [-]", xlab="Parameter b")
points(alld_mcmc_tb[,1], alld_mcmc_tb[,2], type="l", lty=2, lwd=2)
points(alld_mcmc_tb[,1], alld_mcmc_tb[,3], type="l", lty=2, lwd=2)
plot(alld_mcmc_te[,1], alld_mcmc_te[,4], type="l", lwd=2, xlim=c(-4,4),ylim=c(0,2.0),las=1,ylab="Density [-]", xlab="Parameter e")
points(alld_mcmc_te[,1], alld_mcmc_te[,2], type="l", lty=2, lwd=2)
points(alld_mcmc_te[,1], alld_mcmc_te[,3], type="l", lty=2, lwd=2)
plot(alld_mcmc_tf[,1], alld_mcmc_tf[,4], type="l", lwd=2, xlim=c(-10,15),ylim=c(0,0.8),las=1,ylab="Density [-]", xlab="Parameter f")
points(alld_mcmc_tf[,1], alld_mcmc_tf[,2], type="l", lty=2, lwd=2)
points(alld_mcmc_tf[,1], alld_mcmc_tf[,3], type="l", lty=2, lwd=2)

dev.off()

