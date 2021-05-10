#Model non-parametric : box plot of parameter d
#setwd("~appendix")
library(MASS)
ms <-readRDS(file = "result_nonparametric.obj")
#data
log_d_3.0 <- ms$lndp[, 1, 1:11, 1]
log_d_3.2 <- ms$lndp[, 2, 1:11, 1]
log_d_3.4 <- ms$lndp[, 3, 1:11, 1]
log_d_3.6 <- ms$lndp[, 4, 1:11, 1]
log_d_3.8 <- ms$lndp[, 5, 1:11, 1]
list = list(pH3.0 = log_d_3.0, 	
			pH3.2 = log_d_3.2, 
			pH3.4 = log_d_3.4, 
			pH3.6 = log_d_3.6, 
			pH3.8 = log_d_3.8)
#plot
pdf("fig/Fig.3_d.pdf", width = 4, height = 4)
par(ps = 12)
boxplot(list, ylab = expression(paste(ln(delta))), ylim = c(0, 7), las = 2)
dev.off()


#data
log_p_3.0 <- ms$lndp[, 1, 1:11, 2]
log_p_3.2 <- ms$lndp[, 2, 1:11, 2]
log_p_3.4 <- ms$lndp[, 3, 1:11, 2]
log_p_3.6 <- ms$lndp[, 4, 1:11, 2]
log_p_3.8 <- ms$lndp[, 5, 1:11, 2]
list = list(pH3.0 = log_p_3.0, 
			pH3.2 = log_p_3.2, 
			pH3.4 = log_p_3.4, 
			pH3.6 = log_p_3.6, 
			pH3.8 = log_p_3.8)
#plot
pdf("fig/Fig.3_p.pdf", width = 4, height = 4)
par(ps = 12)
boxplot(list, ylab = expression(paste(ln(p))), ylim = c(-2, 3), las = 2)
dev.off()