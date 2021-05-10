#Model non-parametric : box plot of parameter p
#setwd("~appendix")
ms <-readRDS(file = "result_Model_para.obj")

#data
list=list(
pH3.0=ms$e[,1]*3.0+ms$f[,1], pH3.2=ms$e[,1]*3.2+ms$f[,1], pH3.4=ms$e[,1]*3.4+ms$f[,1], pH3.6=ms$e[,1]*3.6+ms$f[,1], pH3.8=ms$e[,1]*3.8+ms$f[,1],
pH3.0=ms$e[,2]*3.0+ms$f[,2], pH3.2=ms$e[,2]*3.2+ms$f[,2], pH3.4=ms$e[,2]*3.4+ms$f[,2],  pH3.6=ms$e[,2]*3.6+ms$f[,2], pH3.8=ms$e[,2]*3.8+ms$f[,2],
pH3.0=ms$e[,3]*3.0+ms$f[,3], pH3.2=ms$e[,3]*3.2+ms$f[,3], pH3.4=ms$e[,3]*3.4+ms$f[,3], pH3.6=ms$e[,4]*3.6+ms$f[,4], pH3.8=ms$e[,3]*3.8+ms$f[,3],
pH3.0=ms$e[,4]*3.0+ms$f[,4], pH3.2=ms$e[,4]*3.2+ms$f[,4], pH3.4=ms$e[,4]*3.4+ms$f[,4], pH3.6=ms$e[,4]*3.6+ms$f[,4], pH3.8=ms$e[,4]*3.8+ms$f[,4],
pH3.0=ms$e[,5]*3.0+ms$f[,5], pH3.2=ms$e[,5]*3.2+ms$f[,5], pH3.4=ms$e[,5]*3.4+ms$f[,5], pH3.6=ms$e[,5]*3.6+ms$f[,5], pH3.8=ms$e[,5]*3.8+ms$f[,5],
pH3.0=ms$e[,6]*3.0+ms$f[,6], pH3.2=ms$e[,6]*3.2+ms$f[,6], pH3.4=ms$e[,6]*3.4+ms$f[,6], pH3.6=ms$e[,6]*3.6+ms$f[,6], pH3.8=ms$e[,6]*3.8+ms$f[,6],
pH3.0=ms$e[,7]*3.0+ms$f[,7], pH3.2=ms$e[,7]*3.2+ms$f[,7], pH3.4=ms$e[,7]*3.4+ms$f[,7], pH3.6=ms$e[,7]*3.6+ms$f[,7], pH3.8=ms$e[,7]*3.8+ms$f[,7],
pH3.0=ms$e[,8]*3.0+ms$f[,8], pH3.2=ms$e[,8]*3.2+ms$f[,8], pH3.4=ms$e[,8]*3.4+ms$f[,8], pH3.6=ms$e[,8]*3.6+ms$f[,8], pH3.8=ms$e[,8]*3.8+ms$f[,8],
pH3.0=ms$e[,9]*3.0+ms$f[,9], pH3.2=ms$e[,9]*3.2+ms$f[,9], pH3.4=ms$e[,9]*3.4+ms$f[,9], pH3.6=ms$e[,9]*3.6+ms$f[,9], pH3.8=ms$e[,9]*3.8+ms$f[,9],
pH3.0=ms$e[,10]*3.0+ms$f[,10], pH3.2=ms$e[,10]*3.2+ms$f[,10], pH3.4=ms$e[,10]*3.4+ms$f[,10], pH3.6=ms$e[,10]*3.6+ms$f[,10], pH3.8=ms$e[,10]*3.8+ms$f[,10],
pH3.0=ms$e[,11]*3.0+ms$f[,11], pH3.2=ms$e[,11]*3.2+ms$f[,11], pH3.4=ms$e[,11]*3.4+ms$f[,11], pH3.6=ms$e[,11]*3.6+ms$f[,11], pH3.8=ms$e[,11]*3.8+ms$f[,11]
)

#plot
pdf("fig/Fig.7_p.pdf",width=14,height=4)
par(ps = 12)
boxplot(list,ylab=expression(paste(ln(p))),ylim=c(-2,3),las=2)
dev.off()