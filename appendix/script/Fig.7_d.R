#Model non-parametric : box plot of parameter d
#setwd("~appendix")
ms <-readRDS(file = "result_Model_para.obj")

#data
list=list(
pH3.0=ms$a[,1]*3.0+ms$b[,1], pH3.2=ms$a[,1]*3.2+ms$b[,1], pH3.4=ms$a[,1]*3.4+ms$b[,1], pH3.6=ms$a[,1]*3.6+ms$b[,1], pH3.8=ms$a[,1]*3.8+ms$b[,1],
pH3.0=ms$a[,2]*3.0+ms$b[,2], pH3.2=ms$a[,2]*3.2+ms$b[,2], pH3.4=ms$a[,2]*3.4+ms$b[,2],  pH3.6=ms$a[,2]*3.6+ms$b[,2], pH3.8=ms$a[,2]*3.8+ms$b[,2],
pH3.0=ms$a[,3]*3.0+ms$b[,3], pH3.2=ms$a[,3]*3.2+ms$b[,3], pH3.4=ms$a[,3]*3.4+ms$b[,3], pH3.6=ms$a[,4]*3.6+ms$b[,4], pH3.8=ms$a[,3]*3.8+ms$b[,3],
pH3.0=ms$a[,4]*3.0+ms$b[,4], pH3.2=ms$a[,4]*3.2+ms$b[,4], pH3.4=ms$a[,4]*3.4+ms$b[,4], pH3.6=ms$a[,4]*3.6+ms$b[,4], pH3.8=ms$a[,4]*3.8+ms$b[,4],
pH3.0=ms$a[,5]*3.0+ms$b[,5], pH3.2=ms$a[,5]*3.2+ms$b[,5], pH3.4=ms$a[,5]*3.4+ms$b[,5], pH3.6=ms$a[,5]*3.6+ms$b[,5], pH3.8=ms$a[,5]*3.8+ms$b[,5],
pH3.0=ms$a[,6]*3.0+ms$b[,6], pH3.2=ms$a[,6]*3.2+ms$b[,6], pH3.4=ms$a[,6]*3.4+ms$b[,6], pH3.6=ms$a[,6]*3.6+ms$b[,6], pH3.8=ms$a[,6]*3.8+ms$b[,6],
pH3.0=ms$a[,7]*3.0+ms$b[,7], pH3.2=ms$a[,7]*3.2+ms$b[,7], pH3.4=ms$a[,7]*3.4+ms$b[,7], pH3.6=ms$a[,7]*3.6+ms$b[,7], pH3.8=ms$a[,7]*3.8+ms$b[,7],
pH3.0=ms$a[,8]*3.0+ms$b[,8], pH3.2=ms$a[,8]*3.2+ms$b[,8], pH3.4=ms$a[,8]*3.4+ms$b[,8], pH3.6=ms$a[,8]*3.6+ms$b[,8], pH3.8=ms$a[,8]*3.8+ms$b[,8],
pH3.0=ms$a[,9]*3.0+ms$b[,9], pH3.2=ms$a[,9]*3.2+ms$b[,9], pH3.4=ms$a[,9]*3.4+ms$b[,9], pH3.6=ms$a[,9]*3.6+ms$b[,9], pH3.8=ms$a[,9]*3.8+ms$b[,9],
pH3.0=ms$a[,10]*3.0+ms$b[,10], pH3.2=ms$a[,10]*3.2+ms$b[,10], pH3.4=ms$a[,10]*3.4+ms$b[,10], pH3.6=ms$a[,10]*3.6+ms$b[,10], pH3.8=ms$a[,10]*3.8+ms$b[,10],
pH3.0=ms$a[,11]*3.0+ms$b[,11], pH3.2=ms$a[,11]*3.2+ms$b[,11], pH3.4=ms$a[,11]*3.4+ms$b[,11], pH3.6=ms$a[,11]*3.6+ms$b[,11], pH3.8=ms$a[,11]*3.8+ms$b[,11]
)

#plot
pdf("fig/Fig.7_d.pdf",width=14,height=4)
par(ps = 12)
boxplot(list,ylab=expression(paste(ln(delta))),ylim=c(0,6),las=2)
dev.off()