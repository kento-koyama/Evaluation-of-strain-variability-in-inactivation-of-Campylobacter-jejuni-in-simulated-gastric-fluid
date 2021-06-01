#setwd("~appendix")
ms <-readRDS(file = "result_Model_para.obj")
library(ggplot2)
library(GGally)
library(hexbin)
library(MASS)


para_a <- c(ms$a[,1],ms$a[,2],ms$a[,3],ms$a[,4],ms$a[,5],ms$a[,6],ms$a[,7],ms$a[,8],ms$a[,9],ms$a[,10],ms$a[,11])
para_b <- c(ms$b[,1],ms$b[,2],ms$b[,3],ms$b[,4],ms$b[,5],ms$b[,6],ms$b[,7],ms$b[,8],ms$b[,9],ms$b[,10],ms$b[,11])
para_e <- c(ms$e[,1],ms$e[,2],ms$e[,3],ms$e[,4],ms$e[,5],ms$e[,6],ms$e[,7],ms$e[,8],ms$e[,9],ms$e[,10],ms$e[,11])
para_f <- c(ms$f[,1],ms$f[,2],ms$f[,3],ms$f[,4],ms$f[,5],ms$f[,6],ms$f[,7],ms$f[,8],ms$f[,9],ms$f[,10],ms$f[,11])

d <- data.frame(a = as.numeric(para_a), b = as.numeric(para_b) , e = as.numeric(para_e) , f= as.numeric(para_f) )
N_col <- ncol(d)
ggp <- ggpairs(d, diag='blank', lower='blank',switch='both') + v1_ggmatrix_theme() + theme(legend.position = "none", 
                                                                                           panel.grid.major = element_blank(), 
                                                                                           axis.ticks = element_blank(), 
                                                                                           panel.border = element_rect(linetype = "solid", colour = "black", fill = NA),strip.text=element_text(size=12))
label_list <- list(a="a",b="b", e="e", f="f")

for(i in 1 : N_col) {
  x <- d[ ,i]
  bw <- ( max(x) - min(x) ) / 10
  p <- ggplot(data.frame(x), aes(x))
  p <- p + theme_bw(base_size = 14)
  p <- p + theme(axis.text.x = element_text(angle = 60 , vjust = 1 , hjust = 1))
  p <- p + geom_histogram(binwidth = bw/1.5 , fill = 'white' , color = 'grey5' )
  p <- p + geom_line(eval(bquote(aes(y=..count..*.(bw/1.5)))) , stat = 'density' )
  p <- p + geom_label(data=data.frame(x=Inf, y=Inf, label=label_list[[colnames(d)[i]]]), aes(x=x, y=y, label=label), hjust=1, vjust=1)
  ggp <- putPlot(ggp, p, i, i)
}
for(j in 1:(N_col-1)) {
  for(i in (j+1):N_col) 
  {
    x <- d[ ,j]
    y <- d[ ,i]
    p <- ggplot(data.frame(x , y) , aes( x = x , y = y ) )
    p <- p + theme_bw(base_size=14)
    p <- p + theme(axis.text.x=element_text(angle=60, vjust=1, hjust=1))
    p <- p + geom_hex()
    p <- p + scale_fill_gradientn(colours=gray.colors(7, start=0.1, end=0.9))
    ggp <- putPlot(ggp, p, i, j)
  }
}

cairo_pdf(file = 'fig/Fig.3.pdf' , w = 5 , h = 4 )
print(ggp , left = 0.3 , bottom = 0.3, file = NULL)
dev.off()
