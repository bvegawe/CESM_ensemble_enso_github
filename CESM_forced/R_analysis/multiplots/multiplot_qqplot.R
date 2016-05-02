######################################################
#  file: multiplot_qqplot.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - plotting qqplots of stats in R
#       - load in all stat arrays
#       - plot those 4-to-a-figure, in paper format
#  Run in R (open R w/ >R on command line)
#  >source("multiplot_qqplot.R")
######################################################

library("stats")
library("magicaxis") #minor ticks
library("Hmisc") #minor ticks

#load in .RData stuff

multiplot_string = "qqplot_multiplot.pdf"
stats = c("STD","Skewness","NinoCount","NinaCount")
y_axis_string = "Sample Quantiles"
main_strings = c(expression(bold("St. d.")),expression(bold("Skewness")),
		expression(bold(paste("El Ni",tilde("n"),"o events",sep=""))),
		expression(bold(paste("La Ni",tilde("n"),"a events",sep=""))))
time_strings = c("Control","1940-1990","2040-2090")
used_ylim=c(c(0.55,0.85),c(-0.3,0.9),c(8,17),c(8,17))
dim(used_ylim) = c(2,4)

pdf(multiplot_string,14,4)
par(mfrow=c(1,4),oma=c(0,0,0,1),mar=c(7.2,8.8,3,0)+0.1,mgp=c(2,1,0),xpd=NA)
xlabel_y = c(0.52,-0.43,7.1,7.1)
ylabel_offset = c(5,4.7,3.7,3.7)
for (i in 1:length(stats)){
    load(paste(stats[i],"_qqplot.RData",sep=""))
    qqdata = qqnorm(stat_list[[1]],plot.it=TRUE,
        cex.axis=1.9,cex.main=1.9,cex.lab=1.9,cex.sub=1.9,
	main = "",ylab="",xlab="",las=1)
    linfit = lm(qqdata$y~qqdata$x)
    ordered_qqx = qqdata$x[order(qqdata$x)]
    ordered_fits = linfit$fitted.values[order(qqdata$x)]
    lines(ordered_qqx[2:(length(qqdata$x)-1)],ordered_fits[2:(length(qqdata$x)-1)])
    #axis(2,cex.axis=2.5,las=2)
    mtext(y_axis_string,cex=1.5,side=2,line=ylabel_offset[i])
    #text(1:3,xlabel_y[i],cex=2.5,pos=2,offset=-0.6,srt=30,
#	labels=time_strings,xpd=TRUE)
    mtext(main_strings[i],font=2,side=3,cex=1.85)
    minor.tick(nx=0,ny=2)
    mtext("Theoretical Quantiles",side=1,cex=1.5,line=3)
#    points(c(2),ersst_val,cex=2.0,lwd=3,pch=4,type="o",col="red")
    #grid(lwd=2)
    #print(par("usr")[3]-0.25)
}
dev.off()


