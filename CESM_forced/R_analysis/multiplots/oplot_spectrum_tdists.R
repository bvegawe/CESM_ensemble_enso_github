######################################################
#  file: multiplot_spectrum.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - plotting spectra in R
#       - load in all spectrum arrays
#       - plot those 3-to-a-figure, in paper format
#  Run in R (open R w/ >R on command line)
#  >source("multiplot_spectrum.R")
######################################################

library("stats")
library("magicaxis") #minor ticks
library("Hmisc") #minor ticks

oplot_string = "spectrum_tdist.pdf"
times = c("Control","1940-1990","2040-2090")

pdf(oplot_string)
xlabel_y = c(0.52,-0.41,7.2,7.2)
ylabel_offset = c(5,4,3.5,3.5)

colors=c("blue","black","red")
ylabels=c("Frequency*power","","")

for (i in 1:length(times)){
    load(paste(times[i],"_tdist_spec.RData",sep=""))
    if(i==1){
	plot(scale,means,col=colors[i],lwd="3",type="l",lty=0,
	  xlab="Frequency (cycles/year)",ylab="Frequency*power",
          ylim=c(0.001,0.25),xlim=c(0.08,1),cex.lab=1.5,
	  cex.axis=1.5,cex.main=2.0,cex.sub=1.5)
	magaxis(cex=1.5,labels=FALSE)
	#axis(2,cex.axis=1.5,las=2)
	axis(3,at=c(0.1,0.2,0.5,1.0),labels=c("10","5","2","1"),cex.axis=1.5) #adding period ticks    
        mtext("Period (years/cycle)",cex=1.5,side=3,line=2.5)}
    else{lines(scale,means,col=colors[i],lwd="2",lty=0)}
    lines(scale,low_t,col=colors[i],lwd="3",lty=1)
    lines(scale,high_t,col=colors[i],lwd="3",lty=1)
    if(times[i] =="1940-1990"){
	lines(scale,ersst_array,col=colors[3],lwd="3",lty=0)}
}
legend(0.68,0.25,c("Control","1940-1990","2040-2090"),lty=1,lwd=3,col=colors,cex=1.3)
dev.off()
