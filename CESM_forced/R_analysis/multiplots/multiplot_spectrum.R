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

multiplot_string = "spectrum_noquants.pdf"
times = c("Control","1940-1990","2040-2090")

pdf(multiplot_string,12,5)
par(mfrow=c(1,3),oma=c(0,2,0,0),mar=c(5,5,7,1)+0.1,mgp=c(5,1,0))
xlabel_y = c(0.52,-0.41,7.2,7.2)
ylabel_offset = c(5,4,3.5,3.5)

colors=c("grey","black","red")
ylabels=c("Frequency*power","","")

for (i in 1:length(times)){
    load(paste(times[i],"_spectrum.RData",sep=""))
    for(j in 1:length(forced_runs)){
	if(j==1){
    	    plot(scale,power_matrix[1,1:length(scale)],log="x",
	        type="l",col=colors[1],lty=1,yaxt="n",
	        xlab="",ylab="",ylim=c(0.001,0.45),xlim=c(0.08,1),
		cex.lab=2.,cex.axis=2.0,cex.main=2.0,cex.sub=2.0)
    	    magaxis(cex=2.0,labels=FALSE) # getting logarithmic minor ticks
    	    axis(2,cex.axis=2.0,las=2)
    	    axis(3,at=c(0.1,0.2,0.5,1.0),labels=c("10","5","2","1"),
		cex.axis=2.0) #period ticks
    	    mtext("Period (years/cycle)",cex=1.4,side=3,line=3.0)
    	    mtext(times[i],cex=1.4,side=3,line=5.5,font=2)
	    mtext("Frequency (cycles/year)",cex=1.4,side=1,line=3)
	    mtext(ylabels[i],side=2,outer=FALSE,line=4.0,cex=1.4)}
	else{
	    lines(scale,power_matrix[j,1:length(scale)],col=colors[1],lty=1)}}
    lines(scale,med_quant,col=colors[2],lwd="3",lty=0)
    lines(scale,low_quant,col=colors[2],lwd="3",lty=0)
    lines(scale,upper_quant,col=colors[2],lwd="3",lty=0)
    if(times[i] =="1940-1990"){
	lines(scale,ersst_array,col=colors[3],lwd="3")}
}
dev.off()
