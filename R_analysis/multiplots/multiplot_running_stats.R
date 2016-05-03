######################################################
#  file: multiplot_running_stats.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - plotting ensemble running stats in R
#       - load in all running stat arrays
#       - plot those 4-to-a-figure, in paper format
#  Run in R (open R w/ >R on command line)
#  >source("multiplot_running_stats.R")
######################################################


library("stats")
library("magicaxis") #minor ticks
library("Hmisc") #minor ticks


#load in .RData stuff
#stat_matrix,ersst_array,med_quant,low_quant,upper_quant,time_masked

ensembles = c("control","forced")
stats = c("std","skewness")
multiplot_string = "running_stats_multiplot.pdf"
#multiplot_string = "running_stats_just2.pdf"
pdf(multiplot_string,10,7)
par(mfrow=c(2,2),oma=c(4,3,0,0),mar=c(0,4,3,1)+0.1,xpd=NA)
colors = c("grey","black","red")

for (stat in stats){
    for (ensemble in ensembles){
        load(paste(ensemble,"_running_",stat,".RData",sep=""))

        #axis parameters
        if(stat == "std"){
            plot_ylim = c(0.2,1.3)
	    grid_y = c(0.2,0.4,0.6,0.8,1.0,1.2) 
            minor_y = 2}
        if(stat == "skewness"){
            plot_ylim = c(-1.25,2.)
	    grid_y = c(-1.0,-0.5,0.0,0.5,1.0,1.5)
            minor_y = 1}
        if(ensemble == "control"){
           start_year = 15.
            end_year = start_year+220.
	    grid_x = c(50,100,150,200)
	    grid_y_x1 = vector("numeric",length(grid_y))+start_year
	    grid_y_x2 = vector("numeric",length(grid_y))+end_year
            minor_x = 5}
        if(ensemble == "forced"){
            start_year = 1865.
            end_year = 2085.
	    grid_x = c(1900,1950,2000,2050)
            minor_x = 5}
	grid_y_x1 = vector("numeric",length(grid_y))+start_year
        grid_y_x2 = vector("numeric",length(grid_y))+end_year
        grid_x_y1 = vector("numeric",length(grid_x))+plot_ylim[1]
	grid_x_y2 = vector("numeric",length(grid_x))+plot_ylim[2] 
        #labels
        if((stat == "std") & (ensemble == "control")){
            main_str="Control"
            stat_string=expression(paste("St. d. (",""*degree,"C",")",sep=""))
            x_str = ""}
        if((stat == "std") & (ensemble == "forced")){
            main_str="Forced"
            stat_string=""
            x_str = ""}
        if((stat == "skewness") & (ensemble == "control")){
            main_str=""
            stat_string="Skewness"
            x_str = "Year"}
        if((stat == "skewness") & (ensemble =="forced")){
            main_str = ""
            stat_string =""
            x_str = "Year"}

        for(i in 1:length(forced_runs)){
            if(i==1){
                plot(time_masked,stat_matrix[1,1:length(time_masked)],col=colors[1],
                    xlim=c(start_year,start_year+220.),ylim=plot_ylim,
                    xlab="",ylab=stat_string,main=main_str,
                    cex.lab=2.0,cex.axis=1.7,cex.main=2.0,cex.sub=2.0,
                    lwd="1",lty=1,type="l",yaxt="n",mgp=c(4.5,1,0))
                axis(2,cex.axis=1.7,las=2)
		axis(4,labels=FALSE,tcl=0.5)
                minor.tick(nx=minor_x,ny=minor_y)
                mtext(x_str,side=1,cex=2.0,line=3)#}
        	segments(grid_x,grid_x_y1,grid_x,grid_x_y2,col="lightgray",lty="dotted")
		segments(grid_y_x1,grid_y,grid_y_x2,grid_y,col="lightgray",lty="dotted")}
	    else{
                lines(time_masked,stat_matrix[i,1:length(time_masked)],
                    col=colors[1],lwd="1",lty=1)}}
        #lines(time_masked,med_quant,col=colors[2],lwd="3")
        #lines(time_masked,low_quant,col=colors[2],lty=2,lwd="3")
        #lines(time_masked,upper_quant,col=colors[2],lty=2,lwd="3")
        #lines(time_masked,med_quant,col=colors[2],lwd="3",lty=0)
        #lines(time_masked,low_quant,col=colors[2],lwd="3",lty=0)
        #lines(time_masked,upper_quant,col=colors[2],lwd="3",lty=0)
	if(ensemble == "forced"){
            lines(time_ersst,ersst_array,col=colors[3],lwd="3")}
    }
}
dev.off()
