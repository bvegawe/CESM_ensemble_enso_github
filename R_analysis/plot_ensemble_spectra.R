######################################################
#  file: plot_ensemble_spectra.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - testing power spectrum tools in R
#	- load in ENSO spectral data for model runs
#	- plot power spectra
#  Run in R (open R w/ >R on command line)
#  >source("plot_ensemble_spectra.R")
######################################################

library("stats")
library("biwavelet") #for morlet wavelet analysis
library("magicaxis") #for minor ticks on plot

## load in the .RData model run spectra
## variables are morlet_Wit09_power,morlet_Wit09_scale,
## morlet_TW99_scale,morlet_TW99_power,
## burg_norm_scale,burg_norm_power,
## burg_norm_longterm_rm_scale,burg_norm_longterm_rm_power

forced_runs = c("42","43","44","45","46","47",
        "48","49","50","51","52","53","54","55",
        "56","57","58","59","60","61","62","63",
        "64","65","66","67","68","69","70","71",
        "72","73","74","75","76","77","78","79",
        "80","81","82","83","84","85","86","87",
        "88","89","90","91")
#forced_runs = c("83")

region = "nino34" #currently either nino34 or nino3...
region_str="NINO34" #how we want it to show up on plot
time_period = "2575"
if(time_period=="0100"){forced_runs=forced_runs[2:(length(forced_runs)-1)]} 
period_str = "Control" #how we want the time to show up on plot
colors=c("grey","black","red") #color for individual spectra,median/mean,and ERSST
spectrum_type = "burg"
## spectrum_type choices are burg,morlet_wit,mortlet_tw
add_tdist = TRUE ##if true, plot the 95% t-dist at each scale as well
add_quant = FALSE  ##if true, plot the 95% quantiles at each scale as well
add_ersst = FALSE  ##plot the corresponding ERSST data (only useable for times before 2013)

#just counting double bumps
double_bump_count = 0
## load in the spectral data for each run
for (j in 1:length(forced_runs)){

model_run = forced_runs[j]
print(model_run)
spectral_file = paste("model_run_spectra/",model_run,"_spectra_",
    time_period,"_",region,".RData",sep="")
load(spectral_file)

if(spectrum_type == "morlet_wit"){
scale=morlet_Wit09_scale
power=morlet_Wit09_power
if(j==1){
    pdf(paste("morlet_Wittenberg09_",time_period,".pdf",sep=""))
###Make Morlet Wittenberg09 fig2 plot - 
    plot(power,scale,col=colors[1],log="y",ylim=c(20,0.25),xlim=c(0,2.5),
        ylab="Period (years)",xlab="Variance / period (months)",
        main=paste(region_str," SST Global Wavelet Power (as in Wittenberg09)",sep=""),
        lwd="1",lty=1,type="l")
        legend("bottomright",c(period_str,"ERSST"),text.col=colors[2:3])
}}

if(spectrum_type == "morlet_tw"){
scale=morlet_TW99_scale
power=morlet_TW99_power
#####Make Torrence+Webster '99  figure 2 - morlet power spectrum
if(j == 1){
     pdf(paste("forced_morlet_TW99_",time_period,".pdf",sep=""))
     plot(scale,power,log="xy",xlim=c(34,0.3),ylim=c(0.1,140),type="l",
         xlab="Period (Years)",ylab="global wavelet power (sigma^2)",col="grey",lty=1,
         main=paste(region_str," SST Global Wavelet Power (as in TW99)",sep=""))
         legend("topleft",c(period_str,"ERSST"),text.col=colors[2:3])
}}

if(spectrum_type == "burg"){
burg_norm_longterm_rm_power = burg_norm_longterm_rm_power[1:length(burg_norm_longterm_rm_power),1] #correct the power matrix to be a 1-d vector
scale = burg_norm_longterm_rm_scale
power = burg_norm_longterm_rm_power
###Make a normalized burg spectrum (as in IPCC)
if(j==1){
        pdf(paste("burg_",period_str,"_",region,".pdf",sep=""))
	par(mar=c(5,5,8,2)+0.1) #adding margin room on top
        plot(scale,power,log="x",#log="xy",
            type="l",xlab="frequency (cycles/year)",ylab="frequency*power",col=colors[1],
            #main=period_str,#paste(period_str," SST Anomalies w/o Longterm Trend",sep=""),
            #ylim=c(0.01,2.0),lty=0)
            ylim=c(0.001,0.45),xlim=c(0.08,1),lty=1,
	    #xlim=c(0.08,1),lty=1,#temporary, use above normally	
	    cex.lab=1.5,cex.axis=1.5,cex.main=2.0,cex.sub=1.5,yaxt="n")
            #ylim=c(0.003,2.0),lty=0)
	magaxis(cex=1.5,labels=FALSE) #getting logarithmic minor ticks
	axis(2,cex.axis=1.5,las=2) 
        axis(3,at=c(0.1,0.2,0.5,1.0),labels=c("10","5","2","1"),cex.axis=1.5) #adding period ticks
	mtext("period (years/cycle)",cex=1.5,side=3,line=2.5)
	mtext(period_str,cex=2.0,side=3,line=5,font=2)
}}

if(j==1){
    power_matrix = power
}
else{
    if(spectrum_type=="morlet_wit"){lines(power,scale,col=colors[1])}
    else{lines(scale,power,col=colors[1])}
    power_matrix = rbind(power_matrix,power)

#CHECKING FOR DOUBLE BUMPS
if ((which.max(power[which(scale > 0.5)])  > 1) & 
    (which.max(power[which(scale<0.5)]) < length(which(scale<0.5)))){
    print("Double bump!")
    double_bump_count = double_bump_count+1}#just to check the double bumps
}
if(j==length(forced_runs)){

if(add_quant==TRUE){
####Do the median and quantile calculations for each period in the spectrum
for (k in 1:length(scale)){
     quants = quantile(power_matrix[1:length(forced_runs),k],probs=c(0.05,0.5,0.95),names=FALSE)
    if(k==1){
        low_quant = quants[1]
        med_quant = quants[2]
        upper_quant = quants[3]
    }
    else{
        low_quant = append(low_quant,quants[1])
        med_quant = append(med_quant,quants[2])
        upper_quant = append(upper_quant,quants[3])
    }
}
if(spectrum_type == "morlet_wit"){
lines(med_quant,scale,col=colors[2],lwd=3)
lines(low_quant,scale,col=colors[2],lty=2,lwd=3)
lines(upper_quant,scale,col=colors[2],lty=2,lwd=3)
}
else{
lines(scale,med_quant,col=colors[2],lwd=3)
lines(scale,low_quant,col=colors[2],lty=2,lwd=3)
lines(scale,upper_quant,col=colors[2],lty=2,lwd=3)
}}

if(add_tdist==TRUE){
####Plot the t-distribution about the mean
for (k in 1:length(scale)){
     x = power_matrix[1:length(forced_runs),k]
     A = qt(0.975,df=length(forced_runs)-1) #the t-distribution value for 49 DOF, 95% two-sided
     low_side_t =mean(x)-A*sd(x)/length(x)^0.5
     high_side_t = mean(x)+A*sd(x)/length(x)^0.5
    if(k==1){
        low_t = low_side_t
        high_t = high_side_t
        means = mean(x)
    }
    else{
        low_t = append(low_t,low_side_t)
        high_t = append(high_t,high_side_t)
        means = append(means,mean(x))
    }
}
if(spectrum_type == "morlet_wit"){
lines(means,scale,col=colors[2],lwd=2)
lines(low_t,scale,col=colors[2],lty=2,lwd=3)
lines(high_t,scale,col=colors[2],lty=2,lwd=3)
}
else{
lines(scale,means,col=colors[2],lwd=3)
lines(scale,low_t,col=colors[2],lty=2,lwd=3)
lines(scale,high_t,col=colors[2],lty=2,lwd=3)
}}
}
}

ersst_scale = NULL
ersst_array = NULL

if(add_ersst==TRUE){
ersst_file = paste("model_run_spectra/ersst_spectra_",time_period,"_",region,".RData",sep="")
load(ersst_file)
    if(spectrum_type=="burg"){
        lines(burg_norm_longterm_rm_scale,
            burg_norm_longterm_rm_power[1:length(burg_norm_longterm_rm_power),1],
            col=colors[3],lwd=3)
        ersst_scale = burg_norm_longterm_rm_scale
        ersst_array = burg_norm_longterm_rm_power[1:length(burg_norm_longterm_rm_power),1]}}

if(spectrum_type=="burg"){
#Save arrays for multiplot
    if(add_quant==TRUE){
	save(forced_runs,power_matrix,ersst_array,med_quant,
	  low_quant,upper_quant,scale,ersst_scale,
          file=paste(period_str,"_spectrum.RData",sep=""))}
    else if(add_tdist==TRUE){
        save(forced_runs,power_matrix,ersst_array,means,
          low_t,high_t,scale,ersst_scale,
          file=paste(period_str,"_tdist_spec.RData",sep=""))}}

dev.off()

###########################

