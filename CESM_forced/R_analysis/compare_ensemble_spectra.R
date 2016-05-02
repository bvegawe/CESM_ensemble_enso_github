######################################################
#  file: compare_ensemble_spectra.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - testing power spectrum tools in R
#	- load in ENSO spectral data for model runs
#	- plot power spectra for two time periods
#  Run in R (open R w/ >R on command line)
#  >source("compare_ensemble_spectra.R")
######################################################

library("stats")
library("biwavelet") #for morlet wavelet analysis

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

region = "nino34" #currently either nino34 or "" for nino3...
time_periods = c("2575","20402090") 
#colors = c("red","red",rgb(0,0.7,0.3),rgb(0,0.7,0.3)) #2 per time period
colors = c("red","red","blue","blue")
spectrum_type = "burg"
## spectrum_type - choices are burg (default), morlet_wit, morlet_tw
plot_type = "tdist"
## plot_type - choices are quantiles (show median, 5%,95% quantiles),
## 	and tdist (the 95% t-dist about the mean)
if(plot_type =="quantiles"){plot_label="5%, 50%, and 95% quantiles"}
if(plot_type =="tdist"){plot_label="95% t-dist"}

for(i in 1:length(time_periods)){
## load in the spectral data for each run
for (j in 1:length(forced_runs)){

model_run = forced_runs[j]
print(model_run)
spectral_file = paste("model_run_spectra/",model_run,"_spectra_",time_periods[i],"_",region,".RData",sep="")
load(spectral_file)


if(spectrum_type == "morlet_wit"){
scale=morlet_Wit09_scale
power=morlet_Wit09_power
if((i == 1) & (j==1)){
    pdf(paste("morlet_Wittenberg09_",time_periods[1],time_periods[2],"_",plot_type,".pdf",sep=""))
###Make Morlet Wittenberg09 fig2 plot - 
    plot(power,scale,col="grey",log="y",ylim=c(20,0.25),xlim=c(0,2.5),
	ylab="Period (years)",xlab="Variance / period (months)",
	main="NINO3 SST Global Wavelet Power (as in Wittenberg09)",
	lwd="2",lty=0,type="l")
	legend("bottomright",c(time_periods[1],plot_label, time_periods[2], plot_label),text.col=colors)
}}

if(spectrum_type == "morlet_tw"){
scale=morlet_TW99_scale
power=morlet_TW99_power
#####Make Torrence+Webster '99  figure 2 - morlet power spectrum
if((j == 1) & (i==1)){
     pdf(paste("forced_morlet_TW99_",time_periods[1],time_periods[2],"_",plot_type,".pdf",sep=""))
     plot(scale,power,log="xy",xlim=c(34,0.3),ylim=c(0.1,140),type="l",
         xlab="Period (Years)",ylab="global wavelet power (sigma^2)",col="grey",lty=0,
         main="NINO3 SST Global Wavelet Power (as in TW99)")
         regend("topleft",c(time_periods[1],plot_label,time_periods[2],plot_label),text.col=colors)
}}

if(spectrum_type == "burg"){
burg_norm_longterm_rm_power = burg_norm_longterm_rm_power[1:length(burg_norm_longterm_rm_power),1] #correct the power matrix to be a 1-d vector
scale = burg_norm_longterm_rm_scale
power = burg_norm_longterm_rm_power
###Make a normalized burg spectrum (as in IPCC)
if((j==1) & (i==1)){
        pdf(paste("burg_",time_periods[1],"_vs_",time_periods[2],"_",plot_type,"_",region,".pdf",sep=""))
        plot(scale,power,log="x",#log="xy",
                type="l",xlab="f (cycles/yr)",ylab="normalized f*S(f)",col="grey",
                main="Power Spectrum of NINO34 SST Anomalies w/o Longterm Trend",
                #ylim=c(0.01,2.0),lty=0)
		ylim=c(0.0,1.0),xlim=c(0.1,1),lty=0)
                #ylim=c(0.003,2.0),lty=0)
        legend("topleft",c(time_periods[1],plot_label,
                time_periods[2],plot_label),text.col=colors)
}}

if(j==1){
    power_matrix = power
}
else{
#    lines(morlet_Wit09_power,morlet_Wit09_scale,col="grey")
    power_matrix = rbind(power_matrix,power)
}
if(j==length(forced_runs)){

if(plot_type=="quantiles"){
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
lines(med_quant,scale,col=colors[2*i])
lines(low_quant,scale,col=colors[2*i],lty=2)
lines(upper_quant,scale,col=colors[2*i],lty=2)
}
else{ 
lines(scale,med_quant,col=colors[2*i])
lines(scale,low_quant,col=colors[2*i],lty=2)
lines(scale,upper_quant,col=colors[2*i],lty=2)
}}
if(plot_type=="tdist"){
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
lines(means,scale,col=colors[2*i])
lines(low_t,scale,col=colors[2*i],lty=2)
lines(high_t,scale,col=colors[2*i],lty=2)  
}
else{
lines(scale,means,col=colors[2*i])
lines(scale,low_t,col=colors[2*i],lty=2)
lines(scale,high_t,col=colors[2*i],lty=2)
}}
}


}}
dev.off()

