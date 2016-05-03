######################################################
#  file: ensemble_stats_hist.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - plotting histogram of ensemble NINO3 stats in R
#       - load in multiple CESM model NINO3 stats
#       - plot ensemble histograms of those stats
#  Run in R (open R w/ >R on command line)
#  >source("ensemble_stats_hist.R")
######################################################

library("stats")
library("biwavelet") #for morlet wavelet analysis
library("moments") #for skewness calculation
## load in the .txt monthly ENSO temperature data
## columns are time,nino3_sst,nino3_climate_anom,
## nino3_smoothed_anom,sst_longterm_rm,nino3_longterm_rm
forced_runs = c("42","43","44","45","46","47",
        "48","49","50","51","52","53","54","55",
        "56","57","58","59","60","61","62","63",
        "64","65","66","67","68","69","70","71",
        "72","73","74","75","76","77","78","79",
        "80","81","82","83","84","85","86","87",
        "88","89","90","91")
#forced_runs = c("42","43")

time_periods = c("2575","19401990","20402090")
time_periods = time_periods[c(1,3)]
stat_choice = "variance"
## stat_choice - plot hist of either variance or skewness
region = "nino34" #choices are nino34 and nino3

colors=c("red",rgb(0.7,0.3,0),rgb(0,1,0,0.5),rgb(0,0.7,0.3),rgb(0,0,1,0.5),rgb(0.3,0,0.7))
colors=colors[c(1,2,5,6)]

for (i in 1:length(time_periods)){

#set the time period for which we calc the stat
time_period = time_periods[i]
if(time_period == "2575"){
start_year = 25.
end_year = 75.}
if(time_period == "19401990"){
start_year = 1940.
end_year = 1990.}
if(time_period == "20402090"){
start_year = 2040.
end_year = 2090.}

## load in the time series for each run
for (j in 1:length(forced_runs)){

model_run = forced_runs[j]
print(model_run)
if (i==1){
nino3_file = paste("../model_time_series/control",model_run,"_",region,"_",toString(start_year),toString(end_year),".txt",sep="")
}
else{nino3_file=paste("../model_time_series/model",model_run,"_",region,"_",toString(start_year),toString(end_year),".txt",sep="")
}
nino3_data = read.table(nino3_file,header=TRUE)
attach(nino3_data)

if (region=="nino34"){nino3_longterm_rm = nino34_longterm_rm}

used_indices = which(time>start_year & time<end_year+0.001)
if(stat_choice == "variance"){
stat_val=var(nino3_longterm_rm[used_indices])}
if(stat_choice == "skewness"){
stat_val=skewness(nino3_longterm_rm[used_indices])}

if(j==1){
    stat_array = c(stat_val)
}
else{
    stat_array = append(stat_array,stat_val)
}

if(j==length(forced_runs)){
stat_hist <- hist(stat_array,plot=FALSE,breaks=20)

if(i==1){

if (stat_choice == "variance"){
    used_xlim = c(0.2,0.8)
    used_ylim = c(0,10)}
else if(stat_choice == "skewness"){
    used_xlim = c(-1.,1.5)
    used_ylim = c(0,15)}
pdf(paste(stat_choice,"_",region,"_control_vs_",time_periods[2],"_histograms.pdf",sep=""))
plot(stat_hist,col=colors[2*i-1],xlab=stat_choice,
        ylab="Frequency",main=paste("Histogram of ",region," ",stat_choice),
	xlim=used_xlim,ylim=used_ylim)
legend("topleft",c("Control","tdist",time_periods[2],"tdist"),#,"2040-2090","tdist"),
        text.col=colors)
}
else{plot(stat_hist,col=colors[2*i-1],add=TRUE,xlim=used_xlim,ylim=used_ylim)}

stat_choice_string = deparse(substitute(running_variance))
####Plot the quantiles of the distribution
quants = quantile(stat_array,probs=c(0.05,0.5,0.95),names=FALSE)
low_quant = quants[1]
med_quant = quants[2]
upper_quant = quants[3]

#abline(v=med_quant,col=colors[2*i-1],lwd="3")
#abline(v=low_quant,col=colors[2*i-1],lty=2,lwd="3")
#abline(v=upper_quant,col=colors[2*i-1],lty=2,lwd="3")

####Plot the t-distribution about the mean
x = stat_array
A = qt(0.975,df=length(forced_runs)-1) #the t-distribution value for 49 DOF, 95% two-sided
low_t =mean(x)-A*sd(x)/length(x)^0.5
high_t = mean(x)+A*sd(x)/length(x)^0.5

abline(v=mean(x),col=colors[2*i],lwd="3")
abline(v=low_t,col=colors[2*i],lty=2,lwd="3")
abline(v=high_t,col=colors[2*i],lty=2,lwd="3")

}
detach(nino3_data)
}}
dev.off()

###########################

