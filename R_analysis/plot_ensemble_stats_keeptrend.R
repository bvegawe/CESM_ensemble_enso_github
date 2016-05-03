######################################################
#  file: plot_ensemble_stats_keeptrend.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - plotting ensemble running NINO3 stats in R
#	- load in multiple CESM model running NINO3 stats 
#       - these are the stats optionally without any trend removal
#	- plot ensemble averages of those stats
#  Run in R (open R w/ >R on command line)
#  >source("plot_ensemble_stats.R")
######################################################

library("stats")
library("biwavelet") #for morlet wavelet analysis
library("magicaxis") #for minor ticks on plot
library("Hmisc") #also minor ticks

## load in the .txt monthly ENSO temperature data
## columns are time,nino3_sst,nino3_climate_anom,
## nino3_longterm_rm,running_avg,running_variance,
## running_skewness,running_kurtosis
forced_runs = c("42","43","44","45","46","47",
        "48","49","50","51","52","53","54","55",
        "56","57","58","59","60","61","62","63",
        "64","65","66","67","68","69","70","71",
        "72","73","74","75","76","77","78","79",
        "80","81","82","83","84","85","86","87",
        "88","89","90","91")
#forced_runs = c("42") #temporary

ensemble = "forced" #choices are control or forced
trend_str = "wTrend" #choices are wTrend (leave in warming trend) and woTrend (rm warming trend)
window = 50.
window_str = "50yr_" #choices are "50yr_" and "" 
climo_str = "_hyun" #choices are "" (normal climo), and "_hyun" (windowed climo)
region = "nino34" #choices are nino3 and nino34
region_str = "" #how we want it to appear in plot
stat_choice = "running_std"
#stat_choice_string = expression(paste("variance (",""*degree,"C"^2,")",sep=""))
stat_choice_string = expression(paste("standard deviation (",""*degree,"C",")",sep=""))
#stat_choice_string = expression(paste("Running avg SST (",""*degree, "C",")",sep=""))
#stat_choice_string = "skewness"
#stat_choice_string = expression(paste("Anomalous T (",""*degree, "C",")",sep=""))
main_string = "St. d."
#main_string = "Skewness"
#main_string = expression(paste("Running avg SST (",""*degree, "C",")",sep=""))
#main_string = "Trend-removed Nino3.4 index"
plot_type = "quantiles"
#plot_type = "none" #temporary

## stat_choice - plot any of the variables loaded above:
## nino3_sst,nino3_climate_anom,nino3_longterm_rm,
## running_avg,running_variance,running_skewness,
## running_kurtosis, running_std
## stat_choice_string - however u want your statistic labelled
## plot_type - choices are quantiles (show median, 5%,95% quantiles),
##      and tdist (the 95% t-dist about the mean), anything else gives neither

if(ensemble == "control"){
    read_file_struct = paste("_control_",region,"_stats_",window_str,trend_str,climo_str,".txt",sep="")
    start_year = 0.+window/2.+3
    end_year = 100.-window/2.-3
    minor_x = 1}
if (ensemble == "forced"){
    read_file_struct = paste("_forced_",region,"_stats_",window_str,trend_str,climo_str,".txt",sep="")
    start_year = 1851.+window/2.+3
    end_year = 2100.-window/2.-3
    minor_x= 5}
    #start_year = 1940.
    #end_year = 1990.}
colors=c("grey","black","red")
if(plot_type =="quantiles"){plot_label="5%, 50%, and 95% quantiles"}
if(plot_type =="tdist"){plot_label="95% t-dist"}

if(stat_choice =="running_std"){
  if(window == 50.){plot_ylim = c(0.4,1.1)}
  else{plot_ylim = c(0.2,1.3)}
  minor_y = 2}
if(stat_choice =="running_skewness"){
  plot_ylim = c(-1.25,2.)
  minor_y = 5}
if(stat_choice =="running_variance"){
  plot_ylim = c(0.,1.5)
  minor_y = 5}
if(stat_choice =="running_avg"){
  plot_ylim = c(-.2,.2)
  minor_y = 0.01}
if(stat_choice =="nino3_longterm_rm"){
  plot_ylim = c(-4,4)
  minor_y = 5}
## load in the time series for each run
for (j in 1:length(forced_runs)){

model_run = forced_runs[j]
print(model_run)
nino3_file = paste("../model_running_stats/model",model_run,read_file_struct,sep="")
nino3_data = read.table(nino3_file,header=TRUE)
#attach(nino3_data)

time_range = which(nino3_data$time>start_year & nino3_data$time<end_year)
time_masked = nino3_data$time[time_range]

if(stat_choice == "running_variance"){
  stat_array = nino3_data$running_variance[time_range]}
if(stat_choice == "running_std"){
  stat_array = (nino3_data$running_variance[time_range])^0.5}
if(stat_choice == "running_skewness"){
  stat_array = nino3_data$running_skewness[time_range]}
if(stat_choice == "running_avg"){
  stat_array = nino3_data$running_avg[time_range]}
if(stat_choice == "running_kurtosis"){
  stat_array = nino3_data$running_kurtosis[time_range]}
if(stat_choice == "nino3_sst"){
  stat_array = nino3_data$nino3_sst[time_range]}
if(stat_choice =="nino3_longterm_rm"){
  stat_array = nino3_data$nino3_longterm_rm[time_range]}

if(j==1){
    pdf(paste(ensemble,"_",stat_choice,"_",plot_type,"_",region,"_",window_str,trend_str,climo_str,".pdf",sep=""))
    par(mar=c(5,5,4,2)+0.1)
    plot(time_masked,stat_array,col=colors[1],xlim=c(start_year,end_year),
        ylab=stat_choice_string,xlab="year",
        main=main_string,cex.lab=1.5,cex.axis=1.5,cex.main=2.0,cex.sub=1.5,
        lwd="1",lty=1,type="l",ylim=plot_ylim,yaxt="n")
    stat_matrix = stat_array
    axis(2,cex.axis=1.5,las=2)
    minor.tick(nx=minor_x,ny=minor_y)
    #magaxis(cex=1.5,labels=FALSE)
    #if(ensemble=="forced"){legend("topleft","ERSST",text.col=colors[3],bty="n",cex=1.5)}
}
else{
    lines(time_masked,stat_array,col=colors[1])
    stat_matrix = rbind(stat_matrix,stat_array)
}

if(j==length(forced_runs)){

if(plot_type=="quantiles"){
####Do the median and quantile calculations for each period in the spectrum
for (k in 1:length(time_masked)){
     quants = quantile(stat_matrix[1:length(forced_runs),k],probs=c(0.05,0.5,0.95),names=FALSE)
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
lines(time_masked,med_quant,col=colors[2],lwd="3")
lines(time_masked,low_quant,col=colors[2],lty=2,lwd="3")
lines(time_masked,upper_quant,col=colors[2],lty=2,lwd="3")
}

if(plot_type=="tdist"){
####Plot the t-distribution about the mean
for (k in 1:length(time_masked)){
     x = stat_matrix[1:length(forced_runs),k]
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
lines(time_masked,means,col=colors[2],lwd="3")
lines(time_masked,low_t,col=colors[2],lty=2,lwd="3")
lines(time_masked,high_t,col=colors[2],lty=2,lwd="3")
}
}
#legend("topright",c(plot_label),text.col=colors,lwd="1")
#detach(nino3_data)
}

ersst_array=c()
####Add the running ERSST data
if(ensemble == "forced"){
  ersst_file = paste("../model_running_stats/ersst_",region,"_stats_",window_str,trend_str,climo_str,".txt",sep="")
  nino3_data = read.table(ersst_file,header=TRUE)
  start_year = 1890.+window/2.+3
  end_year = 2013.-window/2.-1.
  time_range = which(nino3_data$time>start_year & nino3_data$time<end_year)
  time_ersst = nino3_data$time[time_range]
  if(stat_choice == "running_variance"){
    ersst_array = nino3_data$running_variance[time_range]}
  if(stat_choice == "running_std"){
    ersst_array = (nino3_data$running_variance[time_range])^0.5}
  if(stat_choice == "running_skewness"){
    ersst_array = nino3_data$running_skewness[time_range]}
  if(stat_choice == "running_avg"){
    ersst_array = nino3_data$running_avg[time_range]}
  if(stat_choice == "running_kurtosis"){
    ersst_array = nino3_data$running_kurtosis[time_range]}  
  if(stat_choice == "nino3_sst"){
    ersst_array = nino3_data$nino3_sst[time_range]}
  if(stat_choice == "nino3_longterm_rm"){
    ersst_array = nino3_data$nino3_longterm_rm[time_range]}   
  lines(time_ersst,ersst_array,col=colors[3],lwd="3")
}


####Save arrays for multiplot
if (ensemble == "forced"){
save(stat_matrix,ersst_array,med_quant,low_quant,upper_quant,
    time_masked,time_ersst,forced_runs,
    file=paste(ensemble,"_",stat_choice,"_",window_str,trend_str,climo_str,".RData",sep=""))
}
if (ensemble == "control"){
save(stat_matrix,med_quant,low_quant,upper_quant,
    time_masked,forced_runs,
    file=paste(ensemble,"_",stat_choice,"_",window_str,trend_str,climo_str,".RData",sep=""))
}

dev.off()

###########################

