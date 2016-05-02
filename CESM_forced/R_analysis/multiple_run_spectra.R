######################################################
#  file: multiple_run_spectra.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - testing power spectrum tools in R
#	- load in multiple CESM model monthly ENSO data
#	- create power spectra
#	- write to .txt files
#  Run in R (open R w/ >R on command line)
#  >source("multiple_run_spectra.R")
######################################################

library("stats")
library("biwavelet") #for morlet wavelet analysis

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
#forced_runs = c("90","91")

ensemble = "control" #choices are forced, control, and ersst
start_year = 25.
end_year = 75.
if(end_year == 100.){forced_runs = forced_runs[2:(length(forced_runs)-1)]}
#start_year = 1890.
#end_year = 1990. 
region = "nino3"

if (ensemble == "ersst"){forced_runs = "ersst"}

## load in the time series for each run
for (j in 1:length(forced_runs)){

model_run = forced_runs[j]
print(model_run)
if (ensemble == "forced"){
nino3_file = paste("../model_time_series/model",model_run,"_",region,"_",toString(start_year),toString(end_year),".txt",sep="")}
else if (ensemble == "control"){
nino3_file = paste("../model_time_series/control",model_run,"_",region,"_",toString(start_year),toString(end_year),".txt",sep="")}
else if (ensemble == "ersst"){
nino3_file = paste("../model_time_series/ersst_",region,"_",toString(start_year),toString(end_year),".txt",sep="")}

nino3_data = read.table(nino3_file,header=TRUE)#header=FALSE,skip=1,col.names=c("time","nino3_sst","nino3_climate_anom","nino3_smoothed_anom","sst_longterm_rm","nino3_longterm_rm","nino3_smoothed_anom"))
#attach(nino3_data)

### create our time series objects
#start_year = 1851
#start_time = 1851.+31./365
#last_good_index = 1741
#first_good_time = 1860
#nino3_sst_ts <- ts(nino3_sst,frequency=12,start=c(start_time,1))
#nino3_index_ts <- ts(nino3_climate_anom,frequency=12,start=c(start_time,1))
#nino3_smoothed_anom_ts <- ts(nino3_smoothed_anom,frequency=12,start=c(start_time,1))
#nino3_sst_longterm_rm_ts <- ts(sst_longterm_rm,frequency=12,start=c(start_time,1))
#nino3_index_longterm_rm_ts <- ts(nino3_longterm_rm,frequency=12,start=c(start_time,1))
#nino3_smoothed_anom_ts_clipped <- ts(nino3_smoothed_anom[108:last_good_index],frequency=12,start=c(first_good_time,1))
#nino3_sst_longterm_rm_clipped <- ts(sst_longterm_rm[108:last_good_index],frequency=12,start=c(first_good_time,1))
#nino3_index_longterm_rm_ts_clipped <- ts(nino3_longterm_rm[108:last_good_index],frequency=12,start=c(first_good_time,1))

## create our time series objects for a specific time range
## comment out above section to use this and vice versa.
if (region == "nino3"){
used_indices = which(nino3_data$time>start_year & nino3_data$time<end_year+0.001)
start_time = start_year+31./365
nino3_sst_ts <- ts(nino3_data$nino3_sst[used_indices],frequency=12,start=c(start_time,1))
nino3_index_ts <- ts(nino3_data$nino3_climate_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts <- ts(nino3_data$nino3_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_ts <- ts(nino3_data$sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts <- ts(nino3_data$nino3_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts_clipped <- ts(nino3_data$nino3_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_clipped <- ts(nino3_data$sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts_clipped <- ts(nino3_data$nino3_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
}
if (region == "nino34"){
used_indices = which(nino3_data$time>start_year & nino3_data$time<end_year+0.001)
start_time = start_year+31./365
nino3_sst=nino3_data$nino34_sst
nino3_sst_ts <- ts(nino3_data$nino34_sst[used_indices],frequency=12,start=c(start_time,1))
nino3_index_ts <- ts(nino3_data$nino34_climate_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts <- ts(nino3_data$nino34_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_ts <- ts(nino3_data$sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts <- ts(nino3_data$nino34_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts_clipped <- ts(nino3_data$nino34_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_clipped <- ts(nino3_data$sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts_clipped <- ts(nino3_data$nino34_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
}

## generate max entropy power spectra for our anomaly time series
nino3_sst_spec = spec.ar(nino3_sst_ts,1000,method="burg")
nino3_index_spec = spec.ar(nino3_index_ts,1000,method = "burg") #units: power/[unit freq (yr^-1)]
smoothed_anom_spec = spec.ar(nino3_smoothed_anom_ts_clipped,1000,method = "burg")
nino3_index_longterm_rm_spec = spec.ar(nino3_index_longterm_rm_ts_clipped,1000,method="burg")
period = 12./nino3_index_spec$freq #units: months
smoothed_period = 12./smoothed_anom_spec$freq
### convert power spectrum to dP/d(period) from dP/d(frequency) - IPCC doesn't do this, so scratch it.
#nino3_index_power_period = nino3_index_spec$spec*(1./(period/12.)^2)*12. #units: power/[unit period (months)]
#smoothed_anom_power_period = smoothed_anom_spec$spec*(1./(period/12.)^2)*12.

## generate morlet global wavelet spectrum
#time_smoothed = 1851. + 0.038 + 1./12 * c(1:length(time)-1)
#ts = array(c(time_smoothed,nino3_sst),dim=c(length(time_smoothed),2))
#sst_wt = wt(ts)
#sst_gws = rowMeans(sst_wt$power)

## if only using a specific time range, comment out above section
## and vice versa
time_smoothed = start_year+0.038+1./12*c(1:length(used_indices)-1)
ts = array(c(time_smoothed,nino3_sst[used_indices]),dim=c(length(time_smoothed),2))
sst_wt= wt(ts)
sst_gws = rowMeans(sst_wt$power)


## try to recreate Wittenberg 2009 fig. 2a (I'll just use 1870-2004- and not the 20-yr periods)
#start_year = 1870
#end_year = 2005
wit_indices = (nino3_data$time > start_year) & (nino3_data$time < end_year)
time_wit = nino3_data$time[wit_indices]
time_wit_smoothed = time_wit[1] + 1./12 * c(1:length(time_wit)-1)
nino3_sst_wit = nino3_data$nino3_sst[wit_indices]
ts_wit = array(c(time_wit_smoothed,nino3_sst_wit),dim=c(length(time_wit_smoothed),2))
sst_wt_wit = wt(ts_wit,pad=FALSE)
scale_wit = sst_wt_wit$scale
sst_gws_wit = rowMeans(sst_wt_wit$power)
norm_gws_wit = sst_gws_wit/scale_wit/12.

#normalize such that area under our spectrum = 1.
#area_index = sum(nino3_index_spec$spec*(nino3_index_spec$freq[9]-nino3_index_spec$freq[8]))
#area_smoothed = sum(smoothed_anom_spec$spec*(smoothed_anom_spec$freq[9]-smoothed_anom_spec$freq[8]))
#area_sst = sum(nino3_sst_spec$spec*(nino3_sst_spec$freq[9]-nino3_sst_spec$freq[8]))
#area_index_longterm_rm = sum(nino3_index_longterm_rm_spec$spec*(nino3_index_longterm_rm_spec$freq[9]-nino3_index_longterm_rm_spec$freq[8]))
#normalized_index_spec = nino3_index_spec$spec*nino3_index_spec$freq/area_index
#normalized_index_spec_longterm_rm = nino3_index_longterm_rm_spec$spec*nino3_index_longterm_rm_spec$freq/area_index_longterm_rm

#normalize only such that we multiply by frequency
normalized_index_spec = nino3_index_spec$spec*nino3_index_spec$freq
normalized_index_spec_longterm_rm = nino3_index_longterm_rm_spec$spec*nino3_index_longterm_rm_spec$freq

#renaming saved variables for clarity
morlet_Wit09_power = norm_gws_wit
morlet_Wit09_scale = scale_wit
morlet_TW99_scale = sst_wt$scale
morlet_TW99_power = sst_gws
burg_norm_scale = nino3_index_spec$freq
burg_norm_power = normalized_index_spec
burg_norm_longterm_rm_scale = nino3_index_longterm_rm_spec$freq
burg_norm_longterm_rm_power = normalized_index_spec_longterm_rm

#print(nino3_data$nino3_longterm_rm[400:450]) ###Testing for bug
#print(burg_norm_longterm_rm_power[400:450]) ###Testing for bug

save(morlet_Wit09_power,morlet_Wit09_scale,morlet_TW99_scale,morlet_TW99_power,
	burg_norm_scale,burg_norm_power,burg_norm_longterm_rm_scale,burg_norm_longterm_rm_power,
	file = paste("model_run_spectra/",model_run,"_spectra_",start_year,end_year,"_",region,".RData",sep=""))
#detach(nino3_data)
}
###########################

