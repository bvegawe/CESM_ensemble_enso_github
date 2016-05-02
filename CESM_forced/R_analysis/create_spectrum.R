######################################################
#  file: create_spectrum.R
#  Ben Vega-Westhoff
#  Ryan Sriver
#   - Creating spectrum for a time series
#	- load in CESM model monthly ENSO data
#	- create power spectra
#	- pass 
#  Run in R (open R w/ >R on command line)
#  >source("multiple_run_spectra.R")
######################################################

library("stats")
library("biwavelet") #for morlet wavelet analysis

create_spectrum <- function(time_series_file,start_year,end_year,spectrum_type="burg",region="nino34"){
##time_series_file - location/name of time series file,
##	columns are time,nino3_sst,nino3_climate_anom,
## 	nino3_smoothed_anom,sst_longterm_rm,nino3_longterm_rm
##start_year,end_year - range for spectrum
##spectrum_type - choices are burg (default), morlet_wit, morlet_tw 

nino3_data = read.table(time_series_file,header=TRUE)
attach(nino3_data)

if(region=="nino3"){
used_indices = which(time>start_year & time<end_year+0.001)
start_time = start_year+31./365
nino3_sst_ts <- ts(nino3_sst[used_indices],frequency=12,start=c(start_time,1))
nino3_index_ts <- ts(nino3_climate_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts <- ts(nino3_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_ts <- ts(sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts <- ts(nino3_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts_clipped <- ts(nino3_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_clipped <- ts(sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts_clipped <- ts(nino3_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
}

if(region=="nino34"){
used_indices = which(time>start_year & time<end_year+0.001)
start_time = start_year+31./365
nino3_sst=nino34_sst
nino3_sst_ts <- ts(nino34_sst[used_indices],frequency=12,start=c(start_time,1))
nino3_index_ts <- ts(nino34_climate_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts <- ts(nino34_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_ts <- ts(sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts <- ts(nino34_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_smoothed_anom_ts_clipped <- ts(nino34_smoothed_anom[used_indices],frequency=12,start=c(start_time,1))
nino3_sst_longterm_rm_clipped <- ts(sst_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
nino3_index_longterm_rm_ts_clipped <- ts(nino34_longterm_rm[used_indices],frequency=12,start=c(start_time,1))
}

if (spectrum_type =="burg"){
## Generate Burg max entropy power spectrum, IPCC style
nino3_sst_spec = spec.ar(nino3_sst_ts,1000,method="burg")
nino3_index_spec = spec.ar(nino3_index_ts,1000,method = "burg") #units: power/[unit freq (yr^-1)]
smoothed_anom_spec = spec.ar(nino3_smoothed_anom_ts_clipped,1000,method = "burg")
nino3_index_longterm_rm_spec = spec.ar(nino3_index_longterm_rm_ts_clipped,1000,method="burg")
period = 12./nino3_index_spec$freq #units: months
smoothed_period = 12./smoothed_anom_spec$freq
##normalize such that area under our spectrum = 1.
#area_index = sum(nino3_index_spec$spec*(nino3_index_spec$freq[9]-nino3_index_spec$freq[8]))
#area_smoothed = sum(smoothed_anom_spec$spec*(smoothed_anom_spec$freq[9]-smoothed_anom_spec$freq[8]))
#area_sst = sum(nino3_sst_spec$spec*(nino3_sst_spec$freq[9]-nino3_sst_spec$freq[8]))
#area_index_longterm_rm = sum(nino3_index_longterm_rm_spec$spec*(nino3_index_longterm_rm_spec$freq[9]-nino3_index_longterm_rm_spec$freq[8]))
#normalized_index_spec = nino3_index_spec$spec*nino3_index_spec$freq/area_index
#normalized_index_spec_longterm_rm = nino3_index_longterm_rm_spec$spec*nino3_index_longterm_rm_spec$freq/area_index_longterm_rm
#scale = nino3_index_longterm_rm_spec$freq
#power = normalized_index_spec_longterm_rm

#only normalize such that we multiply power by frequency
scale = nino3_index_longterm_rm_spec$freq
power = nino3_index_longterm_rm_spec$spec*nino3_index_longterm_rm_spec$freq
}

if (spectrum_type =="morlet_tw"){
## Generate Morlet power spectrum, TW '99 style
time_smoothed = start_year+0.038+1./12*c(1:length(used_indices)-1)
ts = array(c(time_smoothed,nino3_sst[used_indices]),dim=c(length(time_smoothed),2))
sst_wt= wt(ts)
sst_gws = rowMeans(sst_wt$power)
scale = sst_wt$scale
power = sst_gws
}

if (spectrum_type =="morlet_wit"){
## Generate Morlet power spectrum, Wittenberg '09 style
wit_indices = (time > start_year) & (time < end_year)
time_wit = time[wit_indices]
time_wit_smoothed = time_wit[1] + 1./12 * c(1:length(time_wit)-1)
nino3_sst_wit = nino3_sst[wit_indices]
ts_wit = array(c(time_wit_smoothed,nino3_sst_wit),dim=c(length(time_wit_smoothed),2))
sst_wt_wit = wt(ts_wit,pad=FALSE)
scale_wit = sst_wt_wit$scale
sst_gws_wit = rowMeans(sst_wt_wit$power)
norm_gws_wit = sst_gws_wit/scale_wit/12.
scale = scale_wit
power = norm_gws_wit
}

detach(nino3_data)
return(list(scale,power))
}
###########################

