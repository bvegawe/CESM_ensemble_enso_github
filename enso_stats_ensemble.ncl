;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

;********************************************************
;First form completed 11/7/14
;Description:
;-takes in monthly SST data from the CESM forced runs
;-averages the SST over the NINO-3.4 region.
;-does a running annual mean of this time series and
;   subtracts a 211-month triangle-smoothed background to
;   get anomaly values.
;-computes statistics for 20-year running windows of the 
;   anomaly time series.
;-finally, it writes those statistics to 1 file per run.
;*********************************************************

begin

ensemble = "control" ;choices are forced and control
region = "nino3" ;choices are nino3 and nino34 currently

forced_runs = (/"42", "43", "44","45","46","47",\
       "48","49","50","51","52","53","54","55",\
        "56","57","58","59","60","61","62","63",\
        "64","65","66","67","68","69","70","71",\
        "72","73","74","75","76","77","78","79",\
       "80","81","82","83","84","85","86","87",\
        "88","89","90","91"/)
;forced_runs = (/"42","43"/)
do j=0,dimsizes(forced_runs)-1
;### Read in SST File ###
  model_run = forced_runs(j)
  print(model_run)
  if (ensemble .eq. "forced") then
    f=addfile("~/a/enso_data/forced_ensemble/forcedto2100_"+model_run+"00.nc","r")
  else if (ensemble .eq. "control") then
    f=addfile("~/a/enso_data/cesm_control/control_"+model_run+"00.nc","r")
  end if
  end if
  
    sst=f->temp(:,0,:,:)
    time_in=f->time

;### Reformat time to regular calendar year
  if (ensemble .eq. "control") then
    time=doubletofloat(time_in/365.-time_in(0)/365+31./365.) ;time(0) is Feb. 1
  else if(ensemble .eq. "forced") then
    time=doubletofloat(time_in/365.)
  end if
  end if

;### Reassign time coordinate variable for sst
  sst&time=time
    ;printVarSummary(sst)

;### Calculate Monthly Climatology ###
  if (ensemble .eq. "forced") then 
    t0 = 1851.01
    tf = 2100.01
  else if(ensemble .eq. "control") then
    t0 = 0.01
    tf = 100.01
  end if
  end if
  sst_climatology=clmMonLLT(sst(lat|:,lon|:,{time|t0:tf}))

;### Calculate Monthly SST anomalies referenced to Climatology ###
  sst_clim_anom=calcMonAnomLLT(sst(lat|:,lon|:,time|:),sst_climatology)

;### Average monthly SST anomalies within  Nino 3.4 Region ### 
if (region .eq. "nino34") then
  sst_nino34_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
  sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
;### Let's check the Nino 3 data, for comparison w/ Wittenberg and IPCC
else if (region .eq. "nino3") then
  sst_nino34_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|210:270})))
  sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|210:270})))
end if
end if

;### Running annual mean SST of Nino 3.4
    sst_nino34_annual_smoothed = runave_Wrap(sst_nino34,12,0)

;### Triangle smoother weights
;smoothing_width = 211 ;in months, should be odd
smoothing_width = 213 ;trying this out as possible explanation of diff wrt wittenberg.
peak = (smoothing_width-1)/2+1
weights1 = fspan(0,peak-1,peak)/peak
weights2 = fspan(peak,0,peak+1)/peak
weights=array_append_record(weights1,weights2,0)
;###Apply 211-month triangle smoother to Nino3.4 SST data
  sst_nino34_triangle_smoothed=wgt_runave_Wrap(sst_nino34,weights,0)

;### Do stats on the SST anomaly data ###
sst_anomaly = sst_nino34_annual_smoothed - sst_nino34_triangle_smoothed
;### raw sst with long term trend removed
sst_longterm_rm = sst_nino34 - sst_nino34_triangle_smoothed
;### nino3 stat (or nino34 stat) with long term trend removed
nino34_longterm_rm = sst_nino34_clim_anom - sst_nino34_triangle_smoothed +\
;		avg(sst_nino34_triangle_smoothed({time|t0:tf})) ;should use the climatology time range I think
		avg(sst_nino34)		
;		avg(sst_nino34_triangle_smoothed)
;nino34_longterm_rm = sst_nino34_clim_anom

;### First, on 20-year sliding window
window_size = 20*12
i=window_size/2+1
stats20 = new((/dimsizes(time),4/),float)
do while(i.le.dimsizes(time)-window_size/2-1)
  ;window = sst_anomaly(i-window_size/2:i+window_size/2-1)
  window = nino34_longterm_rm(i-window_size/2:i+window_size/2-1)
  stats20(i,:) = dim_stat4(window)
  i=i+1
end do
last_stats20 = i-1
;###stats array has avg,variance,skewness,kurtosis

;### Print time series to table for spectral analysis in R ###
header = (/"time   nino3_sst   nino3_climate_anom   " + \
        "  nino3_longterm_rm   running_avg        running_variance" +\
	"     running_skewness    running_kurtosis"/)
hlist = [/header/]
alist = [/time,sst_nino34,sst_nino34_clim_anom,\
        nino34_longterm_rm,stats20(:,0),stats20(:,1),\
	stats20(:,2),stats20(:,3)/]
file_name = "model_running_stats/model"+model_run+"_"+ensemble+"_"+region+"_stats.txt"
write_table(file_name,"w",hlist,"%s")
write_table(file_name,"a",alist,"%10.6f  %10.6f  %10.6f  %10.6f " +\
        "%10.6f %10.6f %10.6f %10.6f")
end do

end
