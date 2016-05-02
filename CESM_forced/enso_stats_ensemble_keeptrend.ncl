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
;   *DOES NOT* remove a 211-month triangle-smoothed background to
;   get anomaly values.
;-computes statistics for 20-year or 50-year running windows of the 
;   anomaly time series.
;-finally, it writes those statistics to 1 file per run.
;*********************************************************

begin

window_size = 50*12 ;years for window
ensemble = "ersst" ;choices are forced, control, and ersst
region = "nino34" ;choices are nino3 and nino34 currently
longterm_rm = False ;if true, remove the 211month triangle smoothing
hyun_climo = True ;if true, do climatology in the window, not at the beginning

if (window_size .eq. 50*12) then
  window_str = "50yr_"
else if (window_size .eq. 20*12) then
  window_str = ""
end if
end if

if (longterm_rm .eq. True) then
  trend_str = "woTrend"
else if (longterm_rm .eq. False) then
  trend_str = "wTrend"
end if
end if

if (hyun_climo .eq. True) then
  climo_str = "_hyun"
else if (hyun_climo .eq. False) then
  climo_str = ""
end if 
end if

if (ensemble .ne. "ersst") then
forced_runs = (/"42", "43", "44","45","46","47",\
       "48","49","50","51","52","53","54","55",\
        "56","57","58","59","60","61","62","63",\
        "64","65","66","67","68","69","70","71",\
        "72","73","74","75","76","77","78","79",\
       "80","81","82","83","84","85","86","87",\
        "88","89","90","91"/)
;forced_runs = (/"42","43"/)
end if

if (ensemble .eq. "ersst") then
  forced_runs = (/"ersst"/)
end if

do j=0,dimsizes(forced_runs)-1
;### Read in SST File ###
  model_run = forced_runs(j)
  print(model_run)
  if (ensemble .eq. "forced") then
    f=addfile("~/a/enso_data/forced_ensemble/forcedto2100_"+model_run+"00.nc","r")
  else if (ensemble .eq. "control") then
    f=addfile("~/a/enso_data/cesm_control/control_"+model_run+"00.nc","r")
  else if (ensemble .eq. "ersst") then
    f=addfile("~/a/enso_data/ersst_cat.nc","r")
  end if
  end if
  end if
  
  
  if (ensemble .eq. "ersst") then
    sst=f->sst(:,0,:,:)
  else
    sst=f->temp(:,0,:,:)
  end if
    time_in=f->time

;### Reformat time to regular calendar year
  if (ensemble .eq. "control") then
    time=doubletofloat(time_in/365.-time_in(0)/365+31./365.) ;time(0) is Feb. 1
  else if(ensemble .eq. "forced") then
    time=doubletofloat(time_in/365.)
  else if(ensemble .eq. "ersst") then
    time=doubletofloat(cd_calendar(time_in,4))
  end if
  end if
  end if

;### Reassign time coordinate variable for sst
  sst&time=time
    ;printVarSummary(sst)

;### Average monthly SST anomalies within  Nino 3.4 Region ###
if (region .eq. "nino34") then
  sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
else if (region .eq. "nino3") then
  sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|210:270})))
end if
end if

if (hyun_climo .eq. False) then
;### Calculate Monthly Climatology ###
  if (ensemble .eq. "forced") then 
    t0 = 1851.01
    tf = 2100.01
  else if(ensemble .eq. "control") then
    t0 = 0.01
    tf = 100.01
  else if(ensemble .eq. "ersst") then
    t0 = 1854.01
    tf = 2014.01
  end if
  end if
  end if
  sst_climatology=clmMonLLT(sst(lat|:,lon|:,{time|t0:tf}))

;### Calculate Monthly SST anomalies referenced to Climatology ###
  sst_clim_anom=calcMonAnomLLT(sst(lat|:,lon|:,time|:),sst_climatology)

  if (region .eq. "nino34") then
    sst_nino34_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
  else if (region .eq. "nino3") then
    sst_nino34_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|210:270})))
  end if
  end if

else if (hyun_climo .eq. True) then
  sst_nino34_clim_anom = sst_nino34
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
if (longterm_rm .eq. True) then
  nino34_longterm_rm = sst_nino34_clim_anom-sst_nino34_triangle_smoothed +\
                       avg(sst_nino34)
end if  
if (longterm_rm .eq. False) then
   nino34_longterm_rm = sst_nino34_clim_anom ;not removing longterm trend
end if

;### First, on 20-year sliding window
;window_size = 20*12
i=window_size/2+1
stats20 = new((/dimsizes(time),4/),float)
do while(i.le.dimsizes(time)-window_size/2-1)
  ;window = sst_anomaly(i-window_size/2:i+window_size/2-1)
  window = nino34_longterm_rm(i-window_size/2:i+window_size/2-1)
  
  if (hyun_climo .eq. True) then
    window = rmAnnCycle1D(window)
  end if
  
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
if (ensemble .ne. "ersst") then
  file_name = "model_running_stats/model"+model_run+"_"+ensemble+"_"+region+"_stats_"+window_str+trend_str+climo_str+".txt"
else if (ensemble .eq. "ersst") then
  file_name = "model_running_stats/ersst_"+region+"_stats_"+window_str+trend_str+climo_str+".txt"
end if
end if

write_table(file_name,"w",hlist,"%s")
write_table(file_name,"a",alist,"%10.6f  %10.6f  %10.6f  %10.6f " +\
        "%10.6f %10.6f %10.6f %10.6f")
end do

end
