;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

;********************************************************
;First form completed 11/7/14
;Description:
;-takes in monthly SST data from an ensemble
;-averages the SST over a region.
;-does a running annual mean of this time series and
;   subtracts a 211-month triangle-smoothed background to
;   get anomaly values.
;-finally, it writes new time series to .txt for R analysis.
;*********************************************************

begin

;***********************************;
;**********INPUT VARIABLES**********;
start_year = 0.
end_year = 100.
buffer = 15. ;years to left and right we include (needed because we have triangle smoothing below, but doesn't set triangle smoothing width, I dunno why I did it this way)
buff_start = start_year-buffer+0.01
buff_end = end_year+buffer+0.01
region = "nino34" ;choices are nino3 and nino34 currently
;***********************************;

 
forced_runs = (/"43","44","45","46","47",\
        "48","49","50","51","52","53","54","55",\
        "56","57","58","59","60","61","62","63",\
;forced_runs = (/"60","61","62","63",\
        "64","65","66","67","68","69","70","71",\
        "72","73","74","75","76","77","78","79",\
      "80","81","82","83","84","85","86","87",\
        "88","89","90","91"/)
;forced_runs=(/"42","43","44"/)
dirsize = dimsizes(forced_runs)

do i=1,dirsize-2

print("Starting member "+forced_runs(i))
;### Read in SST File ###
  model_run = forced_runs(i)
  f0 = addfile("~/a/enso_data/cesm_control/control_"+forced_runs(i-1)+"00.nc","r")
  f=addfile("~/a/enso_data/cesm_control/control_"+model_run+"00.nc","r")
  f2 = addfile("~/a/enso_data/cesm_control/control_"+forced_runs(i+1)+"00.nc","r")

;### Reformat time to regular calendar year
  sst=f->temp(:,0,:,:)
  sst0=f0->temp(:,0,:,:)
  sst2=f2->temp(:,0,:,:)
  time_in=f->time
  time_in0=f0->time
  time_in2=f2->time
  time=doubletofloat(time_in/365.-time_in(0)/365+31./365.) ;time(0) is Feb. 1
  time0=doubletofloat(time_in0/365.-time_in0(0)/365+31./365.)-100.
  time2=doubletofloat(time_in2/365.-time_in2(0)/365+31./365.)+100.

  sst&time=time
  sst0&time=time0
  sst2&time=time2

;### Append ssts
  sst_med = array_append_record(sst0,sst,0)
  sst_big = array_append_record(sst_med,sst2,0)
print("Appended array created, starting detrending")

;### Calculate Monthly Climatology ###
  sst_climatology=clmMonLLT(sst_big(lat|:,lon|:,{time|start_year+0.01:end_year+0.01}))

;### Calculate Monthly SST anomalies referenced to Climatology ###
  sst_clim_anom=calcMonAnomLLT(sst_big(lat|:,lon|:,{time|buff_start:buff_end}),sst_climatology)

;### Average monthly SST anomalies within  Nino 3.4 Region ### 
if (region .eq. "nino34") then
  regional_sst_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
  regional_sst=dim_avg_Wrap(dim_avg_Wrap(sst_big({time|buff_start:buff_end},{lat|-5:5},{lon|190:240})))
;### Let's check the Nino 3 data, for comparison w/ Wittenberg and IPCC
else if (region .eq. "nino3") then
  regional_sst_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|210:270})))
  regional_sst=dim_avg_Wrap(dim_avg_Wrap(sst_big({time|buff_start:buff_end},{lat|-5:5},{lon|210:270})))
end if
end if

;### Running annual mean SST of Nino 3.4
regional_sst_annual_smoothed= runave_Wrap(regional_sst,12,0)

;### Triangle smoother weights
;smoothing_width = 211 ;in months, should be odd
smoothing_width = 213 ;trying this out as possible explanation of diff wrt wittenberg.
peak = (smoothing_width-1)/2+1
weights1 = fspan(0,peak-1,peak)/peak
weights2 = fspan(peak,0,peak+1)/peak
weights=array_append_record(weights1,weights2,0)
;###Apply 211-month triangle smoother to regional SST data
  regional_sst_triangle_smoothed=wgt_runave_Wrap(regional_sst,weights,0)

  
;### Create the SST anomaly data ###
sst_anomaly = regional_sst_annual_smoothed - regional_sst_triangle_smoothed
;### raw sst with long term trend removed
sst_longterm_rm = regional_sst - regional_sst_triangle_smoothed
;### nino3 stat (or nino34 stat) with long term trend removed
regional_stat_longterm_rm = regional_sst_clim_anom - regional_sst_triangle_smoothed +\
		avg(regional_sst_triangle_smoothed({time|start_year+0.01:end_year+0.01}))
;#		avg(regional_sst_triangle_smoothed)

;### Print time series to table for spectral analysis in R ###
header = (/"time   "+region+"_sst   "+region+"_climate_anom   "+region+"_smoothed_anom" + \
	"   sst_longterm_rm   "+region+"_longterm_rm" /)
hlist = [/header/]
time_var = sst_clim_anom&time
alist = [/time_var,regional_sst,regional_sst_clim_anom,sst_anomaly,\
	sst_longterm_rm,regional_stat_longterm_rm/]
file_name = "model_time_series/control"+model_run+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)+".txt"
write_table(file_name,"w",hlist,"%s")
write_table(file_name,"a",alist,"%10.6f  %10.6f  %10.6f  %10.6f " +\
	"%10.6f %10.6f")

end do
end
