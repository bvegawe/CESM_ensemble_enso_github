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
;-finally, it plots those statistics.
;*********************************************************

begin

;### Read in SST File ###
  model_run = "42"
  f=addfile("~/a/enso_data/forced_ensemble/forcedto2100_"+model_run+"00.nc","r")
    sst=f->temp(:,0,:,:)
    time_in=f->time

;### Reformat time to regular calendar year
  time=doubletofloat(time_in/365.)
    ;printVarSummary(sst)  

;### Reassign time coordinate variable for sst
  sst&time=time
    ;printVarSummary(sst)

;### Calculate Monthly Climatology ###
  ;sst_climatology=clmMonLLT(sst(lat|:,lon|:,{time|1960.01:2004.01}))
  t0 = 1850.01
  sst_climatology=clmMonLLT(sst(lat|:,lon|:,{time|t0:2100.01}))

;### Calculate Monthly SST anomalies referenced to Climatology ###
  sst_clim_anom=calcMonAnomLLT(sst(lat|:,lon|:,time|:),sst_climatology)

;### Average monthly SST anomalies within  Nino 3.4 Region ### 
  ;sst_nino34_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
;### Let's check the Nino 3 data, for comparison w/ Wittenberg and IPCC
  sst_nino34_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|210:270})))

;### Get the Monthly SST data from Nino 3.4 ###
   ;sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
;### Let's check the Nino 3 data, for comparison w/ Wittenberg 2009
   sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|210:270})))

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

;### Compare our time series of Nino3 data to that of Wittenberg 2009
;### Make stacked array for plotting more than one set of time series data. ###
  plot_stack= new((/2,dimsizes(time)/),float)
  plot_stack(0,:) = sst_nino34_annual_smoothed
  plot_stack(1,:) = sst_nino34_triangle_smoothed
  wks = gsn_open_wks("pdf","model"+model_run+"_3_smoothed_t_compare")
  res = True
  res@trYMinF = 22.0
  res@trYMaxF = 30.0
  res@trXMinF = 1870
  res@trXMaxF = 2008
  res@vpHeightF= 0.1;0.1
  res@vpWidthF = 0.8
  res@tiYAxisString = "Degrees Celsius"
  res@gsnXYAboveFillColors = "red"
  res@gsnXYBelowFillColors = "blue"
  res@gsnYRefLine           = avg(sst_nino34_triangle_smoothed)
  plot = gsn_csm_xy(wks,time,plot_stack,res)
  frame(wks)

  
;### Do stats on the SST anomaly data ###
sst_anomaly = sst_nino34_annual_smoothed - sst_nino34_triangle_smoothed
;### raw sst with long term trend removed
sst_longterm_rm = sst_nino34 - sst_nino34_triangle_smoothed
;### nino3 stat (or nino34 stat) with long term trend removed
nino34_longterm_rm = sst_nino34_clim_anom - sst_nino34_triangle_smoothed +\
;		avg(sst_nino34_triangle_smoothed({time|t0:2100.01})) ;should use the climatology time range
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
header = (/"time   nino3_sst   nino3_climate_anom   nino3_smoothed_anom" + \
	"   sst_longterm_rm   nino3_longterm_rm" /)
hlist = [/header/]
alist = [/time,sst_nino34,sst_nino34_clim_anom,sst_anomaly,\
	sst_longterm_rm,nino34_longterm_rm/]
write_table("model"+model_run+"_nino3.txt","w",hlist,"%s")
write_table("model"+model_run+"_nino3.txt","a",alist,"%10.6f  %10.6f  %10.6f  %10.6f " +\
	"%10.6f %10.6f")

;### Plot Results ###
  wks   = gsn_open_wks ("pdf", "model"+model_run+"_nino3_longterm_rm_stats");"test_nino34" ) ; Define Workstation
  res                 = True   ; plot mods desired

;### Specify Plot Limits ###
  res@trYMinF = 0.0
  res@trYMaxF = 1.0
  ;res@trXMinF = 1960
  res@trXMinF = 1850
  res@trXMaxF = 2015

;### Plot Titles ###
  res@tiMainFontHeightF  = 0.018
  res@tiYAxisFontHeightF = 0.014
  ;res@tiMainString = "Model Nino3.4 annual mean SST variance (20 year windows)" ;"Nino3 Monthly SST Anomalies"
  ;res@tiYAxisString = "Degrees Celsius ^2"

;### Specify Aspect ratio ###
  res@vpHeightF= 0.1;0.1
  res@vpWidthF = 0.8

;### Set shading above and below zero anomaly ###
  ;res@gsnYRefLine = avg(stats20(431:last_stats20,1))
  ;res@gsnAboveYRefLineColor = "red"              ; above ref line fill red
  ;res@gsnBelowYRefLineColor = "blue"             ; below ref line fill blue
   ;res@gsnYRefLine           = avg(sst_nino34_triangle_smoothed)  
	;#create a reference line
  ;res@gsnXYAboveFillColors = "red"
  ;res@gsnXYBelowFillColors = "blue"

;### Plot the stats (running avg, stddev, skewness) all on same page ###
  res@gsnFrame = False 
  res1 = res
  res2 = res
  res3 = res
  res4 = res

;first, anomalies
  res1@vpXF = 0.1
  res1@vpYF = 0.93
  res1@trYMinF = -3
  res1@trYMaxF = 3
  res1@tiMainString = "Nino3 statistic - 20-yr running avg"
  res1@tiYAxisString = "Degrees Celsius"
  res1@gsnYRefLine = 0.0
  res1@gsnAboveYRefLineColor="red"
  res1@gsnBelowYRefLineColor="blue"
  plot = gsn_csm_xy(wks,time,nino34_longterm_rm,res1)

;second, avg
  res2@vpXF = 0.1
  res2@vpYF = 0.68
  res2@trYMinF = -1.0;-0.2
  res2@trYMaxF = 1.0;0.2
  res2@tiMainString = "running avg anomaly"
  res2@tiYAxisString = "Degrees Celsius"
  res2@gsnYRefLine = 0.0
  res2@gsnAboveYRefLineColor="red"
  res2@gsnBelowYRefLineColor="blue"
  plot = gsn_csm_xy(wks,time,stats20(:,0),res2)

;third, stddev
  res3@vpXF = 0.1
  res3@vpYF = 0.43
  res3@trYMinF = 0
  res3@trYMaxF = 1
  res3@tiMainString = "running std. dev."
  res3@tiYAxisString = "Degrees Celsius"
  plot = gsn_csm_xy(wks,time,stats20(:,1)^0.5,res3)

;fourth, skewness
  res4@vpXF = 0.1
  res4@vpYF = 0.18
  res4@trYMinF = -1
  res4@trYMaxF = 1.5
  res4@tiMainString = "running skewness"
  res4@gsnYRefLine = 0.0
  res4@gsnAboveYRefLineColor="red"
  res4@gsnBelowYRefLineColor="blue"
  plot = gsn_csm_xy(wks,time,stats20(:,2),res4)

  frame(wks)

print(stats20(0:20,0))
print(time(0:20))
print(avg(sst_anomaly))

end
