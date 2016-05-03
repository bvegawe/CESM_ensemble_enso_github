;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

;********************************************************
;First form completed 11/7/14
;Description:
;-takes in monthly SST data from CESM ensemble
;-averages the SST over the NINO-3.4 region.
;-does a running annual mean of this time series
;*********************************************************

begin

;### Read in SST File ###
  ensemble_run = "43"
  f=addfile("~/a/enso_data/forced_ensemble/forcedto2100_"+ensemble_run+"00.nc","r")
  ;f=addfile("/data/rsriver/a/rsriver/doe_piamddi/data/cesm_ens/monthly/TS_out/TS_"+ensemble_run+"00.nc","r")
  ;f=addfile("~/a/enso_data/forced_ensemble/forcedtest_"+ensemble_run+"00.nc","r") 
   sst=f->temp(:,0,:,:)
    ;sst = f->TS
   time_in=f->time

;### Reformat time to regular calendar year
  ;;time=doubletofloat(cd_calendar(time_in,4))
  time = doubletofloat(time_in/365.)
   ;time=doubletofloat(time_in/365.)+1850.


;### Reassign time coordinate variable for sst
  sst&time=time
    ;printVarSummary(sst)
  
;### Get the Monthly SST data from Nino 3.4 ###
    sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
    ;sst_nino34=sst_nino34-273.15
;### Let's check the Nino 3 data, for comparison w/ Wittenberg 2009
   ; sst_nino34=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|210:270})))

;### Running annual mean SST of Nino 3.4
    sst_nino34_annual_smoothed = runave_Wrap(sst_nino34,12,0)

;### Plot Results ###
  wks   = gsn_open_wks ("pdf", "forcedto2100_"+ensemble_run+"00_nino34");"test_nino34" ) ; Define Workstation
  res                 = True   ; plot mods desired

;### Specify Plot Limits ###
  res@trYMinF = 21.0
  res@trYMaxF = 29.0
  ;res@trXMinF = 1960
  res@trXMinF = 1850
  ;res@trXMaxF = 2015
  res@trXMaxF = 2105

;### Plot Titles ###
  res@tiMainFontHeightF  = 0.018
  res@tiYAxisFontHeightF = 0.014
  res@tiMainString = "Model Nino3.4 SST" ;"Nino3 Monthly SST Anomalies"
  res@tiYAxisString = "Degrees Celsius"

;### Specify Aspect ratio ###
  res@vpHeightF= 0.2;0.1
  res@vpWidthF = 0.8

data = new((/2,dimsizes(sst_nino34)/),float)
data(0,:) = sst_nino34
data(1,:) = sst_nino34_annual_smoothed
res@xyLineColors = (/"blue","red"/)
res@xyLineThicknesses = (/1.0,4.0/)

plot = gsn_csm_xy(wks,time,data,res)

end
