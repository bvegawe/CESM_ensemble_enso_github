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
ensemble = "cmip5" ;choices are forced, control, cmip5, and ersst

;start_year = 25.
;end_year = 75.
start_year = 1940. ;these are the years over which we do the climatology
end_year = 1990.   ;control starts at year 0, forced at 1851
;start_year = 1865.
;end_year = 2085.

region = "nino34" ;choices are nino3 and nino34 currently. cmip5 only nino34 atm
;***********************************;

if ((ensemble .eq. "control") .or. (ensemble .eq. "forced")) then 
;forced_runs = (/"42","43","44","45","46","47",\
;        "48","49","50","51","52","53","54","55",\
;        "56","57","58","59","60","61","62","63",\
;;forced_runs = (/"60","61","62","63",\
;        "64","65","66","67","68","69","70","71",\
;        "72","73","74","75","76","77","78","79",\
;      "80","81","82","83","84","85","86","87",\
;        "88","89","90","91"/)
forced_runs=(/"42"/)
end if

if (ensemble .eq. "ersst") then
  forced_runs = (/"ersst"/)
end if

if (ensemble .eq. "cmip5") then
  forced_runs = ispan(0,34,1)
end if

dirsize = dimsizes(forced_runs)

if (ensemble .ne. "cmip5") then
do i=0,dirsize-1

;### Read in SST File ###
  model_run = forced_runs(i)
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

    ;printVarSummary(sst)  

;### Reassign time coordinate variable for sst
  sst&time=time
    ;printVarSummary(sst)

;### Calculate Monthly Climatology ###
  sst_climatology=clmMonLLT(sst(lat|:,lon|:,{time|start_year+0.01:end_year+0.01}))

;### Calculate Monthly SST anomalies referenced to Climatology ###
  sst_clim_anom=calcMonAnomLLT(sst(lat|:,lon|:,time|:),sst_climatology)

;### Average monthly SST anomalies within  Nino 3.4 Region ### 
if (region .eq. "nino34") then
  regional_sst_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
  regional_sst=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
;### Let's check the Nino 3 data, for comparison w/ Wittenberg and IPCC
else if (region .eq. "nino3") then
  regional_sst_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|210:270})))
  regional_sst=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|210:270})))
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
;		avg(regional_sst_triangle_smoothed)

;### Print time series to table for spectral analysis in R ###
header = (/"time   "+region+"_sst   "+region+"_climate_anom   "+region+"_smoothed_anom" + \
	"   sst_longterm_rm   "+region+"_longterm_rm" /)
hlist = [/header/]
alist = [/time,regional_sst,regional_sst_clim_anom,sst_anomaly,\
	sst_longterm_rm,regional_stat_longterm_rm/]
if (ensemble .eq. "control") then
  file_name = "model_time_series/control"+model_run+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)+".txt"
else if (ensemble .eq. "forced") then
  file_name = "model_time_series/model"+model_run+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)+".txt"
else if (ensemble .eq. "ersst") then
  file_name = "model_time_series/ersst"+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)+".txt"
end if
end if
end if
write_table(file_name,"w",hlist,"%s")
write_table(file_name,"a",alist,"%10.6f  %10.6f  %10.6f  %10.6f " +\
	"%10.6f %10.6f")

end do
end if

if (ensemble .eq. "cmip5") then
do i= 0,dirsize-1
  print("CMIP5 ensemble member "+i+"/"+dirsize)
  if (region .eq. "nino34") then
    root = "/data/rsriver/a/bvegawe2/enso_data/cmip5/NINO34/"
    if (i .le. 9) then
      f = addfile(root+"/icmip5_tos_Omon_one_rcp85_190-240E_-5-5N_n_su_00"+i+".nc","r")
    else if (i .gt. 9) then
      f = addfile(root+"/icmip5_tos_Omon_one_rcp85_190-240E_-5-5N_n_su_0"+i+".nc","r")
  end if
  end if
  end if

  time = f->time/12.+1861.
  in_time_period = ind((time .ge. start_year) .and. (time .lt. end_year-0.01))
  regional_sst = f->tos
  regional_sst&time = time

  regional_clim_anom = rmAnnCycle1D(regional_sst({time|start_year:end_year-0.01}))

;### Triangle smoother weights
  smoothing_width = 213 ;trying this out as possible explanation of diff wrt wittenberg.
  peak = (smoothing_width-1)/2+1
  weights1 = fspan(0,peak-1,peak)/peak
  weights2 = fspan(peak,0,peak+1)/peak
  weights=array_append_record(weights1,weights2,0)
;###Apply 211-month triangle smoother to regional SST data
  regional_sst_triangle_smoothed=wgt_runave_Wrap(regional_sst,weights,0)

;### nino3 stat (or nino34 stat) with long term trend removed
  regional_stat_longterm_rm = regional_clim_anom({time|start_year:end_year-0.01}) -\
             regional_sst_triangle_smoothed({time|start_year:end_year-0.01}) +\
             avg(regional_sst_triangle_smoothed({time|start_year:end_year-0.01}))
  sst_longterm_rm = regional_sst({time|start_year:end_year-0.01}) -\
             regional_sst_triangle_smoothed({time|start_year:end_year-0.01})
  regional_sst_annual_smoothed= runave_Wrap(regional_sst,12,0)
  sst_anomaly = regional_sst_annual_smoothed({time|start_year:end_year-0.01}) -\
              regional_sst_triangle_smoothed({time|start_year:end_year-0.01})

;### Print time series to table for spectral analysis in R ###
header = (/"time   "+region+"_sst   "+region+"_climate_anom   "+region+"_smoothed_anom" + \
        "   sst_longterm_rm   "+region+"_longterm_rm" /)
hlist = [/header/]
alist = [/time(in_time_period),regional_sst({time|start_year:end_year-0.01}),\
        regional_clim_anom,sst_anomaly,\
        sst_longterm_rm,regional_stat_longterm_rm/]
  file_name = "cmip5_time_series/cmip5"+"_"+i+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)+".txt"
write_table(file_name,"w",hlist,"%s")
write_table(file_name,"a",alist,"%10.6f  %10.6f  %10.6f  %10.6f " +\
        "%10.6f %10.6f")

end do
end if
;;####Just plotting once, remove after
;wks = gsn_open_wks("pdf","run42_trend_removed_nino34")
;res = True
;res@trYMinF = -5
;res@trYMaxF = 5
;res@trXMinF = 1865
;res@trXMaxF = 2085

;;##plot titles
;res@tiMainFontHeightF = 0.015
;res@tiYAxisFontHeightF = 0.012
;res@tiMainString = "Nino3.4 Index"
;res@tiYAxisString = "Anomalous T (~S~o~N~C)"
;;##aspect ratio
;res@vpHeightF = 0.11
;res@vpWidthF = 0.7
;;##
;res@gsnYRefLine = 0.0
;res@gsnXYAboveFillColors = "red"
;res@gsnXYBelowFillColors = "blue"
;plot_stack = new((/2,dimsizes(time)/),float)
;plot_stack(0,:) = regional_sst_clim_anom
;plot_stack(1,:) = regional_sst_triangle_smoothed - avg(regional_sst_triangle_smoothed)
;print(dimsizes(time))
;print(dimsizes(plot_stack(0,:)))
;print(dimsizes(plot_stack(1,:)))
;plot = gsn_csm_xy(wks,time,plot_stack,res)

end
