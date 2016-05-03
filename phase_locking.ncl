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
;-calculates std.dev. for each month in the time series,
;-plots an annual cycle, should get a winter peak if we have phase locking
;*********************************************************

begin

;***********************************;
;**********INPUT VARIABLES**********;
ensemble = "forced" ;choices are forced, control, and ersst

;start_year = 10.
;end_year = 90.
start_year = 1940. ;these are the years over which we do the climatology
end_year = 1990.   ;control starts at year 0, forced at 1851
;start_year = 1865.
;end_year = 2085.

region = "nino34" ;choices are nino3 and nino34 currently
compare_ersst = True ;if True, plot ersst on same curve
;***********************************;

if (ensemble .ne. "ersst") then 
forced_runs = (/"42","43","44","45","46","47",\
        "48","49","50","51","52","53","54","55",\
        "56","57","58","59","60","61","62","63",\
;;forced_runs = (/"60","61","62","63",\
       "64","65","66","67","68","69","70","71",\
        "72","73","74","75","76","77","78","79",\
      "80","81","82","83","84","85","86","87",\
        "88","89","90","91"/)
;forced_runs=(/"44"/)
end if

if (ensemble .eq. "ersst") then
  forced_runs = (/"ersst"/)
end if

dirsize = dimsizes(forced_runs)

phased_std = new((/dirsize,12/),float)
month = ispan(1,12,1)

do i=0,dirsize-1
print("Now on run "+i+"/"+dirsize)
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
regional_stat_longterm_rm = regional_sst_clim_anom({time|start_year+0.01:end_year+0.01}) - regional_sst_triangle_smoothed({time|start_year+0.01:end_year+0.01}) +\
                avg(regional_sst_triangle_smoothed({time|start_year+0.01:end_year+0.01}))

windowed_stat = regional_stat_longterm_rm
window_indices = ispan(0,dimsizes(windowed_stat)-1,1)

do j=0,11 
  in_phase = ind(mod(window_indices,12) .eq. j) 
  phased_std(i,j) = stddev(windowed_stat(in_phase))
end do

end do

avg_std = dim_avg_n(phased_std,0)

;Overplot ERSST phased st.dev.
if (compare_ersst .eq. True) then
  print("Now on ERSST")
  f=addfile("~/a/enso_data/ersst_cat.nc","r")
  delete([/sst,time_in,time,sst_climatology,sst_clim_anom,regional_sst_clim_anom, \
        regional_sst,regional_sst_annual_smoothed,regional_sst_triangle_smoothed, \
        regional_stat_longterm_rm,windowed_stat,window_indices/])
  sst=f->sst(:,0,:,:)
  time_in=f->time
  time=doubletofloat(cd_calendar(time_in,4))
  sst&time=time
  sst_climatology=clmMonLLT(sst(lat|:,lon|:,{time|start_year+0.01:end_year+0.01}))
  sst_clim_anom=calcMonAnomLLT(sst(lat|:,lon|:,time|:),sst_climatology)
  if (region .eq. "nino34") then
    regional_sst_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
    regional_sst=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
  else if (region .eq. "nino3") then
    regional_sst_clim_anom=dim_avg_Wrap(dim_avg_Wrap(sst_clim_anom(time|:,{lat|-5:5},{lon|210:270})))
    regional_sst=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|210:270})))
  end if
  end if
  regional_sst_annual_smoothed= runave_Wrap(regional_sst,12,0)
  smoothing_width = 213 ;trying this out as possible explanation of diff wrt wittenberg.
  peak = (smoothing_width-1)/2+1
  weights1 = fspan(0,peak-1,peak)/peak
  weights2 = fspan(peak,0,peak+1)/peak
  weights=array_append_record(weights1,weights2,0)
  regional_sst_triangle_smoothed=wgt_runave_Wrap(regional_sst,weights,0)
  regional_stat_longterm_rm = regional_sst_clim_anom({time|start_year+0.01:end_year+0.01}) -\
                regional_sst_triangle_smoothed({time|start_year+0.01:end_year+0.01}) +\
                avg(regional_sst_triangle_smoothed({time|start_year+0.01:end_year+0.01}))
  windowed_stat = regional_stat_longterm_rm
  window_indices = ispan(0,dimsizes(windowed_stat)-1,1)
  ersst_phased_std = new(12,float)
  do j=0,11
    in_phase = ind(mod(window_indices,12) .eq. j)
    ersst_phased_std(j) = stddev(windowed_stat(in_phase))
  end do
end if

if (ensemble .eq. "control") then
  file_name = "phase_locking/control"+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)
else if (ensemble .eq. "forced") then
  file_name = "phase_locking/model"+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)
else if (ensemble .eq. "ersst") then
  file_name = "phase_locking/ersst"+"_"+region+"_"+flt2string(start_year)+flt2string(end_year)
end if
end if
end if

;##Customizing plot parameters
wks = gsn_open_wks("pdf",file_name)
res = True
res@trYMinF = 0.25
res@trYMaxF = 1.2
res@trXMinF = 1
res@trXMaxF = 12
res@tmXBMode = "Explicit"
res@tmXBValues = (/1,2,3,4,5,6,7,8,9,10,11,12/)
res@tmXBLabels = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"/)
res@xyLineThicknesses = (/4.0,4.0/)
res@xyLineColors = (/"black","red"/)
res@xyDashPattern = 0

;##plot titles
res@tiMainFontHeightF = 0.025
;res@tiYAxisFontHeightF = 0.018
;res@tmXBLabelFontHeightF = 0.018
res@tiMainString = "Annual variations of Nino3.4 Index st. d."
res@tiYAxisString = "St. d. (~S~o~N~C)"
res@tiXAxisString = ""
;##aspect ratio
res@vpHeightF = 0.5
res@vpWidthF = 0.7
;##
if (compare_ersst .eq. True) then
  data = new((/2,12/),float)
  data(0,:) = avg_std
  data(1,:) = ersst_phased_std
else if (compare_ersst .eq. False) then
  data = avg_std
end if
end if

plot = gsn_csm_xy(wks,month,data,res)


end
