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
;-averages the SST over 2 regions, region1 and region2,
;    in 2 time periods.
;-finds how the diff b/w the 2 regions changes from t1 to t2,
;-then, averages over a 3rd region, std_region
;-finds how the std in another region changes from t1 to t2.
;-plots the 2 things we just calculated.
;*********************************************************

begin

;***********************************;
;**********INPUT VARIABLES**********;
t1_start = 1940.  ;we do std_t2 - std_t1 and (r1-r2)_t2 - (r1-r2)_t1
t1_end = 1990.    
t2_start = 2040.
t2_end = 2090.

region1 = "WP"  ;currently only choice is Western Pacific, WP
region2 = "EP"  ;currently only choice is EP, eastern pacific
mean_state_change = "ratio" ;if ratio, do WP-EP(2040-2090)/WP-EP(1940-1990), if diff, subtract
std_region = "nino34" ;currently only choice is the nino34 region
std_change = "ratio" ;if ratio, do std(2040-2090)/std(1940-1990), if diff, subtract
plot_name = "mean_state_std_ratio" 
;***********************************;

forced_runs = (/"42","43","44","45","46","47",\
      "48","49","50","51","52","53","54","55",\
      "56","57","58","59","60","61","62","63",\
;forced_runs = (/"60","61","62","63",\
      "64","65","66","67","68","69","70","71",\
      "72","73","74","75","76","77","78","79",\
      "80","81","82","83","84","85","86","87",\
      "88","89","90","91"/)
;forced_runs=(/"42","43"/);,"44"/)

dirsize = dimsizes(forced_runs)

delta_mean_state = new(dirsize,float)
delta_std = new(dirsize,float)
ratio_mean_state = new(dirsize,float)
ratio_std = new(dirsize,float)

;#### First, calculate the change in the mean state ####
;#### (2040-2090 WP - EP) - (1940-1990 WP - EP) ####
do i=0,dirsize-1
  print("CESM ensemble, member "+i+"/"+dirsize)
  model_run = forced_runs(i)
  f=addfile("~/a/enso_data/forced_ensemble/forcedto2100_"+model_run+"00.nc","r")
  
  sst=f->temp(:,0,:,:)
  time_in=f->time

;### Reformat time to regular calendar year
  time=doubletofloat(time_in/365.)

;### Reassign time coordinate variable for sst
  sst&time=time

;### Average monthly SST anomalies within the 2 regions ###
  if (region1 .eq. "WP") then 
    region1_sst=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|120:170})))
  end if
  if (region2 .eq. "EP") then
    region2_sst=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|210:270})))
  end if

  r1_t1 = avg(region1_sst({time|t1_start+0.01:t1_end+0.01}))
  r1_t2 = avg(region1_sst({time|t2_start+0.01:t2_end+0.01}))
  r2_t1 = avg(region2_sst({time|t1_start+0.01:t1_end+0.01}))
  r2_t2 = avg(region2_sst({time|t2_start+0.01:t2_end+0.01}))

  delta_mean_state(i) = (r1_t2-r2_t2) - (r1_t1 - r2_t1)
  ratio_mean_state(i) = (r1_t2-r2_t2)/(r1_t1-r2_t1)
;#### Now, the change in standard deviation ####
;#### (2040-2090 std) - or / (1940-1990 std) ####

  if (std_region .eq. "nino34") then
    regional_sst = dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
  end if
  regional_t1_clim_anom = rmAnnCycle1D(regional_sst({time|t1_start+0.01:t1_end+0.01}))
  regional_t2_clim_anom = rmAnnCycle1D(regional_sst({time|t2_start+0.01:t2_end+0.01}))


;too slow...
;;### Calculate Monthly Climatology ###
;  t1_climatology=clmMonLLT(sst(lat|:,lon|:,{time|t1_start+0.01:t1_end+0.01}))
;  t2_climatology=clmMonLLT(sst(lat|:,lon|:,{time|t2_start+0.01:t2_end+0.01}))
;
;;### Calculate Monthly SST anomalies referenced to Climatology ###
;  t1_clim_anom=calcMonAnomLLT(sst(lat|:,lon|:,time|:),t1_climatology)
;  t2_clim_anom=calcMonAnomLLT(sst(lat|:,lon|:,time|:),t2_climatology)
;
;;### Average monthly SST anomalies within region ###
;  if (std_region .eq. "nino34") then
;    regional_t1_clim_anom=dim_avg_Wrap(dim_avg_Wrap(t1_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
;    regional_t2_clim_anom=dim_avg_Wrap(dim_avg_Wrap(t2_clim_anom(time|:,{lat|-5:5},{lon|190:240})))
;    regional_sst=dim_avg_Wrap(dim_avg_Wrap(sst(time|:,{lat|-5:5},{lon|190:240})))
;  end if

;### Triangle smoother weights
  smoothing_width = 213 ;trying this out as possible explanation of diff wrt wittenberg.
  peak = (smoothing_width-1)/2+1
  weights1 = fspan(0,peak-1,peak)/peak
  weights2 = fspan(peak,0,peak+1)/peak
  weights=array_append_record(weights1,weights2,0)
;###Apply 211-month triangle smoother to regional SST data
  regional_sst_triangle_smoothed=wgt_runave_Wrap(regional_sst,weights,0)

;### nino3 stat (or nino34 stat) with long term trend removed
  regional_stat_t1_longterm_rm = regional_t1_clim_anom({time|t1_start+0.01:t1_end+0.01}) -\
             regional_sst_triangle_smoothed({time|t1_start+0.01:t1_end+0.01}) +\
             avg(regional_sst_triangle_smoothed({time|t1_start+0.01:t1_end+0.01}))
  regional_stat_t2_longterm_rm = regional_t2_clim_anom({time|t2_start+0.01:t2_end+0.01}) -\
             regional_sst_triangle_smoothed({time|t2_start+0.01:t2_end+0.01}) +\
             avg(regional_sst_triangle_smoothed({time|t2_start+0.01:t2_end+0.01}))

  t1_std = stddev(regional_stat_t1_longterm_rm)
  t2_std = stddev(regional_stat_t2_longterm_rm)
  
  delta_std(i) = t2_std - t1_std
  ratio_std(i) = t2_std/t1_std
end do

;#### Now repeat for the CMIP5 data ####
n_models = 35
delete(regional_t1_clim_anom)
delete(regional_t2_clim_anom)
delete(regional_sst)
delete(regional_sst_triangle_smoothed)
delta_mean_state_cmip5 = new(n_models,float)
delta_std_cmip5 = new(n_models,float)
ratio_mean_state_cmip5 = new(n_models,float)
ratio_std_cmip5 = new(n_models,float)
model_name = new(n_models,string)
long_name = new(n_models,string)
do i=0,n_models-1
  print("CMIP5 ensemble member "+i+"/"+n_models)
  root = "/data/rsriver/a/bvegawe2/enso_data/cmip5/"
  if (i .le. 9) then
    wp = addfile(root+"/WP/icmip5_tos_Omon_one_rcp85_120-170E_-5-5N_n_su_00"+i+".nc","r")
    ep = addfile(root+"/EP/icmip5_tos_Omon_one_rcp85_210-270E_-5-5N_n_su_00"+i+".nc","r")
    nino34 = addfile(root+"/NINO34/icmip5_tos_Omon_one_rcp85_190-240E_-5-5N_n_su_00"+i+".nc","r")
  else if (i .gt. 9) then
    wp = addfile(root+"/WP/icmip5_tos_Omon_one_rcp85_120-170E_-5-5N_n_su_0"+i+".nc","r")
    ep = addfile(root+"/EP/icmip5_tos_Omon_one_rcp85_210-270E_-5-5N_n_su_0"+i+".nc","r")
    nino34 = addfile(root+"/NINO34/icmip5_tos_Omon_one_rcp85_190-240E_-5-5N_n_su_0"+i+".nc","r")
  end if
  end if

  cmip5_time = wp->time/12.+1861.

  wp_sst = wp->tos
  ep_sst = ep->tos
  nino34_sst = nino34->tos
  wp_sst&time = cmip5_time
  ep_sst&time = cmip5_time
  nino34_sst&time = cmip5_time

  if (i .gt. 0) then 
    delete(split_name)
  end if
  long_name(i) = wp_sst@long_name
  split_name = str_split(long_name(i)," ")
  model_name(i) = split_name(1)

  r1_t1 = avg(wp_sst({time|t1_start:t1_end-0.01}))
  r1_t2 = avg(wp_sst({time|t2_start:t2_end-0.01}))
  r2_t1 = avg(ep_sst({time|t1_start:t1_end-0.01}))
  r2_t2 = avg(ep_sst({time|t2_start:t2_end-0.01}))
  delta_mean_state_cmip5(i) = (r1_t2-r2_t2) - (r1_t1 - r2_t1)
  ratio_mean_state_cmip5(i) = (r1_t2-r2_t2) / (r1_t1 - r2_t1)

;### Calculate Monthly Climatology anomaly###
  regional_t1_clim_anom = rmAnnCycle1D(nino34_sst({time|t1_start:t1_end-0.01}))
  regional_t2_clim_anom = rmAnnCycle1D(nino34_sst({time|t2_start:t2_end-0.01}))
  regional_sst = nino34_sst

;### Triangle smoother weights
  smoothing_width = 213 ;trying this out as possible explanation of diff wrt wittenberg.
  peak = (smoothing_width-1)/2+1
  weights1 = fspan(0,peak-1,peak)/peak
  weights2 = fspan(peak,0,peak+1)/peak
  weights=array_append_record(weights1,weights2,0)
;###Apply 211-month triangle smoother to regional SST data
  regional_sst_triangle_smoothed=wgt_runave_Wrap(regional_sst,weights,0)

;### nino3 stat (or nino34 stat) with long term trend removed
  regional_stat_t1_longterm_rm = regional_t1_clim_anom({time|t1_start:t1_end-0.01}) -\
             regional_sst_triangle_smoothed({time|t1_start:t1_end-0.01}) +\
             avg(regional_sst_triangle_smoothed({time|t1_start:t1_end-0.01}))
  regional_stat_t2_longterm_rm = regional_t2_clim_anom({time|t2_start:t2_end-0.01}) -\
             regional_sst_triangle_smoothed({time|t2_start:t2_end-0.01}) +\
             avg(regional_sst_triangle_smoothed({time|t2_start:t2_end-0.01}))

  t1_std = stddev(regional_stat_t1_longterm_rm)
  t2_std = stddev(regional_stat_t2_longterm_rm)

  delta_std_cmip5(i) = t2_std - t1_std
  ratio_std_cmip5(i) = t2_std / t1_std
end do

;####Writing names to file for later use
model_name_file = "cmip5_sst_models.txt"
name_list = [/long_name/]
write_table(model_name_file,"w",name_list,"%s")

;####Plotting scatter plot overlay of both ensembles
wks = gsn_open_wks("pdf",plot_name)
res = True
res@gsnDraw = False
res@gsnFrame = False

res@tiMainFontHeightF = 0.025
res@tiYAxisFontHeightF = 0.018
res@tiXAxisFontHeightF = 0.018
res@tiMainString = "Change in mean state and ENSO variability"
if (mean_state_change .eq. "diff") then
  res@tiXAxisString = "Change in WP-EP"
  xval = delta_mean_state
  xval2 = delta_mean_state_cmip5
else if (mean_state_change .eq. "ratio") then
  res@tiXAxisString = "Fractional change in WP-EP"
  xval = ratio_mean_state-1
  xval2 = ratio_mean_state_cmip5-1
end if
end if
if (std_change .eq. "diff") then
  res@tiYAxisString = "Change in ENSO st. d."
  yval = delta_std
  yval2 = delta_std_cmip5
else if (std_change .eq. "ratio") then
  res@tiYAxisString = "Fractional change in ENSO st. d."
  yval = ratio_std-1
  yval2 = ratio_std_cmip5-1
end if
end if

;##aspect ratio
;res@vpHeightF = 0.5
;res@vpWidthF = 0.7
;##
res@gsnMaximize = False
res@trYMinF = -0.7
res@trYMaxF = 0.7
res@trXMinF = -0.5
res@trXMaxF = 0.5
res@xyMarkLineMode = "Markers"
res@xyMarkerSizeF = 0.015
res@gsnYRefLine = 0.0
res@gsnXRefLine = 0.0

plot = gsn_csm_xy(wks,xval,yval,res)
res2 = res
res2@xyMarkerColor =(/"red"/)
plot2 = gsn_csm_xy(wks,xval2,yval2,res2)
overlay(plot2,plot)


;##add Legend to plot
lgres = True
lgres@lgMonoItemType = False
lgres@lgMonoMarkerColor = False
lgres@MonoMarkerIndex = False
lgres@lgItemTypes = (/"Markers","Markers"/)
lgres@lgMarkerColors = (/"black","red"/)
lgres@lgMarkerIndexes = (/0,0/)
lgres@lgLabelFontHeightF = 0.08
lgres@vpWidthF = 0.15
lgres@vpHeightF = 0.08
lgres@lgMarkerSizeF = 0.015
lgres@lgBoxMajorExtentF =0.1
lgres@lgBoxMinorExtentF = 0.1
lgres@lgJustification = "TopLeft"
lbid = gsn_create_legend(wks,2,(/"CESM","CMIP5"/),lgres)
amres = True
amres@amParallelPosF = 0.35
amres@amOrthogonalPosF = 0.4
annoid1 = gsn_add_annotation(plot2,lbid,amres)

draw(plot2)
frame(wks)

end
