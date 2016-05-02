;Calculate the 20C depth across the equatorial Pacific

;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

begin

type = "pdf"

remove_trend = True ;#subtract triangle smoothing for trend removal
ti=1940
tf=1990
ti_ensemble = (/1939.99,2039.99,24.99/)
tf_ensemble = (/1989.99,2089.99,74.99/)
file_roots = (/"~/a/enso_data/forced_w_depth/forcedto2100_",\
	"~/a/enso_data/forced_w_depth/forcedto2100_",\
	"~/a/enso_data/control_w_depth/control_"/)

;#Data already only includes trop Pacific, so unneeded
;lon_max = 270.
;lon_min = 120.
;lat_max = 5.
;lat_min = -5.
lon_nino3 = (/210,270/)

forced_runs = (/"42", "43", "44","45","46","47",\
       "48","49","50","51","52","53","54","55",\
       "56","57","58","59","60","61","62","63",\
       "64","65","66","67","68","69","70","71",\
       "72","73","74","75","76","77","78","79",\
       "80","81","82","83","84","85","86","87",\
       "88","89","90","91"/)
;forced_runs = (/"44","45"/)

avg_ccrs = new((/2*36+1,3/),double)

do i = 0,2

print("i="+i)
;ensemble_depth = new((/50*12,dimsizes(forced_runs)/),float)
;ensemble_sst = new((/50*12,dimsizes(forced_runs)/),float)
ensemble_ccr = new((/2*36+1,dimsizes(forced_runs)/),double)

do j=0,dimsizes(forced_runs)-1
model_run = forced_runs(j)
ftemp = addfile(file_roots(i)+model_run+"00.nc","r")
if (i .ne. 0) then ;Arrays are different length for control
  delete(temp_in)
  delete(time_in)
  delete(time1)
  delete(z)
end if
temp_in = ftemp->temp

;#Converting to years and meters
time_in = temp_in&time
if(i .eq. 2) then
  time1 = doubletofloat(time_in/365 -time_in(0)/365+31./365.) ;time(0) is Feb. 1 in control
else
  time1 = doubletofloat(time_in/365)
end if
temp_in&time = time1
z = temp_in&z_t
z = z/100.
temp_in&z_t = z

temp_troppac = temp_in({time|ti_ensemble(i):tf_ensemble(i)},z_t|:,lat|:,lon|:)
sst_nino3 = temp_troppac(lat|:,{lon|lon_nino3(0):lon_nino3(1)},time|:,z_t|:)
sst_3d = sst_nino3(:,:,:,0)

;#Formatting a depth array to interpolate the 20C depth
depth = temp_troppac
depth = conform_dims(dimsizes(temp_troppac),z,1)

isot = int2p_n_Wrap(temp_troppac,depth,20,1,1)
isot_3d = isot(:,0,:,:)

;#Calculate climatological anomalies and average over lat/lon 
;#For 20 deg C thermocline-
clm_depth = clmMonLLT(isot_3d(lat|:,lon|:,time|:))
monthly_depth_anom = calcMonAnomLLT(isot_3d(lat|:,lon|:,time|:),clm_depth)
troppac_mon_depth = dim_avg_n_Wrap(dim_avg_n_Wrap(monthly_depth_anom,0),0)
norm_depth_anom = (troppac_mon_depth-avg(troppac_mon_depth))/dim_stddev(troppac_mon_depth)
;ensemble_depth(0:dimsizes(time1)-1,j) = norm_depth_anom

;#And for SST-
clm_sst = clmMonLLT(sst_3d)
monthly_sst_anom = calcMonAnomLLT(sst_3d,clm_sst)
nino3_mon_sst = dim_avg_n_Wrap(dim_avg_n_Wrap(monthly_sst_anom,0),0)
nino3_mon_sst_raw = dim_avg_n_Wrap(dim_avg_n_Wrap(sst_3d,0),0)

;#Subtract triangle smoothing
if (remove_trend .eq. True) then
  smoothing_width = 213 ;trying this out as possible explanation of diff wrt wittenberg.
  peak = (smoothing_width-1)/2+1
  weights1 = fspan(0,peak-1,peak)/peak
  weights2 = fspan(peak,0,peak+1)/peak
  weights=array_append_record(weights1,weights2,0)
;###Apply 211-month triangle smoother to regional SST data
  nino3_sst_smoothed=wgt_runave_Wrap(nino3_mon_sst_raw,weights,0)
  nino3_longterm_rm = nino3_mon_sst - nino3_sst_smoothed+avg(nino3_sst_smoothed)
end if

if (remove_trend .eq. True) then
  norm_sst_anom = (nino3_longterm_rm-avg(nino3_longterm_rm))/dim_stddev(nino3_longterm_rm)
else
  norm_sst_anom = (nino3_mon_sst-avg(nino3_mon_sst))/dim_stddev(nino3_mon_sst)
end if

;ensemble_sst(0:dimsizes(time1)-1,j) = norm_sst_anom

;#Now calculate the lagged cross-correlation of SST and 20degC depth
maxlag = 36
lag=ispan(-36,36,1)
lag =lag(::-1)

pos_ccr = esccr(norm_depth_anom,norm_sst_anom,maxlag)
neg_ccr = esccr(norm_sst_anom,norm_depth_anom,maxlag)

tot_ccr = new(2*maxlag+1,double)
tot_ccr(0:maxlag-1)=neg_ccr(1:maxlag:-1)
tot_ccr(maxlag:)=pos_ccr

ensemble_ccr(:,j) = tot_ccr

end do

avg_ccrs(:,i) = dim_avg_n_Wrap(ensemble_ccr,1) 
end do
;fout = addfile("test_forced_20C.nc","c")  ;if we want to save 20C depth array
;fout->depth_20C = isot_3d


plot_array = new((/3,dimsizes(lag)/),double)
plot_array(0,:)= avg_ccrs(:,0)
plot_array(1,:)= avg_ccrs(:,1)
plot_array(2,:)= avg_ccrs(:,2)

wks = gsn_open_wks(type,"./corr_d_t_longterm_rm")

 res                  = True                     ; plot mods desired
 res@gsnDraw          = False  ;Don't draw, want to overlay plots
 res@gsnFrame         = False

; res@tiMainString     = "Nino3 SST and Eq Pac 20C depth"
 res@tiYAxisString = "Correlation"
 res@tiXAxisString = "Lag (months)"

res@trXMinF = -36
res@trXMaxF = 36
res@trYMinF = -1
res@trYMaxF = 1

res@xyLineColors      = (/"black","black","blue"/)
res@xyDashPatterns    = (/0,16,0/)
res@xyLineThicknesses = (/5,5,5/)

  res@vpXF     = 0.165                   ; start plot at x ndc coord
  res@vpHeightF= 0.4*0.75                    ; change aspect ratio of plot
  res@vpWidthF = 0.8*0.75


 res@pmLegendDisplayMode    = "Always"            ; turn on legend
  res@pmLegendSide           = "Top"               ; Change location of
  res@pmLegendParallelPosF   =  0.25                 ; move units right
  res@pmLegendOrthogonalPosF = -1.                ; move units down
  res@pmLegendWidthF         = 0.125                ; Change width and
  res@pmLegendHeightF        = 0.1                ; height of legend.
  res@lgPerimOn              = False               ; turn off box around
  res@lgLabelFontHeightF     = .0125                 ; label font height
;  res@xyExplicitLegendLabels = (/ti/)
  res@xyExplicitLegendLabels = (/"Ensemble, 1940-1990","Ensemble, 2040-2090","Control"/)

 plot  = gsn_csm_xy (wks,lag,plot_array,res) ; create plot

res2=True
 res2@gsnDraw          = False
 res2@gsnFrame         = False

  res2@xyLineColor     = "black"
  res2@xyLineThicknesses     = 0.5
  res2@xyDashPattern    = 1
 plot2  = gsn_csm_xy (wks,lag,avg_ccrs(:,0)-avg_ccrs(:,0),res2) ; create plot

overlay(plot,plot2)
draw(plot)
frame(wks)
end
