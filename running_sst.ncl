;Calculate the 20C depth across the equatorial Pacific

;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

begin

type = "pdf"

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
;forced_runs = (/"42","43","44","45","46","47","48"/)

avg_ssts = new((/50*12,3/),float)
do i = 0,2

print("i="+i)
ensemble_sst = new((/50*12,dimsizes(forced_runs)/),float)

do j=0,dimsizes(forced_runs)-1
print("j="+j)
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

;#Calculate climatological anomalies and average over lat/lon for SST-
clm_sst = clmMonLLT(sst_3d)
monthly_sst_anom = calcMonAnomLLT(sst_3d,clm_sst)
nino3_mon_sst = dim_avg_n_Wrap(dim_avg_n_Wrap(monthly_sst_anom,0),0)
norm_sst_anom = (nino3_mon_sst-avg(nino3_mon_sst))/dim_stddev(nino3_mon_sst)
ensemble_sst(:,j) = norm_sst_anom

end do

avg_ssts(:,i) = dim_avg_n_Wrap(ensemble_sst,1) 
end do

plot_array = new((/3,dimsizes(sst_3d&time)/),double)
plot_array(0,:)= avg_ssts(:,0)
plot_array(1,:)= avg_ssts(:,1)
plot_array(2,:)= avg_ssts(:,2)

wks = gsn_open_wks(type,"./sst_anom_ts")

 res                  = True                     ; plot mods desired
 res@gsnDraw          = False  ;Don't draw, want to overlay plots
 res@gsnFrame         = False

; res@tiMainString     = "Nino3 SST and Eq Pac 20C depth"
 res@tiYAxisString = "SST anomaly"
 res@tiXAxisString = "Year"

res@trXMinF = 0
res@trXMaxF = 50
res@trYMinF = -3
res@trYMaxF = 3

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

 plot  = gsn_csm_xy (wks,sst_3d&time-sst_3d&time(0),plot_array,res) ; create plot

draw(plot)
frame(wks)
end
