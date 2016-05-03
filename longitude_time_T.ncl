;longitude_time_T.ncl - plots longitude-time figure of 
;   annual surface T anomalies relative to longterm mean

;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

begin

;type="x11"
type="pdf"

region = "eq_pac" ;not implemented yet - choices are eq_pac(equatorial pacific),
variable_contour = "sst_anom" ;choices are sst_anom,

ti=1940
tf=1990

ti_ensemble = (/1939.99,2039.99,24.99/)
tf_ensemble = (/1989.99,2089.99,74.99/)

forced_runs = (/"42", "43", "44","45","46","47",\
       "48","49","50","51","52","53","54","55",\
       "56","57","58","59","60","61","62","63",\
       "64","65","66","67","68","69","70","71",\
       "72","73","74","75","76","77","78","79",\
       "80","81","82","83","84","85","86","87",\
       "88","89","90","91"/)
;forced_runs = (/"42","43"/)

lonmax=275
lonmin=130
nlons = lonmax-lonmin
ersst_nlons = floattointeger(floor(nlons/2)+1)
latmax = 5
latmin = -5

do i=0,2
print("i="+i)
ensemble_array = new((/13,nlons,dimsizes(forced_runs)/),float)
ti1 = ti_ensemble(i)
tf1 = tf_ensemble(i)

do j=0,dimsizes(forced_runs)-1
model_run = forced_runs(j)
if (i .eq. 2) then
  file_string = "~/a/enso_data/cesm_control/control_"+model_run+"00.nc"
else
  file_string = "~/a/enso_data/forced_ensemble/forcedto2100_"+model_run+"00.nc"
end if
print("model run = "+model_run)
fts1=addfile(file_string,"r")
if (i .eq. 2) then   ;Arrays are different length for control
  delete(sst1)
  delete(time1_in)
  delete(time1)
end if
sst1=fts1->temp(:,0,:,:)

time1_in=fts1->time
if (i .eq. 2) then
  time1 = doubletofloat(time1_in/365 -time1_in(0)/365+31./365.) ;time(0) is Feb. 1 in control
else
  time1=doubletofloat(time1_in/365.)
end if

sst1&time=time1

if (variable_contour .eq. "sst_anom") then
  sst_troppac = sst1({lat|latmin:latmax},{lon|lonmin:lonmax},{time|ti1:tf1})
  clm_sst = clmMonLLT(sst_troppac)
  clm_lat_avg = dim_avg_n_Wrap(clm_sst(lat|:,month|:,lon|:),0)
  mean_sst = dim_avg_n_Wrap(clm_lat_avg,0)
  
  seasonal_anom = new((/12,nlons/),float)
  do l=0,11
    seasonal_anom(l,:)=clm_lat_avg(l,:)-mean_sst(:)
  end do

  ensemble_array(0:11,:,j) = seasonal_anom
  ensemble_array(12,:,j) = seasonal_anom(0,:)
end if

end do

if (i .gt. 0) then
  delete(ensemble_mean)
end if
ensemble_mean = new((/13,nlons/),float)
ensemble_mean = dim_avg_n_Wrap(ensemble_array(:,:,:),2)
ensemble_mean!1 = "lon"
ensemble_mean&lon = sst_troppac&lon

if (i .eq. 0) then
  hist_mean = ensemble_mean
else if (i .eq. 1) then
  proj_mean = ensemble_mean
else if (i .eq. 2) then
  control_mean = ensemble_mean
end if
end if
end if

end do

;##############


;### ERSST

;### Read in and format SST data ###
f1=addfile("~/a/enso_data/ersst_cat.nc","r")

sst_obs=f1->sst(:,0,:,:)
sst_obs!1="lat"
sst_obs!2="lon"

time_in=f1->time
  time=doubletofloat(cd_calendar(time_in,4))
  sst_obs!0="time"
  sst_obs&time=time

if (variable_contour .eq. "sst_anom") then
  ersst_troppac = sst_obs({lat|latmin:latmax},{lon|lonmin:lonmax},{time|ti:tf})
  clm_ersst = clmMonLLT(ersst_troppac)
  clm_ersst_lat_avg = dim_avg_n_Wrap(clm_ersst(lat|:,month|:,lon|:),0)
  mean_ersst = dim_avg_n_Wrap(clm_ersst_lat_avg,0)
  
  seasonal_anom_ersst = new((/13,ersst_nlons/),float)
  do m=0,11
    seasonal_anom_ersst(m,:)=clm_ersst_lat_avg(m,:)-mean_ersst(:)
  end do
  seasonal_anom_ersst(12,:) = seasonal_anom_ersst(0,:)

  ersst_array = seasonal_anom_ersst
  ersst_array!1 = "lon"
  ersst_array&lon = ersst_troppac&lon
 
end if

wks_type = type
  wks  = gsn_open_wks(wks_type,"lon_time_"+variable_contour)            
 plot = new(4,graphic)
  gsn_define_colormap(wks,"BlWhRe")            ; choose 

  res                      = True                ; make plot mods
 
  res@tmXBLabelFontHeightF = 0.025
  res@tmYLLabelFontHeightF = 0.025 
  res@gsnStringFontHeightF = 0.025
  res@tiMainFontHeightF    = 0.03
  res@gsnDraw             = False           ; don't draw
  res@gsnFrame            = False           ; don't advance frame

;##Color contours
  res@cnFillOn             = True                ; turn on color
  ;res@gsnSpreadColors      = True                ; use full colormap
  ;res@gsnSpreadColorStart    =  128                 ; Start color in 
  res@cnLevelSelectionMode = "ManualLevels"      ; manually set cn levels
  if (variable_contour .eq. "sst_anom") then
    res@cnMinLevelValF       = -3                 ; min level
    res@cnMaxLevelValF       = 3                 ; max level
    res@cnLevelSpacingF      = .25                  ; contour level 
  end if

  res@cnInfoLabelOn 	    = False
  res@cnLineLabelsOn 	    = False
  res@lbLabelBarOn 	    = False

;##Y-axis months
  res@tmYLMode = "Explicit"
  res@trYReverse = True
 
  res@tmYLValues = (/0.,1.,2.,3.,4.,5.,6.,7.,8.,9.,10.,11.,12./)
  res@tmYLLabels = (/"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug",\
			"Sep","Oct","Nov","Dec",""/)

  res@gsnLeftString = "Anomalous SST"
  res@gsnRightString = "Celsius"

;##Aspect ratio of plot
  res@vpHeightF = 1*0.75
  res@vpWidthF = 0.6*0.75

  res@tiMainString         = "ERSST v3b, 1940-1990"
  plot(0) = gsn_csm_hov(wks,ersst_array,res)
  res@tiMainString	   = "Control"
  res@tmYLLabels = (/"","","","","","","","","","","","",""/)
  plot(1) = gsn_csm_hov(wks,control_mean,res) 
  res@tiMainString         = "Forced, 1940-1990"
  plot(2) = gsn_csm_hov(wks,hist_mean,res)
  res@tiMainString	   = "Forced, 2040-2090"
  plot(3) = gsn_csm_hov(wks,proj_mean,res)

  pres=True
  ;pres@gsnMaximize=True
  pres@lbLabelAutoStride = False
  pres@lbLabelStride = 4
  pres@gsnPanelLabelBar = True
  pres@pmLabelBarWidthF=0.5
  pres@lbLabelFontHeightF=0.013
  pres@lbTitleOn = True
  pres@lbTitlePosition = "Bottom"
  pres@lbTitleString = "Celsius"
  pres@lbTitleFontHeightF = 0.013
  pres@pmLabelBarOrthogonalPosF = -0.02

  gsn_panel(wks,plot,(/1,4/),pres)               ; now draw as one plot

delete(wks)


end



