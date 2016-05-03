;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

begin

;type="x11"
type="pdf"

;### MODEL SST
region = "nino34" ;not implemented yet - choices are nino3 and nino34 currently
variable_contour = "sst_std" ;choices are sst_avg and sst_std

forced_runs = (/"42", "43", "44","45","46","47",\
       "48","49","50","51","52","53","54","55",\
       "56","57","58","59","60","61","62","63",\
       "64","65","66","67","68","69","70","71",\
       "72","73","74","75","76","77","78","79",\
       "80","81","82","83","84","85","86","87",\
       "88","89","90","91"/)
;forced_runs = (/"42","43"/)

ensemble_array = new((/180,360,dimsizes(forced_runs)/),float)

do j=0,dimsizes(forced_runs)-1
model_run = forced_runs(j)
print(model_run)
fts1=addfile("~/a/enso_data/forced_ensemble/forcedto2100_"+model_run+"00.nc","r")
sst1=fts1->temp(:,0,:,:)

time1_in=fts1->time
time1=doubletofloat(time1_in/365.)
sst1&time=time1
ti=1940.01
tf=1990.01

if (variable_contour .eq. "sst_std") then
  sst1_clim=clmMonLLT(sst1(lat|:,lon|:,{time|ti:tf}))
  sst1_anom=calcMonAnomLLT(sst1(lat|:,lon|:,{time|ti:tf}),sst1_clim)
  sst1_std=dim_stddev_Wrap(sst1_anom(lat|:,lon|:,{time|ti:tf}))
  ensemble_array(:,:,j) = sst1_std
else if (variable_contour .eq. "sst_avg") then
  ensemble_array(:,:,j) = dim_avg_Wrap(sst1(lat|:,lon|:,{time|ti:tf}))
end if
end if

end do

ensemble_mean = dim_avg_Wrap(ensemble_array(:,:,:))
;##############

;### ERSST


;### Read in and format SST data ###
f1=addfile("~/a/enso_data/ersst_cat.nc","r")

sst_eq=f1->sst(:,0,:,:)
sst_eq!1="lat"
sst_eq!2="lon"

time_in=f1->time
  time=doubletofloat(cd_calendar(time_in,4))
  sst_eq!0="time"
  sst_eq&time=time

ti=1940
tf=1990

if (variable_contour .eq. "sst_std") then
  ersst_sst_clim=clmMonLLT(sst_eq(lat|:,lon|:,{time|ti:tf}))
  ersst_sst_anom=calcMonAnomLLT(sst_eq(lat|:,lon|:,{time|ti:tf}),ersst_sst_clim)
  ersst_array=dim_stddev_Wrap(ersst_sst_anom(lat|:,lon|:,time|:))
else if (variable_contour .eq. "sst_avg") then
  ersst_array=dim_avg_Wrap(sst_eq(lat|:,lon|:,{time|ti:tf}))
end if
end if

wks_type = type
;wks_type@wkOrientation = "landscape"
  wks  = gsn_open_wks(wks_type,"map_"+variable_contour)            
; plot = new(3,graphic)
 plot = new(2,graphic)
;  gsn_define_colormap(wks,"WhBlGrYeRe")              ; choose 
;  gsn_define_colormap(wks,"cust_new_amwg_blueyellowred")
  gsn_define_colormap(wks,"BlueWhiteOrangeRed")            ; choose 
;  gsn_define_colormap(wks,"Rainbow")              ; choose colormap

NhlSetColor(wks,2,.8,.8,.8) ; sets the last index in the color table to light gray.
 
  res                      = True                ; make plot mods

 res@gsnDraw             = False           ; don't draw
 res@gsnFrame            = False           ; don't advance frame




 res@gsnStringFontHeightF = 0.015
  res@gsnRightString   = ""
  res@gsnLeftString   = ""
  res@tiMainString    = ""
  
res@cnFillOn             = True                ; turn on color
  res@gsnSpreadColors      = True                ; use full colormap
  res@gsnSpreadColorStart    =  128                 ; Start color in 

  res@mpCenterLonF = 200
  res@lbLabelBarOn        = False           ; turn off individual cb's
  res@cnLevelSelectionMode = "ManualLevels"      ; manually set cn levels
  if (variable_contour .eq. "sst_std") then
    res@cnMinLevelValF       = 0.2                 ; min level
    res@cnMaxLevelValF       = 2                 ; max level
    res@cnLevelSpacingF      = .2                  ; contour level 
  else if (variable_contour .eq. "sst_avg") then
    res@cnMinLevelValF	     = 21. 		;min level
    res@cnMaxLevelValF	     = 32.		;max level
    res@cnLevelSpacingF	     = 1.		;contour level
  end if
  end if
  res@cnMonoLineColor=False
  res@cnLineColors = (/"transparent","black","black","black","black","black","black","black","black","black"/)

;res@cnFillDrawOrder      = "PreDraw"  ; draw contours first
   res@cnLinesOn            = True
;  res@cnMonoLineColor = True
;  res@cnLineColor = "white"
;  res@gsnContourLineThicknessesScale = .25

 res@gsnSpreadColorEnd = -2
 res@cnMissingValPerimOn     = True            ; turn on the missing value perim$
 res@cnMissingValPerimColor  = "gray"           ; outline the perimeter red
 res@cnMissingValFillPattern = 0               ; choose fill pattern 0 (solid f$
 res@cnMissingValFillColor   = "gray"          ; color fill missing areas blue
  res@mpLandFillColor       = "gray"            ; change continent color
res@mpFillDrawOrder      = "PostDraw"         ; Draw map fill last

  res@lbLabelFontHeightF   = 0.015
   res@tmXBLabelFontHeightF = 0.015                 ; resize tick labels
   res@tmYLLabelFontHeightF = 0.015
   res@pmLabelBarOrthogonalPosF = .25                ; move label bar down
   res@cnInfoLabelOn = False
   res@cnLineLabelsOn = False

   res@gsnMajorLonSpacing  = 30.

  res@gsnAddCyclic          = False
  res@mpMinLatF            = -10      ; range to zoom in on
  res@mpMaxLatF            = 10
  res@mpMinLonF            = 120.
  res@mpMaxLonF            = 290.

  res@mpShapeMode           = "FreeAspect"
  res@vpWidthF              = 1.*.9
  res@vpHeightF             = 0.2*.9


  res@mpGridAndLimbOn        = True
  res@mpGridLineDashPattern  = 5                  ; lat/lon lines dashed
  res@mpGridLatSpacingF      = 5
  res@mpGridLonSpacingF      = 30
;  res@mpGridAndLimbDrawOrder = "PreDraw"          ; Draw grid first



  res@gsnCenterString         = "ERSST v3b"
  plot(0) = gsn_csm_contour_map_ce(wks,ersst_array,res) 
  res@gsnCenterString         = "Ensemble mean"
  plot(1) = gsn_csm_contour_map_ce(wks,ensemble_mean,res) 
;  res@gsnCenterString         = "Ensemble mean"
;  plot(2) = gsn_csm_contour_map_ce(wks,ensemble_mean,res) 


  resP                     = True                ; modify the panel plot
;  resP@txString            = "Monthly SST Standard Deviation (C)"
  resP@lbLabelFontHeightF  = 0.0175               ; make labels smaller
  ;resP@lbLabelAutoStride    = False                ; automatic lb label 
  resP@lbLabelStride           = 1                ; every 4th label
  resP@gsnPanelLabelBar    = True                ; add common colorbar
  resP@lbTitleOn = True
  resP@lbTitleString = "Celsius"
  resP@lbTitlePosition = "Bottom"
  resP@lbTitleFontHeightF= .0175                 ; make title smaller
  resP@pmLabelBarOrthogonalPosF = -.05           ; move whole thing down

;;;;;trying to add outline of NINO3 or NINO34 region
  boxres = True
  boxres@gsLineThicknessF = 4.0
  line1 = gsn_add_polyline(wks,plot(0),(/190,240/),(/-5,-5/),boxres)
  line2 = gsn_add_polyline(wks,plot(0),(/190,240/),(/5,5/),boxres)
  line3 = gsn_add_polyline(wks,plot(0),(/190,190/),(/5,-5/),boxres)
  line4 = gsn_add_polyline(wks,plot(0),(/240,240/),(/5,-5/),boxres)
  line21 = gsn_add_polyline(wks,plot(1),(/190,240/),(/-5,-5/),boxres)
  line22 = gsn_add_polyline(wks,plot(1),(/190,240/),(/5,5/),boxres)
  line23 = gsn_add_polyline(wks,plot(1),(/190,190/),(/5,-5/),boxres)
  line24 = gsn_add_polyline(wks,plot(1),(/240,240/),(/5,-5/),boxres)


  gsn_panel(wks,plot,(/3,1/),resP)               ; now draw as one plot

delete(wks)


end



