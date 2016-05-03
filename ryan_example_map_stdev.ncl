;********************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
;********************************************************

begin

type="x11"


;### SAT

fts1=addfile("../data/ens_sat/A3.0_L240_13500.nc","r")
ts=fts1->ts  ;(600:1199,:,:)

ts_clim=clmMonLLT(ts(lat|:,lon|:,time|:))
ts_anom=calcMonAnomLLT(ts(lat|:,lon|:,time|:),ts_clim)

ts1_std=dim_stddev_Wrap(ts_anom(lat|:,lon|:,time|:))

printVarSummary(ts1_std)

;################

fts2=addfile("../data/ts_out-of-box_active.nc","r")
ts2=fts2->ts  ;(0:599,:,:)

ts2_clim=clmMonLLT(ts2(lat|:,lon|:,time|:))
ts2_anom=calcMonAnomLLT(ts2(lat|:,lon|:,time|:),ts2_clim)

ts2_std=dim_stddev_Wrap(ts2_anom(lat|:,lon|:,time|:))

printVarSummary(ts2_std)


;### ORA


;### Read in and format SST data ###
;f1=addfile("../data/ora/eqpac_sst_ora.nc","r")
f1=addfile("./oras3_sst_pac.nc","r")

t_eq=f1->TEMP(:,0,:,:)
t_eq!1="lat"
t_eq!2="lon"

time_in=f1->TIME
  time=ut_calendar(time_in,-1)
  t_eq!0="time"
  t_eq&time=time

ti=196001
tf=200912


ora_ts_clim=clmMonLLT(t_eq(lat|:,lon|:,{time|ti:tf}))
ora_ts_anom=calcMonAnomLLT(t_eq(lat|:,lon|:,{time|ti:tf}),ora_ts_clim)

ora_std=dim_stddev_Wrap(ora_ts_anom(lat|:,lon|:,time|:))




wks_type = type
;wks_type@wkOrientation = "landscape"
  wks  = gsn_open_wks(wks_type,"./plot_stdev")            
 plot = new(3,graphic)

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
  res@cnMinLevelValF       = 0.2                 ; min level
  res@cnMaxLevelValF       = 2                 ; max level
  res@cnLevelSpacingF      = .2                  ; contour level 
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



  res@gsnCenterString         = "ORA-S3"
  plot(0) = gsn_csm_contour_map_ce(wks,ora_std,res) 
  res@gsnCenterString         = "LOVECLIM (Calibrated)"
  plot(1) = gsn_csm_contour_map_ce(wks,ts1_std,res) 
  res@gsnCenterString         = "LOVECLIM (out-of-box)"
  plot(2) = gsn_csm_contour_map_ce(wks,ts2_std,res) 


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

  gsn_panel(wks,plot,(/3,1/),resP)               ; now draw as one plot

delete(wks)


end



