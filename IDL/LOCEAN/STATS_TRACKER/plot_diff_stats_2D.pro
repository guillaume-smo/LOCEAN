PRO plot_diff_stats_2D


; parametres
basin    = 'SIO'
datebeg  = '19900101'
period   = '1990-2009'
dateend  = '20100101'
nbyear   = 20
ucrit    = 17.5
vorcrit  = 30.e-5
vorelax  = 30.e-5
templist = [1]
temptype = 'TREAL'


; lecture best-track
FOR item = 0, n_elements(templist)-1 DO BEGIN
    tempcrit = templist[item]

    expname = 'IBTRACS'  
    @read_ibtracs_wmo.pro
    indcgsio  = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcgsio)
    pdf_cg_obs = hist_2D(loncycn[indcgsio,0], latcycn[indcgsio,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)
    pdf_obs = hist_2D(loncycn[indcgsio,*], latcycn[indcgsio,*], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)

    expname = 'COUPLED_SW2_KF'
    pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathin
    filein = 'tracker_light_'+temptype+'_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
    restore, pathin + filein, /VERBOSE
    monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
    yearcycn  = long(datecycn/10000.)
    help, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn
    indcgsio  = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcgsio)
    pdf_cg_cpl = hist_2D(loncycn[indcgsio,0], latcycn[indcgsio,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)
    pdf_cpl = hist_2D(loncycn[indcgsio,*], latcycn[indcgsio,*], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)

    expname = 'FORCED_SW2_KF'
    dateend = '19990101' & nbyear = 9 & period = '1990-1998' 
    pathin = '/net/adonis/usr/adonis/varclim/gslod/IDL/TRACKER/EXP_'+expname+'/DATA/' & help, pathin
    filein = 'tracker_light_'+temptype+'_RVM_NICO_MASK_u'+ strtrim(long(ucrit),2) +'_v'+ strtrim(long(vorcrit*100000),2) +'_t'+ strtrim(long(tempcrit*10),2) +'_r'+ strtrim(long(vorelax*100000),2) +'_'+period+'.idl'
    restore, pathin + filein, /VERBOSE
    monthcycn = long(datecycn/100.) - long(datecycn/10000.)*100.
    yearcycn  = long(datecycn/10000.)
    help, loncycn, latcycn, mslpcycn, uv10cycn, datecycn, juldcycn
    indcgsio  = where(latcycn[*,0] LE 0. AND latcycn[*,0] GE -30. AND loncycn[*,0] GE 30. AND loncycn[*,0] LE 140., cntcgsio)
    pdf_cg_frc = hist_2D(loncycn[indcgsio,0], latcycn[indcgsio,0], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)
    pdf_frc = hist_2D(loncycn[indcgsio,*], latcycn[indcgsio,*], bin1=5, min1=30, max1=130, bin2=5, min2=-30, max2=0) / float(nbyear)

ENDFOR


; plot
IF basin EQ 'SIO' THEN computegrid,30,-30,5,5,21,7
plt, (pdf_cg_cpl/total(pdf_cg_cpl)-pdf_cg_obs/total(pdf_cg_obs))*100., -5, 5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,1], format='(I3)', lct=66, charsize=1.2, cb_charsize=1.2
plt, (pdf_cg_frc/total(pdf_cg_frc)-pdf_cg_obs/total(pdf_cg_obs))*100., -5, 5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,2], format='(I3)', lct=66, charsize=1.2, cb_charsize=1.2, /noerase
plt, (pdf_cg_frc/total(pdf_cg_frc)-pdf_cg_cpl/total(pdf_cg_cpl))*100., -5, 5, /realcont, /nocont, subtitle=' ', title=' ', xtitle=' ', ytitle=' ', /rempli, small=[1,3,3], format='(I3)', lct=66, charsize=1.2, cb_charsize=1.2, /noerase
saveimage, 'DIFF_CG_NORM_2D_KF-OBS.gif'

stop
END
