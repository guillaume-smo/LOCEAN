;+
;
; @file_comments
; Overprint vectors in a field traced by plt.
;
; @categories
; Graphics
;
; @param VECTEUR {in}{required}{type=vector}
; It is a structure with 2 elements containing we 2 matrices U and V of
; values of the zonal and meridian component of the field of vectors to
; be traced.
;      For ex:
;       vecteur={matriceu:lec('unsurface'),matricev:lec('vnsurface')}
;       rq:the name of elements of  vector does not have any importance.
;       vecteur={u:lec('unsurface'),v:lec('vnsurface')} goes well too.
;
; @keyword UNVECTSUR {type=scalar or array}
; It is a scalar n or an array with 2 elements [n1,n2].
; In the first case, we will trace a vector on n following x and y.
; In the second case, we will trace a vector on n1 following x and a
; vector n2 following n2
;   Comments: To trace all vectors following y and one vector on two
; following x, put unvectsur=[2,1]
;
; @keyword VECTMIN {in}{required}
; Minimum norme of vectors to be traced
;
; @keyword VECTMAX {in}{required}
; Maximum norme of vectors to be traced
;
; @keyword _EXTRA
; Used to pass keywords
;
; @uses
; <pro>common</pro>
;
; @history
; Sebastien Masson (smasson\@lodyc.jussieu.fr)
;			10/3/1999
;                       11/6/1999 compatibilite avec NAN et la lecture
;                       des structures.
;
; @version
; $Id: ajoutvect.pro 370 2008-08-07 07:59:15Z pinsard $
;
;-
PRO ajoutvectt,vecteur, vectlegende $
             , UNVECTSUR=unvectsur,VECTMIN=vectmin, VECTMAX=vectmax, _EXTRA=ex
;
  compile_opt idl2, strictarrsubs
;
@common
   tempsun = systime(1)         ; For key_performance
;----------------------------------------------------------------------------
;
   u = litchamp(vecteur.(0))
   u = checkfield(u, 'plt', TYPE = 'xt', /NOQUESTION)
   v = litchamp(vecteur.(1))
   v = checkfield(v, 'plt', TYPE = 'xt', /NOQUESTION)
   help, u, v
;-----------------------------------------------------------
;-----------------------------------------------------------
; We recuperate possible informations on fields
;-----------------------------------------------------------
   grilleu = litchamp(vecteur.(0), /grid)
   if grilleu EQ '' then grilleu = 'U'
   grillev = litchamp(vecteur.(1), /grid)
   if grillev EQ '' then grillev = 'V'

   IF grilleu EQ 'V' AND grillev EQ 'U' THEN inverse = 1
   IF grilleu EQ grillev THEN interpolle  = 0 ELSE interpolle = 1
   help, interpolle
   if keyword_set(inverse) then begin
      rien = u
      u = v
      v = rien
   endif
;------------------------------------------------------------
; We find common points between u and v
;------------------------------------------------------------
   if interpolle then begin
      indicexu = (lindgen(jpi))[firstxu:firstxu+nxu-1]
      indicexv = (lindgen(jpi))[firstxv:firstxv+nxv-1]
      indicex = inter(indicexu, indicexv)
      indiceyu = (lindgen(jpj))[firstyu:firstyu+nyu-1]
      indiceyv = (lindgen(jpj))[firstyv:firstyv+nyv-1]
      indicey = inter(indiceyu, indiceyv)
      nx = n_elements(indicex)
      ny = n_elements(indicey)
      indice2d = lindgen(jpi, jpj)
      indice2d = indice2d[indicex[0]:indicex[0]+nx-1,indicey[0]:indicey[0]+ny-1]
;------------------------------------------------------------
; extraction of u and v on the appropriated domain
;------------------------------------------------------------
      case 1 of
         (size(u))[0] NE 2 OR (size(v))[0] NE 2: return
         (size(u))[1] EQ nxu AND (size(u))[2] EQ nyu AND $
          (size(v))[1] EQ nxv AND (size(v))[2] EQ nyv:BEGIN
            if nxu NE nx then $
             if indicex[0] EQ firstxu then u = u[0:nx-1, *] ELSE u = u[1: nx, *]
            IF nxv NE nx THEN $
             if indicex[0] EQ firstxv then v = v[0:nx-1, *] ELSE v = v[1: nx, *]
            IF nyu NE ny THEN $
             if indicey[0] EQ firstyu then u = u[*, 0:ny-1] ELSE u = u[*, 1: ny]
            IF nyv NE ny THEN $
             if indicey[0] EQ firstyv then v = v[*, 0:ny-1] ELSE v = v[*, 1: ny]
         END
         (size(u))[1] EQ jpi AND (size(u))[2] EQ jpj AND $
          (size(v))[1] EQ jpi AND (size(v))[2] EQ jpj:BEGIN
            u = u[indice2d]
            v = v[indice2d]
         END
         ELSE:BEGIN
            ras = report('problemes d''adequation entre la taille du domaine et la taille des matrices necessaires a tracer des vecteurs')
            return
         end
      endcase
;------------------------------------------------------------------
; We reshape u and v to make sure that none dimension has been erased.
;------------------------------------------------------------------
      if ny EQ 1 then begin
         u = reform(u, nx, ny)
         v = reform(v, nx, ny)
      endif
      help, u, v
;------------------------------------------------------------------
; construction of u and v at points T
;-----------------------------------------------------------
      a=u[0,*]
      u=(u+shift(u,1,0))/2.
      if NOT keyword_set(key_periodic) OR nx NE jpi then u[0,*]=a
      a=v[*,0]
      v=(v+shift(v,0,1))/2.
      if NOT keyword_set(key_periodic) OR nx NE jpi then v[*,0]=a
;----------------------------------------------------------------------------
; attribution of the mask and of longitude and latitude arrays.
; We recuperate the complete grid to establish a big mask extensive
; in the four directions to cover points for which a land point has
; been considerated (do a small drawing)
;----------------------------------------------------------------------------
      vargrid='T'
      msku = (umask())[indice2d+jpi*jpj*firstzt]
      mskv = (vmask())[indice2d+jpi*jpj*firstzt]
      glam = glamt[indice2d]
      gphi = gphit[indice2d]
      if ny EQ 1 then begin
         msku = reform(msku, nx, ny)
         mskv = reform(mskv, nx, ny)
;          glam = reform(glam, nx, ny)
;          gphi = reform(gphi, nx, ny)
      endif
;-----------------------------------------------------------
; We mask u and v the long of coasts (the place where we
; can not calculate the average)
;-----------------------------------------------------------
; extension of the mask
      u = u*msku*shift(msku,1,0)
      v = v*mskv*shift(mskv,0,1)
   ENDIF ELSE BEGIN ; interpolle
;      u = u*tmask[firstxt:lastxt,jpt]
;      v = v*tmask[firstxt:lastxt,jpt]
      indice2d = lindgen(jpi, jpt)
      indice2d = indice2d[firstxt:lastxt,0:jpt-1]
      nx = nxt
      ny = jpt
      print, 'case: interpolle=0'
      help, u, v
   ENDELSE

   tabnorme=sqrt(u^2+v^2)
   nan = where(finite(u, /nan) EQ 1)
   if nan[0] NE -1 then u[nan] = 1e5
   nan = where(finite(v, /nan) EQ 1)
   if nan[0] NE -1 then v[nan] = 1e5
   if keyword_set(vectmin) then BEGIN
      toosmall=where(tabnorme lt vectmin)
      if toosmall[0] NE -1 then begin
         u[toosmall] = 1e5
         v[toosmall] = 1e5
      ENDIF
   endif
   if keyword_set(vectmax) then BEGIN
      toobig=where(tabnorme gt vectmax)
      if toobig[0] NE -1 then begin
         u[toobig] = 1e5
         v[toobig] = 1e5
      ENDIF
   ENDIF
;-----------------------------------------------------------
; Put back of a big value on all points for which we can do the calculation.
;-----------------------------------------------------------
   if interpolle then t2 = msku*shift(msku,1,0)*mskv*shift(mskv,0,1) $
;   ELSE t2 = tmask[firstxt:lastxt,firstyt:lastyt,firstzt]
   ELSE t2 = u * 0. + 1.; & t2[where(finite(u) EQ 0)]=0.
   if NOT keyword_set(key_periodic) OR nx NE jpi then t2[0, *]=0.
   t2[*,0]=0.
   terre=where(t2 eq 0)
   if terre[0] ne -1 then begin
      u[terre]=1e5
      v[terre]=1e5
   ENDIF
;-----------------------------------------------------------
; trace only one vector one two
;-----------------------------------------------------------
   if keyword_set(unvectsur) then BEGIN ;
; indx is a vector containing number of columns to be selected.
; indy is a vector containing number of lines to be selected.
      if n_elements(unvectsur) EQ 1 then begin
         indx = where((lindgen(nx) MOD unvectsur[0]) eq 0)
         indy = where((lindgen(ny) MOD unvectsur[0]) eq 0)
      ENDIF ELSE BEGIN
         indx = where((lindgen(nx) MOD unvectsur[0]) eq 0)
         indy = where((lindgen(ny) MOD unvectsur[1]) eq 0)
     ENDELSE
; From indx and indy, we will construct an array which will give indexes
; of intersections points of columns specified by indx.
      indicereduit = indx#replicate(1,n_elements(indy))+nx*replicate(1,n_elements(indx))#indy
; We reduce arrays which will be passed to vecteur.
      u = u[indicereduit]
      v = v[indicereduit]
      tabnorme = tabnorme[indicereduit]
      t2 = t2[indicereduit]
   endif
;-----------------------------------------------------------
;
;-----------------------------------------------------------
   if keyword_set(inverse) then begin
      rien = u
      u = v
      v = rien
   endif
;-----------------------------------------------------------
; Drawing of vectors.
;----------------------------------------------------------
   help, u, v, t2, indx, indy, indice2d, indicereduit
   vecteurt, u, v, tabnorme, indice2d, indicereduit, missing=1e5, _extra = ex
;-----------------------------------------------------------
; We complete the caption.
;-----------------------------------------------------------
   if terre[0] ne -1 then mini = min(tabnorme[where(t2 eq 1)], max = maxi, /nan) $
   ELSE mini = min(tabnorme, max = maxi, /nan)

   if litchamp(vecteur.(0), /u) NE '' then $
    vectlegende = {minmax:[mini, maxi], unite:litchamp(vecteur.(0), /u)} $
   ELSE vectlegende = {minmax:[mini, maxi], unite:varunit}


sortie:
   if keyword_set(key_performance) NE 0 THEN print, 'temps ajoutvect', systime(1)-tempsun
   return
end
