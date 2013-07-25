FUNCTION m_mean,vari,nan=nan,dimension=dimension

  IF keyword_set(dimension) EQ 1 THEN BEGIN
    IF keyword_set(nan) EQ 1 THEN BEGIN

      ; ig=where(finite(vari) EQ 1)
      vari_fn=finite(vari)
      IF (size(vari_fn))[0] EQ (size(vari))[0]-1 THEN vari_fn=reform(vari_fn,(size(vari_fn))[1],(size(vari_fn))[2],1)
      varimean=total(vari,dimension,/nan)/total(vari_fn,dimension)
    ENDIF ELSE BEGIN

      varimean=total(vari)/n_elements(vari)

    ENDELSE

  ENDIF ELSE BEGIN
    IF keyword_set(nan) EQ 1 THEN BEGIN

      ig=where(finite(vari) EQ 1)
      IF ig[0] NE -1 THEN varimean=total(vari[ig])/n_elements(ig) ELSE varimean=!values.f_nan

    ENDIF ELSE BEGIN

      varimean=total(vari)/n_elements(vari)

    ENDELSE
  ENDELSE
  return, varimean

END
