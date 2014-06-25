IF e EQ 0 THEN maxres = min(e1t, /nan)
maxres = min([REFORM(e1t, n_elements(e1t)),maxres], /nan)
IF maxres EQ min(e1t, /nan) THEN nummod_maxres = e
