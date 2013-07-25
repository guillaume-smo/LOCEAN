!path = expand_path('+' + '/usr/home/gslod/IDL/COMMON') $
      + path_sep(/search_path) + expand_path('+' + '/usr/home/smasson/SAXO_DIR/SRC') $
      + path_sep(/search_path) + expand_path('+' + !dir)

keep_compatibility, 0

@all_cm

homedir = isadirectory('/usr/home/gslod/IDL', title = 'Select the default HOME directory')
iodir = isadirectory('/usr/home/gslod/IDL', title = 'Select the default IO directory')
psdir = isadirectory('/usr/home/gslod/IDL', title = 'Select the default postscripts directory')
imagedir = isadirectory('/usr/home/gslod/IDL', title = 'Select the default images directory')
animdir = isadirectory('/usr/home/gslod/IDL', title = 'Select the default animations directory')

;set_plot, 'PS'
;set_plot, 'Z'
set_plot, 'X'
device, decomposed=0, retain=0
lct, 33
key_portrait = 1
page_size = [20.9903, 29.7039]
windowsize_scale = 1.00000
archive_ps = 0
;@updateold
