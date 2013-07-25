argus = COMMAND_LINE_ARGS()
prog = argus[0] & help, prog

.r select_maxima_RVM_online_ULAM
IF prog EQ 'select_maxima_RVM_online_ULAM' THEN select_maxima_RVM_online_ULAM

.r build_tracks_ULAM
IF prog EQ 'build_tracks_ULAM' THEN build_tracks_ULAM

EXIT
