
      ; DISPOSITION DES PLOTS SUR UNE MEME FIGURE
      IF nb_plot EQ 1 THEN win=[1, 1]
      IF nb_plot EQ 2 THEN win=[1, 2]
      IF nb_plot EQ 3 THEN win=[1, 3]
      IF nb_plot EQ 4 THEN IF key_portrait THEN win=[2, 2] ELSE win=[1, 4]
      IF nb_plot EQ 5 THEN IF key_portrait THEN win=[2, 3] ELSE win=[2, 3]
      IF nb_plot EQ 6 THEN win=[2, 3]


      ; COORDONNEES DES STATS
      IF nb_plot EQ 2 THEN IF key_portrait THEN xy=[ (box[0]+box[1])/2., box[2]-(ABS(box[2])+ABS(box[3]))/16. ]
      IF nb_plot EQ 3 THEN IF key_portrait THEN xy=[ box[1]+(ABS(box[0])+ABS(box[1]))/14., (box[2]+box[3])/2. ] $
                                           ELSE xy=[ box[1]+(ABS(box[0])+ABS(box[1]))/8. , (box[2]+box[3])/2. ]
      IF nb_plot EQ 4 THEN IF key_portrait THEN xy=[ (box[0]+box[1])/2., box[2]-(ABS(box[2])+ABS(box[3]))/8. ]
      IF nb_plot EQ 4 THEN IF key_portrait EQ 0 THEN xy=[ (box[0]+box[1])/2., box[2]-(ABS(box[2])+ABS(box[3]))/4. ]
      IF nb_plot EQ 5 THEN xy=[ (box[0]+box[1])/2., box[2]-(ABS(box[2])+ABS(box[3]))/2. ]
