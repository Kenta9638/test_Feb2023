  ?0  v   k820309    ,          2021.5.0    Ťúc                                                                                                          
       /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/observations/forward_operators/obs_def_mod.f90 OBS_DEF_GPS_MOD              SET_GPSRO_REF GET_GPSRO_REF WRITE_GPSRO_REF READ_GPSRO_REF GET_EXPECTED_GPSRO_REF INTERACTIVE_GPSRO_REF                      @                              
       R8 MISSING_R8 RAD2DEG DEG2RAD PI                      @                              
  	     REGISTER_MODULE ERROR_HANDLER E_ERR NMLFILEUNIT CHECK_NAMELIST_READ FIND_NAMELIST_IN_FILE DO_NML_FILE DO_NML_TERM ASCII_FILE_FORMAT          @          @                              
       LOCATION_TYPE SET_LOCATION GET_LOCATION IS_VERTICAL VERTISHEIGHT                      @                              
       INTERPOLATE                      @                              
       QTY_TEMPERATURE QTY_SPECIFIC_HUMIDITY QTY_PRESSURE          @          @                              
       ENSEMBLE_TYPE                      @                              
       TRACK_STATUS               @                                       u #SET_LOCATION_SINGLE    #SET_LOCATION_ARRAY    &         @   @                                                      #LON 	   #LAT 
   #VERT_LOC    #WHICH_VERT    #LOCATION_TYPE              
                                 	     	                
                                 
     	                
                                      	                
                                             &         @   @                                                       #LIST    #LOCATION_TYPE              
                                                   	              &                                                             @                                '                    #LON    #LAT    #VLOC    #WHICH_VERT                  D                                             	                 D                                            	                 D                                            	                 D                                                              @  @               @                'h                   #NUM_VARS    #NUM_COPIES    #MY_NUM_COPIES    #MY_NUM_VARS    #MY_COPIES    #MY_VARS    #COPIES    #VARS    #TIME    #DISTRIBUTION_TYPE !   #VALID "   #ID_NUM #   #TASK_TO_PE_LIST $   #PE_TO_TASK_LIST %   #MY_PE &   #LAYOUT_TYPE '   #TRANSPOSE_TYPE (   #NUM_EXTRAS )   #CURRENT_TIME *                 $                                                              $                                                              $                                                              $                                                            $                                                                       &                                                       $                                         `                             &                                                       $                                         ¨                 	            &                   &                                                       $                                                         	            &                   &                                                       $                                          h             	      #TIME_TYPE              &                                                          Ŕ  @                               '                    #SECONDS    #DAYS                   D                                                              D                                                              $                              !     °      
                    $                              "     ´                          $                              #     ¸                        $                              $            Ŕ                            &                                                      $                              %                                        &                                                         $                              &     P                          $                              '     T                          $                              (     X                          $                              )     \                          $                              *            `             #TIME_TYPE                   @  @               D           +     'h                   #NUM_VARS ,   #NUM_COPIES -   #MY_NUM_COPIES .   #MY_NUM_VARS /   #MY_COPIES 0   #MY_VARS 1   #COPIES 2   #VARS 3   #TIME 4   #DISTRIBUTION_TYPE 5   #VALID 6   #ID_NUM 7   #TASK_TO_PE_LIST 8   #PE_TO_TASK_LIST 9   #MY_PE :   #LAYOUT_TYPE ;   #TRANSPOSE_TYPE <   #NUM_EXTRAS =   #CURRENT_TIME >                 $                             ,                                 $                              -                                $                              .                                $                              /                              $                              0                                         &                                                       $                             1            `                             &                                                       $                             2            ¨                 	            &                   &                                                       $                             3                            	            &                   &                                                       $                              4            h             	      #TIME_TYPE              &                                                         $                              5     °      
                    $                              6     ´                          $                              7     ¸                        $                              8            Ŕ                            &                                                      $                              9                                        &                                                         $                              :     P                          $                              ;     T                          $                              <     X                          $                              =     \                          $                              >            `             #TIME_TYPE                  @  @               Ä           ?     '                    #NT @   #TYPE_TO_CUTOFF_MAP A   #GTT B                 $                              @                              $                              A                                         &                                                       $                              B            P                   #GET_CLOSE_TYPE_BY_TYPE C             &                                                         Ŕ  @              D           C     '                   #NUM D   #MAXDIST E   #LON_OFFSET F   #LOC_BOX G   #COUNT H   #START I   #BOT_LAT J   #TOP_LAT K   #BOT_LON L   #TOP_LON M   #LON_WIDTH N   #LAT_WIDTH O   #LON_CYCLIC P                 D                             D                                 D                            E               	              D                             F                                         &                   &                                                      D                             G            h                             &                                                      D                             H            °                             &                   &                                                      D                             I                                        &                   &                                                         D                            J     p         	                 D                            K     t         	                 D                            L     x      	   	                 D                            M     |      
   	                 D                            N              	                 D                            O              	                 D                             P                 #         @                                  Q                    #GPSKEY R   #NX S   #NY T   #NZ U   #RFICT0 V   #DS W   #HTOP X   #SUBSET0 Y             D                                 R                      
                                 S     	                
                                 T     	                
                                 U     	                
                                 V     	                
                                 W     	                
                                 X     	                
                                 Y                           #         @                                   Z                    #GPSKEY [   #NX \   #NY ]   #NZ ^   #RFICT0 _   #DS `   #HTOP a   #SUBSET0 b             
                                  [                     D                                \     	                 D                                ]     	                 D                                ^     	                 D                                _     	                 D                                `     	                 D                                a     	                 D                                b                            #         @                                   c                    #GPSKEY d   #IFILE e   #FFORM f             
                                  d                     
                                  e                     
 @                             f                    1 #         @                                   g                    #GPSKEY h   #IFILE i   #FFORM j             D @                               h                      
                                  i                     
 @                             j                    1 #         @                                   k                    #STATE_HANDLE l   #ENS_SIZE m   #LOCATION n   #GPSKEY o   #RO_REF p   #ISTATUS q             
  @                               l     h             #ENSEMBLE_TYPE              
  @                               m                     
  @                               n                   #LOCATION_TYPE              
                                  o                    D @                              p                    	     p          5  p        r m       5  p        r m                              D @                               q                         p          5  p        r m       5  p        r m                     #         @                                   r                    #GPSKEY s             D @                               s                   |      fn#fn %     x   b   uapp(OBS_DEF_GPS_MOD      a   J  TYPES_MOD    ő  Ä   J  UTILITIES_MOD    š     J  LOCATION_MOD     :  L   J  ASSIM_MODEL_MOD      s   J  OBS_KIND_MOD %   ů  N   J  ENSEMBLE_MANAGER_MOD &   G  M   J  OBS_DEF_UTILITIES_MOD .     q       gen@SET_LOCATION+LOCATION_MOD 1           SET_LOCATION_SINGLE+LOCATION_MOD 5     @   a   SET_LOCATION_SINGLE%LON+LOCATION_MOD 5   Ř  @   a   SET_LOCATION_SINGLE%LAT+LOCATION_MOD :     @   a   SET_LOCATION_SINGLE%VERT_LOC+LOCATION_MOD <   X  @   a   SET_LOCATION_SINGLE%WHICH_VERT+LOCATION_MOD 0     m      SET_LOCATION_ARRAY+LOCATION_MOD 5        a   SET_LOCATION_ARRAY%LIST+LOCATION_MOD +     |       LOCATION_TYPE+LOCATION_MOD 3     H   %   LOCATION_TYPE%LON+LOCATION_MOD=LON 3   U  H   %   LOCATION_TYPE%LAT+LOCATION_MOD=LAT 5     H   %   LOCATION_TYPE%VLOC+LOCATION_MOD=VLOC A   ĺ  H   %   LOCATION_TYPE%WHICH_VERT+LOCATION_MOD=WHICH_VERT 3   -	  x     ENSEMBLE_TYPE+ENSEMBLE_MANAGER_MOD <   Ľ
  H   a   ENSEMBLE_TYPE%NUM_VARS+ENSEMBLE_MANAGER_MOD >   í
  H   a   ENSEMBLE_TYPE%NUM_COPIES+ENSEMBLE_MANAGER_MOD A   5  H   a   ENSEMBLE_TYPE%MY_NUM_COPIES+ENSEMBLE_MANAGER_MOD ?   }  H   a   ENSEMBLE_TYPE%MY_NUM_VARS+ENSEMBLE_MANAGER_MOD =   Ĺ     a   ENSEMBLE_TYPE%MY_COPIES+ENSEMBLE_MANAGER_MOD ;   Y     a   ENSEMBLE_TYPE%MY_VARS+ENSEMBLE_MANAGER_MOD :   í  Ź   a   ENSEMBLE_TYPE%COPIES+ENSEMBLE_MANAGER_MOD 8     Ź   a   ENSEMBLE_TYPE%VARS+ENSEMBLE_MANAGER_MOD 8   E  Ł   a   ENSEMBLE_TYPE%TIME+ENSEMBLE_MANAGER_MOD +   č  g      TIME_TYPE+TIME_MANAGER_MOD ;   O  H   %   TIME_TYPE%SECONDS+TIME_MANAGER_MOD=SECONDS 5     H   %   TIME_TYPE%DAYS+TIME_MANAGER_MOD=DAYS E   ß  H   a   ENSEMBLE_TYPE%DISTRIBUTION_TYPE+ENSEMBLE_MANAGER_MOD 9   '  H   a   ENSEMBLE_TYPE%VALID+ENSEMBLE_MANAGER_MOD :   o  H   a   ENSEMBLE_TYPE%ID_NUM+ENSEMBLE_MANAGER_MOD C   ˇ     a   ENSEMBLE_TYPE%TASK_TO_PE_LIST+ENSEMBLE_MANAGER_MOD C   K     a   ENSEMBLE_TYPE%PE_TO_TASK_LIST+ENSEMBLE_MANAGER_MOD 9   ß  H   a   ENSEMBLE_TYPE%MY_PE+ENSEMBLE_MANAGER_MOD ?   '  H   a   ENSEMBLE_TYPE%LAYOUT_TYPE+ENSEMBLE_MANAGER_MOD B   o  H   a   ENSEMBLE_TYPE%TRANSPOSE_TYPE+ENSEMBLE_MANAGER_MOD >   ˇ  H   a   ENSEMBLE_TYPE%NUM_EXTRAS+ENSEMBLE_MANAGER_MOD @   ˙  _   a   ENSEMBLE_TYPE%CURRENT_TIME+ENSEMBLE_MANAGER_MOD 3   ^  x     ENSEMBLE_TYPE+ENSEMBLE_MANAGER_MOD <   Ö  H   a   ENSEMBLE_TYPE%NUM_VARS+ENSEMBLE_MANAGER_MOD >     H   a   ENSEMBLE_TYPE%NUM_COPIES+ENSEMBLE_MANAGER_MOD A   f  H   a   ENSEMBLE_TYPE%MY_NUM_COPIES+ENSEMBLE_MANAGER_MOD ?   Ž  H   a   ENSEMBLE_TYPE%MY_NUM_VARS+ENSEMBLE_MANAGER_MOD =   ö     a   ENSEMBLE_TYPE%MY_COPIES+ENSEMBLE_MANAGER_MOD ;        a   ENSEMBLE_TYPE%MY_VARS+ENSEMBLE_MANAGER_MOD :     Ź   a   ENSEMBLE_TYPE%COPIES+ENSEMBLE_MANAGER_MOD 8   Ę  Ź   a   ENSEMBLE_TYPE%VARS+ENSEMBLE_MANAGER_MOD 8   v  Ł   a   ENSEMBLE_TYPE%TIME+ENSEMBLE_MANAGER_MOD E     H   a   ENSEMBLE_TYPE%DISTRIBUTION_TYPE+ENSEMBLE_MANAGER_MOD 9   a  H   a   ENSEMBLE_TYPE%VALID+ENSEMBLE_MANAGER_MOD :   Š  H   a   ENSEMBLE_TYPE%ID_NUM+ENSEMBLE_MANAGER_MOD C   ń     a   ENSEMBLE_TYPE%TASK_TO_PE_LIST+ENSEMBLE_MANAGER_MOD C        a   ENSEMBLE_TYPE%PE_TO_TASK_LIST+ENSEMBLE_MANAGER_MOD 9     H   a   ENSEMBLE_TYPE%MY_PE+ENSEMBLE_MANAGER_MOD ?   a  H   a   ENSEMBLE_TYPE%LAYOUT_TYPE+ENSEMBLE_MANAGER_MOD B   Š  H   a   ENSEMBLE_TYPE%TRANSPOSE_TYPE+ENSEMBLE_MANAGER_MOD >   ń  H   a   ENSEMBLE_TYPE%NUM_EXTRAS+ENSEMBLE_MANAGER_MOD @   9  _   a   ENSEMBLE_TYPE%CURRENT_TIME+ENSEMBLE_MANAGER_MOD ,     y      GET_CLOSE_TYPE+LOCATION_MOD /     H   a   GET_CLOSE_TYPE%NT+LOCATION_MOD ?   Y     a   GET_CLOSE_TYPE%TYPE_TO_CUTOFF_MAP+LOCATION_MOD 0   í  °   a   GET_CLOSE_TYPE%GTT+LOCATION_MOD 4     ű      GET_CLOSE_TYPE_BY_TYPE+LOCATION_MOD <     H   %   GET_CLOSE_TYPE_BY_TYPE%NUM+LOCATION_MOD=NUM D   ŕ  H   %   GET_CLOSE_TYPE_BY_TYPE%MAXDIST+LOCATION_MOD=MAXDIST J   (   Ź   %   GET_CLOSE_TYPE_BY_TYPE%LON_OFFSET+LOCATION_MOD=LON_OFFSET D   Ô      %   GET_CLOSE_TYPE_BY_TYPE%LOC_BOX+LOCATION_MOD=LOC_BOX @   h!  Ź   %   GET_CLOSE_TYPE_BY_TYPE%COUNT+LOCATION_MOD=COUNT @   "  Ź   %   GET_CLOSE_TYPE_BY_TYPE%START+LOCATION_MOD=START D   Ŕ"  H   %   GET_CLOSE_TYPE_BY_TYPE%BOT_LAT+LOCATION_MOD=BOT_LAT D   #  H   %   GET_CLOSE_TYPE_BY_TYPE%TOP_LAT+LOCATION_MOD=TOP_LAT D   P#  H   %   GET_CLOSE_TYPE_BY_TYPE%BOT_LON+LOCATION_MOD=BOT_LON D   #  H   %   GET_CLOSE_TYPE_BY_TYPE%TOP_LON+LOCATION_MOD=TOP_LON H   ŕ#  H   %   GET_CLOSE_TYPE_BY_TYPE%LON_WIDTH+LOCATION_MOD=LON_WIDTH H   ($  H   %   GET_CLOSE_TYPE_BY_TYPE%LAT_WIDTH+LOCATION_MOD=LAT_WIDTH J   p$  H   %   GET_CLOSE_TYPE_BY_TYPE%LON_CYCLIC+LOCATION_MOD=LON_CYCLIC    ¸$         SET_GPSRO_REF %   O%  @   a   SET_GPSRO_REF%GPSKEY !   %  @   a   SET_GPSRO_REF%NX !   Ď%  @   a   SET_GPSRO_REF%NY !   &  @   a   SET_GPSRO_REF%NZ %   O&  @   a   SET_GPSRO_REF%RFICT0 !   &  @   a   SET_GPSRO_REF%DS #   Ď&  @   a   SET_GPSRO_REF%HTOP &   '  P   a   SET_GPSRO_REF%SUBSET0    _'         GET_GPSRO_REF %   ö'  @   a   GET_GPSRO_REF%GPSKEY !   6(  @   a   GET_GPSRO_REF%NX !   v(  @   a   GET_GPSRO_REF%NY !   ś(  @   a   GET_GPSRO_REF%NZ %   ö(  @   a   GET_GPSRO_REF%RFICT0 !   6)  @   a   GET_GPSRO_REF%DS #   v)  @   a   GET_GPSRO_REF%HTOP &   ś)  P   a   GET_GPSRO_REF%SUBSET0     *  j       WRITE_GPSRO_REF '   p*  @   a   WRITE_GPSRO_REF%GPSKEY &   °*  @   a   WRITE_GPSRO_REF%IFILE &   đ*  L   a   WRITE_GPSRO_REF%FFORM    <+  j       READ_GPSRO_REF &   Ś+  @   a   READ_GPSRO_REF%GPSKEY %   ć+  @   a   READ_GPSRO_REF%IFILE %   &,  L   a   READ_GPSRO_REF%FFORM '   r,         GET_EXPECTED_GPSRO_REF 4   -  [   a   GET_EXPECTED_GPSRO_REF%STATE_HANDLE 0   h-  @   a   GET_EXPECTED_GPSRO_REF%ENS_SIZE 0   ¨-  [   a   GET_EXPECTED_GPSRO_REF%LOCATION .   .  @   a   GET_EXPECTED_GPSRO_REF%GPSKEY .   C.  ´   a   GET_EXPECTED_GPSRO_REF%RO_REF /   ÷.  ´   a   GET_EXPECTED_GPSRO_REF%ISTATUS &   Ť/  T       INTERACTIVE_GPSRO_REF -   ˙/  @   a   INTERACTIVE_GPSRO_REF%GPSKEY 