  Q  ź   k820309    ,          2021.5.0    wŤúc                                                                                                          
       /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/location/threed_sphere/location_mod.f90 LOCATION_MOD       '       LOCATION_TYPE GET_LOCATION SET_LOCATION_MISSING IS_LOCATION_IN_REGION GET_MAXDIST WRITE_LOCATION READ_LOCATION INTERACTIVE_LOCATION QUERY_LOCATION LOCATIONDIMS LOCATIONNAME LOCATIONLNAME LOCATIONSTORAGEORDER LOCATIONUNITS GET_CLOSE_TYPE GET_CLOSE_INIT GET_CLOSE_OBS GET_CLOSE_STATE GET_CLOSE_DESTROY GET_DIST HAS_VERTICAL_CHOICE VERTICAL_LOCALIZATION_ON SET_VERTICAL IS_VERTICAL GET_VERTICAL_LOCALIZATION_COORD GET_CLOSE SET_VERTICAL_LOCALIZATION_COORD CONVERT_VERTICAL_OBS CONVERT_VERTICAL_STATE VERTISUNDEF VERTISSURFACE VERTISLEVEL VERTISPRESSURE VERTISHEIGHT VERTISSCALEHEIGHT PRINT_GET_CLOSE_TYPE i@ i@ gen@SET_LOCATION                      @                              
       R8 MISSING_R8 MISSING_I PI RAD2DEG DEG2RAD OBSTYPELENGTH I8                      @                              
       ERROR_HANDLER E_ERR ASCII_FILE_FORMAT E_MSG OPEN_FILE CLOSE_FILE SET_OUTPUT LOGFILEUNIT NMLFILEUNIT FIND_NAMELIST_IN_FILE CHECK_NAMELIST_READ DO_OUTPUT DO_NML_FILE DO_NML_TERM IS_LONGITUDE_BETWEEN                      @                              
       RANDOM_SEQ_TYPE INIT_RANDOM_SEQ RANDOM_UNIFORM                      @                              
       GET_NUM_TYPES_OF_OBS GET_NAME_FOR_TYPE_OF_OBS GET_INDEX_FOR_TYPE_OF_OBS                      @                              
       MY_TASK_ID TASK_COUNT                      @                              
       ENSEMBLE_TYPE                                                            #LOC_EQ    %         @   @X                                                       #LOC1    #LOC2 
             
                                                     #LOCATION_TYPE 	             
                                  
                   #LOCATION_TYPE 	                                                              #LOC_NE    %         @   @X                                                        #LOC1    #LOC2              
  @                                                  #LOCATION_TYPE 	             
  @                                                  #LOCATION_TYPE 	                                                          u #SET_LOCATION_SINGLE    #SET_LOCATION_ARRAY    &         @   @X                                                       #LON    #LAT    #VERT_LOC    #WHICH_VERT    #LOCATION_TYPE 	             
                                      	                
                                      	                
                                      	                
                                             &         @   @X                                                        #LIST    #LOCATION_TYPE 	             
 @                                                 	              &                                                          Ŕ  @                                '                   #MTI    #MT    #LASTG    #GSET                  D                                                               D                                  p                         p           & p         p o          p p                                      D                                           	                 D                                                             @  @               @                'h                   #NUM_VARS    #NUM_COPIES    #MY_NUM_COPIES    #MY_NUM_VARS    #MY_COPIES    #MY_VARS     #COPIES !   #VARS "   #TIME #   #DISTRIBUTION_TYPE '   #VALID (   #ID_NUM )   #TASK_TO_PE_LIST *   #PE_TO_TASK_LIST +   #MY_PE ,   #LAYOUT_TYPE -   #TRANSPOSE_TYPE .   #NUM_EXTRAS /   #CURRENT_TIME 0                 $                                                              $                                                              $                                                              $                                                            $                                                                       &                                                       $                                          `                             &                                                       $                             !            ¨                 	            &                   &                                                       $                             "                            	            &                   &                                                       $                              #            h             	      #TIME_TYPE $             &                                                         Ŕ  @                         $     '                    #SECONDS %   #DAYS &                 D                             %                                 D                             &                                $                              '     °      
                    $                              (     ´                          $                              )     ¸                        $                              *            Ŕ                            &                                                      $                              +                                        &                                                         $                              ,     P                          $                              -     T                          $                              .     X                          $                              /     \                          $                              0            `             #TIME_TYPE $                     @                           	     '                    #LON 1   #LAT 2   #VLOC 3   #WHICH_VERT 4                 D                             1                	                 D                             2               	                 D                             3               	                 D                              4                  (         `                                5                                   	    #LOC 6   p          p            p                                    
                                  6                   #LOCATION_TYPE 	   &         @                                 7                            #LOCATION_TYPE 	   %         @                                 8                           #LOC 9   #MINL :   #MAXL ;             
                                  9                   #LOCATION_TYPE 	             
                                  :                   #LOCATION_TYPE 	             
                                  ;                   #LOCATION_TYPE 	   %         @                                <                    	       #GC =   #OBS_TYPE ?             
                                  =                   #GET_CLOSE_TYPE >             
                                 ?           #         @                                  @                    #LOCFILE A   #LOC B   #FFORM C   #CHARSTRING D             
                                  A                     
                                  B                   #LOCATION_TYPE 	             
 @                             C                    1           F @                             D                     1 &         @                                 E                           #LOCFILE F   #FFORM G   #LOCATION_TYPE 	             
                                  F                     
 @                             G                    1 #         @                                   H                    #LOCATION I   #SET_TO_DEFAULT J             D                                 I                    #LOCATION_TYPE 	             
 @                               J           %         @                                K                    	       #LOC L   #ATTR M             
                                  L                   #LOCATION_TYPE 	             
 @                             M                    1                                              N                                                      3                                           O                                                        Cloc3Dsphere                                                           P     +                            ,                       Cthreed sphere locations: lon, lat, vertical                                                           Q                                                        CLon Lat Vertical                                                           R                                                        Cdegrees degrees which_vert                                  @               Ŕ           >     '                    #NT S   #TYPE_TO_CUTOFF_MAP T   #GTT U                 $                              S                              $                              T                                         &                                                       $                              U            P                   #GET_CLOSE_TYPE_BY_TYPE V             &                                                         Ŕ  @              @           V     '                   #NUM W   #MAXDIST X   #LON_OFFSET Y   #LOC_BOX Z   #COUNT [   #START \   #BOT_LAT ]   #TOP_LAT ^   #BOT_LON _   #TOP_LON `   #LON_WIDTH a   #LAT_WIDTH b   #LON_CYCLIC c                D                              W                                 D                             X               	              D                              Y                                         &                   &                                                      D                              Z            h                             &                                                      D                              [            °                             &                   &                                                      D                              \                                        &                   &                                                         D                             ]     p         	                 D                             ^     t         	                 D                             _     x      	   	                 D                             `     |      
   	                 D                             a              	                 D                             b              	                 D                              c                 #         @                                   d                    #GC e   #NUM f   #MAXDIST g   #LOCS h   #MAXDIST_LIST i             
D @                               e                    #GET_CLOSE_TYPE >             
  @                               f                     
                                 g     	                
  @                               h                                  &                                           #LOCATION_TYPE 	             
@                              i                   	              &                                           #         @                                   j                 
   #GC k   #BASE_LOC l   #BASE_TYPE m   #LOCS n   #LOC_QTYS o   #LOC_TYPES p   #NUM_CLOSE q   #CLOSE_IND r   #DIST s   #ENS_HANDLE t             
  @                               k                   #GET_CLOSE_TYPE >             
D @                               l                    #LOCATION_TYPE 	             
  @                               m                     
D @                               n                   "                &                                           #LOCATION_TYPE 	             
  @                               o                    #             &                                                     
                                  p                    $             &                                                     D @                               q                      D @                               r                    %              &                                                     F @                              s                   	 &              &                                                     
 @                               t     h             #ENSEMBLE_TYPE    #         @                                   u                 
   #GC v   #BASE_LOC w   #BASE_TYPE x   #LOCS y   #LOC_QTYS z   #LOC_INDX {   #NUM_CLOSE |   #CLOSE_IND }   #DIST ~   #ENS_HANDLE              
  @                               v                   #GET_CLOSE_TYPE >             
D @                               w                    #LOCATION_TYPE 	             
  @                               x                     
D @                               y                   '                &                                           #LOCATION_TYPE 	             
  @                               z                    (             &                                                     
                                 {                    )             &                                                     D @                               |                      D @                               }                    *              &                                                     F @                              ~                   	 +              &                                                     
 @                                    h             #ENSEMBLE_TYPE    #         @                                                       #GC              
D                                                     #GET_CLOSE_TYPE >   %         @                                                   	       #LOC1    #LOC2    #TYPE1    #KIND2    #NO_VERT              
  @                                                  #LOCATION_TYPE 	             
  @                                                  #LOCATION_TYPE 	             
 @                                                    
                                                      
 @                                          %         @                                                             %         @                                                             #         @                                                       #LOC    #VLOC    #WHICH_VERT              
D                                                     #LOCATION_TYPE 	             
 @                                   	                
 @                                          %         @                                                            #LOC    #WHICH_VERT              
                                                     #LOCATION_TYPE 	             
                                                    1 %         @                                                             #         @                                                   	   #GC    #BASE_LOC    #BASE_TYPE    #LOCS    #LOC_QTYS    #NUM_CLOSE    #CLOSE_IND    #DIST    #ENS_HANDLE              
                                                     #GET_CLOSE_TYPE >             
 @                                                   #LOCATION_TYPE 	             
  @                                                 0  
@                                                  ,                &                                           #LOCATION_TYPE 	             
  @                                                   -             &                                                     D                                                       D                                                     .              &                                                     F @                                                 	 /              &                                                     
                                      h             #ENSEMBLE_TYPE    #         @                                                       #WHICH_VERT              
                                             #         @                                                       #ENS_HANDLE    #NUM     #LOCS Ą   #LOC_QTYS ˘   #LOC_TYPES Ł   #WHICH_VERT ¤   #STATUS Ľ             
                                       h             #ENSEMBLE_TYPE              
                                                        
                                 Ą                                   &                                           #LOCATION_TYPE 	             
                                  ˘                                 &                                                     
                                  Ł                                 &                                                     
                                  ¤                     D                                 Ľ                                  &                                           #         @                                   Ś                    #ENS_HANDLE §   #NUM ¨   #LOCS Š   #LOC_QTYS Ş   #LOC_INDX Ť   #WHICH_VERT Ź   #ISTATUS ­             
                                  §     h             #ENSEMBLE_TYPE              
                                  ¨                     
                                 Š                                   &                                           #LOCATION_TYPE 	             
                                  Ş                                 &                                                     
                                 Ť                                 &                                                     
                                  Ź                     D                                 ­                                                         Ž                                          ţ˙˙˙˙˙˙˙                                                     Ż                                          ˙˙˙˙˙˙˙˙                                                     °                                                      1                                             ą                                                      2                                             ˛                                                      3                                             ł                                                      4#         @                                  ´                    #GC ľ   #TT ś   #AMOUNT ˇ            
                                  ľ                   #GET_CLOSE_TYPE >             
 @                               ś                     
 @                               ˇ                        fn#fn "   $    b   uapp(LOCATION_MOD    Š  |   J  TYPES_MOD    %    J  UTILITIES_MOD    *  o   J  RANDOM_SEQ_MOD         J  OBS_KIND_MOD "   !  V   J  MPI_UTILITIES_MOD %   w  N   J  ENSEMBLE_MANAGER_MOD    Ĺ  L      i@      d      LOC_EQ    u  [   a   LOC_EQ%LOC1    Đ  [   a   LOC_EQ%LOC2    +  L      i@    w  d      LOC_NE    Ű  [   a   LOC_NE%LOC1    6	  [   a   LOC_NE%LOC2 !   	  q       gen@SET_LOCATION $   
        SET_LOCATION_SINGLE (   
  @   a   SET_LOCATION_SINGLE%LON (   Ő
  @   a   SET_LOCATION_SINGLE%LAT -     @   a   SET_LOCATION_SINGLE%VERT_LOC /   U  @   a   SET_LOCATION_SINGLE%WHICH_VERT #     m      SET_LOCATION_ARRAY (        a   SET_LOCATION_ARRAY%LIST /     v      RANDOM_SEQ_TYPE+RANDOM_SEQ_MOD 7     H   %   RANDOM_SEQ_TYPE%MTI+RANDOM_SEQ_MOD=MTI 5   L  Ź   %   RANDOM_SEQ_TYPE%MT+RANDOM_SEQ_MOD=MT ;   ř  H   %   RANDOM_SEQ_TYPE%LASTG+RANDOM_SEQ_MOD=LASTG 9   @  H   %   RANDOM_SEQ_TYPE%GSET+RANDOM_SEQ_MOD=GSET 3     x     ENSEMBLE_TYPE+ENSEMBLE_MANAGER_MOD <      H   a   ENSEMBLE_TYPE%NUM_VARS+ENSEMBLE_MANAGER_MOD >   H  H   a   ENSEMBLE_TYPE%NUM_COPIES+ENSEMBLE_MANAGER_MOD A     H   a   ENSEMBLE_TYPE%MY_NUM_COPIES+ENSEMBLE_MANAGER_MOD ?   Ř  H   a   ENSEMBLE_TYPE%MY_NUM_VARS+ENSEMBLE_MANAGER_MOD =         a   ENSEMBLE_TYPE%MY_COPIES+ENSEMBLE_MANAGER_MOD ;   ´     a   ENSEMBLE_TYPE%MY_VARS+ENSEMBLE_MANAGER_MOD :   H  Ź   a   ENSEMBLE_TYPE%COPIES+ENSEMBLE_MANAGER_MOD 8   ô  Ź   a   ENSEMBLE_TYPE%VARS+ENSEMBLE_MANAGER_MOD 8      Ł   a   ENSEMBLE_TYPE%TIME+ENSEMBLE_MANAGER_MOD +   C  g      TIME_TYPE+TIME_MANAGER_MOD ;   Ş  H   %   TIME_TYPE%SECONDS+TIME_MANAGER_MOD=SECONDS 5   ň  H   %   TIME_TYPE%DAYS+TIME_MANAGER_MOD=DAYS E   :  H   a   ENSEMBLE_TYPE%DISTRIBUTION_TYPE+ENSEMBLE_MANAGER_MOD 9     H   a   ENSEMBLE_TYPE%VALID+ENSEMBLE_MANAGER_MOD :   Ę  H   a   ENSEMBLE_TYPE%ID_NUM+ENSEMBLE_MANAGER_MOD C        a   ENSEMBLE_TYPE%TASK_TO_PE_LIST+ENSEMBLE_MANAGER_MOD C   Ś     a   ENSEMBLE_TYPE%PE_TO_TASK_LIST+ENSEMBLE_MANAGER_MOD 9   :  H   a   ENSEMBLE_TYPE%MY_PE+ENSEMBLE_MANAGER_MOD ?     H   a   ENSEMBLE_TYPE%LAYOUT_TYPE+ENSEMBLE_MANAGER_MOD B   Ę  H   a   ENSEMBLE_TYPE%TRANSPOSE_TYPE+ENSEMBLE_MANAGER_MOD >     H   a   ENSEMBLE_TYPE%NUM_EXTRAS+ENSEMBLE_MANAGER_MOD @   Z  _   a   ENSEMBLE_TYPE%CURRENT_TIME+ENSEMBLE_MANAGER_MOD    š  |       LOCATION_TYPE "   5  H   !   LOCATION_TYPE%LON "   }  H   !   LOCATION_TYPE%LAT #   Ĺ  H   !   LOCATION_TYPE%VLOC )     H   !   LOCATION_TYPE%WHICH_VERT    U  ­       GET_LOCATION !     [   a   GET_LOCATION%LOC %   ]  c       SET_LOCATION_MISSING &   Ŕ  m       IS_LOCATION_IN_REGION *   -  [   a   IS_LOCATION_IN_REGION%LOC +     [   a   IS_LOCATION_IN_REGION%MINL +   ă  [   a   IS_LOCATION_IN_REGION%MAXL    >  f       GET_MAXDIST    ¤  \   a   GET_MAXDIST%GC %      @   a   GET_MAXDIST%OBS_TYPE    @  y       WRITE_LOCATION '   š  @   a   WRITE_LOCATION%LOCFILE #   ů  [   a   WRITE_LOCATION%LOC %   T  L   a   WRITE_LOCATION%FFORM *      L   a   WRITE_LOCATION%CHARSTRING    ě  {       READ_LOCATION &   g   @   a   READ_LOCATION%LOCFILE $   §   L   a   READ_LOCATION%FFORM %   ó   j       INTERACTIVE_LOCATION .   ]!  [   a   INTERACTIVE_LOCATION%LOCATION 4   ¸!  @   a   INTERACTIVE_LOCATION%SET_TO_DEFAULT    ř!  c       QUERY_LOCATION #   ["  [   a   QUERY_LOCATION%LOC $   ś"  L   a   QUERY_LOCATION%ATTR    #  q       LOCATIONDIMS    s#         LOCATIONNAME    ˙#  Ź       LOCATIONLNAME %   Ť$         LOCATIONSTORAGEORDER    <%         LOCATIONUNITS    ×%  y       GET_CLOSE_TYPE "   P&  H   a   GET_CLOSE_TYPE%NT 2   &     a   GET_CLOSE_TYPE%TYPE_TO_CUTOFF_MAP #   ,'  °   a   GET_CLOSE_TYPE%GTT '   Ü'  ű       GET_CLOSE_TYPE_BY_TYPE +   ×(  H   !   GET_CLOSE_TYPE_BY_TYPE%NUM /   )  H   !   GET_CLOSE_TYPE_BY_TYPE%MAXDIST 2   g)  Ź   !   GET_CLOSE_TYPE_BY_TYPE%LON_OFFSET /   *     !   GET_CLOSE_TYPE_BY_TYPE%LOC_BOX -   §*  Ź   !   GET_CLOSE_TYPE_BY_TYPE%COUNT -   S+  Ź   !   GET_CLOSE_TYPE_BY_TYPE%START /   ˙+  H   !   GET_CLOSE_TYPE_BY_TYPE%BOT_LAT /   G,  H   !   GET_CLOSE_TYPE_BY_TYPE%TOP_LAT /   ,  H   !   GET_CLOSE_TYPE_BY_TYPE%BOT_LON /   ×,  H   !   GET_CLOSE_TYPE_BY_TYPE%TOP_LON 1   -  H   !   GET_CLOSE_TYPE_BY_TYPE%LON_WIDTH 1   g-  H   !   GET_CLOSE_TYPE_BY_TYPE%LAT_WIDTH 2   Ż-  H   !   GET_CLOSE_TYPE_BY_TYPE%LON_CYCLIC    ÷-         GET_CLOSE_INIT "   y.  \   a   GET_CLOSE_INIT%GC #   Ő.  @   a   GET_CLOSE_INIT%NUM '   /  @   a   GET_CLOSE_INIT%MAXDIST $   U/     a   GET_CLOSE_INIT%LOCS ,   ô/     a   GET_CLOSE_INIT%MAXDIST_LIST    0  Ě       GET_CLOSE_OBS !   L1  \   a   GET_CLOSE_OBS%GC '   ¨1  [   a   GET_CLOSE_OBS%BASE_LOC (   2  @   a   GET_CLOSE_OBS%BASE_TYPE #   C2     a   GET_CLOSE_OBS%LOCS '   â2     a   GET_CLOSE_OBS%LOC_QTYS (   n3     a   GET_CLOSE_OBS%LOC_TYPES (   ú3  @   a   GET_CLOSE_OBS%NUM_CLOSE (   :4     a   GET_CLOSE_OBS%CLOSE_IND #   Ć4     a   GET_CLOSE_OBS%DIST )   R5  [   a   GET_CLOSE_OBS%ENS_HANDLE     ­5  Ë       GET_CLOSE_STATE #   x6  \   a   GET_CLOSE_STATE%GC )   Ô6  [   a   GET_CLOSE_STATE%BASE_LOC *   /7  @   a   GET_CLOSE_STATE%BASE_TYPE %   o7     a   GET_CLOSE_STATE%LOCS )   8     a   GET_CLOSE_STATE%LOC_QTYS )   8     a   GET_CLOSE_STATE%LOC_INDX *   &9  @   a   GET_CLOSE_STATE%NUM_CLOSE *   f9     a   GET_CLOSE_STATE%CLOSE_IND %   ň9     a   GET_CLOSE_STATE%DIST +   ~:  [   a   GET_CLOSE_STATE%ENS_HANDLE "   Ů:  P       GET_CLOSE_DESTROY %   );  \   a   GET_CLOSE_DESTROY%GC    ;         GET_DIST    <  [   a   GET_DIST%LOC1    g<  [   a   GET_DIST%LOC2    Â<  @   a   GET_DIST%TYPE1    =  @   a   GET_DIST%KIND2 !   B=  @   a   GET_DIST%NO_VERT $   =  P       HAS_VERTICAL_CHOICE )   Ň=  P       VERTICAL_LOCALIZATION_ON    ">  k       SET_VERTICAL !   >  [   a   SET_VERTICAL%LOC "   č>  @   a   SET_VERTICAL%VLOC (   (?  @   a   SET_VERTICAL%WHICH_VERT    h?  i       IS_VERTICAL     Ń?  [   a   IS_VERTICAL%LOC '   ,@  L   a   IS_VERTICAL%WHICH_VERT 0   x@  P       GET_VERTICAL_LOCALIZATION_COORD    Č@  ˝       GET_CLOSE    A  \   a   GET_CLOSE%GC #   áA  [   a   GET_CLOSE%BASE_LOC $   <B  @   a   GET_CLOSE%BASE_TYPE    |B     a   GET_CLOSE%LOCS #   C     a   GET_CLOSE%LOC_QTYS $   §C  @   a   GET_CLOSE%NUM_CLOSE $   çC     a   GET_CLOSE%CLOSE_IND    sD     a   GET_CLOSE%DIST %   ˙D  [   a   GET_CLOSE%ENS_HANDLE 0   ZE  X       SET_VERTICAL_LOCALIZATION_COORD ;   ˛E  @   a   SET_VERTICAL_LOCALIZATION_COORD%WHICH_VERT %   ňE  ¤       CONVERT_VERTICAL_OBS 0   F  [   a   CONVERT_VERTICAL_OBS%ENS_HANDLE )   ńF  @   a   CONVERT_VERTICAL_OBS%NUM *   1G     a   CONVERT_VERTICAL_OBS%LOCS .   ĐG     a   CONVERT_VERTICAL_OBS%LOC_QTYS /   \H     a   CONVERT_VERTICAL_OBS%LOC_TYPES 0   čH  @   a   CONVERT_VERTICAL_OBS%WHICH_VERT ,   (I     a   CONVERT_VERTICAL_OBS%STATUS '   ´I  ¤       CONVERT_VERTICAL_STATE 2   XJ  [   a   CONVERT_VERTICAL_STATE%ENS_HANDLE +   łJ  @   a   CONVERT_VERTICAL_STATE%NUM ,   óJ     a   CONVERT_VERTICAL_STATE%LOCS 0   K     a   CONVERT_VERTICAL_STATE%LOC_QTYS 0   L     a   CONVERT_VERTICAL_STATE%LOC_INDX 2   ŞL  @   a   CONVERT_VERTICAL_STATE%WHICH_VERT /   ęL  @   a   CONVERT_VERTICAL_STATE%ISTATUS    *M  p       VERTISUNDEF    M  p       VERTISSURFACE    
N  q       VERTISLEVEL    {N  q       VERTISPRESSURE    ěN  q       VERTISHEIGHT "   ]O  q       VERTISSCALEHEIGHT %   ÎO  d       PRINT_GET_CLOSE_TYPE (   2P  \   a   PRINT_GET_CLOSE_TYPE%GC (   P  @   a   PRINT_GET_CLOSE_TYPE%TT ,   ÎP  @   a   PRINT_GET_CLOSE_TYPE%AMOUNT 