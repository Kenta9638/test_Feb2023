  `Q  Ą   k820309    ,          2021.5.0    ”«śc                                                                                                          
       /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/io/io_filenames_mod.f90 IO_FILENAMES_MOD               IO_FILENAMES_INIT IO_FILENAMES_FINALIZE FILE_INFO_TYPE NETCDF_FILE_TYPE STAGE_METADATA_TYPE SET_MEMBER_FILE_METADATA ASSERT_FILE_INFO_INITIALIZED ASSERT_RESTART_NAMES_INITIALIZED FILE_INFO_DUMP COMBINE_FILE_INFO CHECK_FILE_INFO_VARIABLE_SHAPE GET_RESTART_FILENAME GET_SINGLE_FILE GET_CYCLING GET_FILE_DESCRIPTION GET_COPY_NAME GET_STAGE_METADATA SINGLE_FILE_INITIALIZED INHERIT_COPY_UNITS COPY_IS_CLAMPED FORCE_COPY_BACK NOUTPUT_STATE_VARIABLES QUERY_READ_COPY QUERY_WRITE_COPY QUERY_COPY_PRESENT READ_COPY WRITE_COPY READ_WRITE_COPY NO_IO COPY_NOT_PRESENT gen@SET_IO_COPY_FLAG gen@SET_FILE_METADATA                      @                              
                            @                              
       R4 R8 MISSING_R8 MAX_NUM_DOMS DIGITS12                      @                              
  	     FILE_EXIST E_ERR E_MSG E_WARN ERROR_HANDLER OPEN_FILE CLOSE_FILE FIND_TEXTFILE_DIMS DO_OUTPUT                      @                              
       TIME_TYPE                      @                              
       MY_TASK_ID                      @                              
       GET_NUM_DOMAINS GET_DIM_LENGTH GET_DIM_NAME GET_NUM_DIMS GET_IO_NUM_DIMS GET_NUM_VARIABLES GET_VARIABLE_NAME GET_UNITS GET_LONG_NAME GET_SHORT_NAME GET_MISSING_VALUE GET_FILLVALUE GET_XTYPE GET_ADD_OFFSET GET_SCALE_FACTOR GET_HAS_MISSING_VALUE GET_HAS_FILLVALUE DO_IO_UPDATE GET_IO_DIM_LENGTH                      @                              
       ENSEMBLE_TYPE                      @                              
       NC_CHECK                                                        u #SET_IO_COPY_FLAG_RANGE 	   #SET_IO_COPY_FLAG_SINGLE    #         @     @X                             	                    #FILE_INFO 
   #C1    #C2    #IO_FLAG    #NUM_OUTPUT_ENS    #INHERIT_UNITS    #CLAMP_VARS    #FORCE_COPY_BACK              
D                                 
     ų              #FILE_INFO_TYPE              
                                                       
                                                       
                                                       
 @                                                    
 @                                                    
 @                                                    
 @                                          #         @     @X                                                 #FILE_INFO    #CNUM    #IO_FLAG    #INHERIT_UNITS    #CLAMP_VARS    #FORCE_COPY_BACK              
D                                      ų              #FILE_INFO_TYPE              
                                                       
                                                       
 @                                                    
 @                                                    
 @                                                                                                u #SET_EXPLICIT_FILE_METADATA    #SET_STAGE_FILE_METADATA     #         @     @X                                                #FILE_INFO    #CNUM    #FNAMES    #BASENAME    #DESC              
D                                      ų              #FILE_INFO_TYPE              
                                             ,          
                                                                 &                                           1           
  @                                                 1           
  @                                                 1 #         @     @X                                                 #FILE_INFO !   #CNUM "   #STAGE #   #BASENAME $   #DESC %   #OFFSET &             
D                                 !     ų              #FILE_INFO_TYPE              
                                  "                     
                                #                    1           
  @                             $                    1           
  @                             %                    1           
 @                               &                         @                                      u #GET_MISSING_VALUE_R8 '   #GET_MISSING_VALUE_R4 +   #GET_MISSING_VALUE_INT /   #         @     @                            '                    #DOM_ID (   #VAR_ID )   #MISSING_VALUE_R8 *             
                                  (                     
                                  )                                                     *     
       #         @     @                           +                    #DOM_ID ,   #VAR_ID -   #MISSING_VALUE_R4 .             
                                  ,                     
                                  -                                                     .     	       #         @     @                           /                    #DOM_ID 0   #VAR_ID 1   #MISSING_VALUE_INT 2             
                                  0                     
                                  1                                                      2                          @                                      u #GET_SPVAL_R8 3   #GET_SPVAL_R4 7   #GET_SPVAL_INT ;   #         @     @                            3                    #DOM_ID 4   #VAR_ID 5   #SPVAL_R8 6             
                                  4                     
                                  5                                                     6     
       #         @     @                           7                    #DOM_ID 8   #VAR_ID 9   #SPVAL_R4 :             
                                  8                     
                                  9                                                     :     	       #         @     @                           ;                    #DOM_ID <   #VAR_ID =   #SPVAL_INT >             
                                  <                     
                                  =                                                      >                           Ą  @                          ?     '                    #SECONDS @   #DAYS A                 D                              @                                 D                              A                                @  @               @           B     'h                   #NUM_VARS C   #NUM_COPIES D   #MY_NUM_COPIES E   #MY_NUM_VARS F   #MY_COPIES G   #MY_VARS H   #COPIES I   #VARS J   #TIME K   #DISTRIBUTION_TYPE L   #VALID M   #ID_NUM N   #TASK_TO_PE_LIST O   #PE_TO_TASK_LIST P   #MY_PE Q   #LAYOUT_TYPE R   #TRANSPOSE_TYPE S   #NUM_EXTRAS T   #CURRENT_TIME U                 $                             C                                 $                              D                                $                              E                                $                              F                              $                              G                                         &                                                       $                             H            `                             &                                                       $                             I            Ø                 	            &                   &                                                       $                             J                            	            &                   &                                                       $                              K            h             	      #TIME_TYPE ?             &                                                         $                              L     °      
                    $                              M     “                          $                              N     ø                        $                              O            Ą                            &                                                      $                              P                                        &                                                         $                              Q     P                          $                              R     T                          $                              S     X                          $                              T     \                          $                              U            `             #TIME_TYPE ?   #         @                                  V                    #FILE_INFO W   #NCOPIES X   #CYCLING Y   #SINGLE_FILE Z   #RESTART_FILES [   #ROOT_NAME \   #CHECK_OUTPUT_COMPATIBILITY ]             D                                 W     ų              #FILE_INFO_TYPE              
                                  X                     
                                  Y                     
                                  Z           ,          
@                             [                                  &                   &                                           1           
 @                             \                    1           
 @                               ]           #         @                                   ^                    #FILE_INFO _             
D                                 _     ų              #FILE_INFO_TYPE                      @                               'ų                   #INITIALIZED `   #SINGLEFILE_INITIALIZED a   #CHECK_OUTPUT_COMPATIBILITY b   #CYCLING c   #SINGLE_FILE d   #ROOT_NAME e   #STAGE_METADATA f                $                              `                                          ķ                                                                          $                              a                                         ķ                                                                          $                              b                                         ķ                                                                          $                              c                                         ķ                                                                          $                              d                                         ķ                                                                          $                             e                                                  ķ                                !                       Cnull                                                              $                              f     Ą      8              #STAGE_METADATA_TYPE g                     @              A           g     'Ą                   #INITIALIZED h   #NOUTPUT_ENS i   #NUM_COPIES j   #CLAMP_VARS k   #INHERIT_UNITS l   #FORCE_COPY_BACK m   #IO_FLAG n   #MY_COPY_NUMBER o   #COPY_NAME p   #LONG_NAME q   #FILENAMES r   #FILE_DESCRIPTION s   #NCFILEID t                $                              h                                          ķ                                                                          $                              i                                         ķ                                                      0                 $                              j                                         ķ                                                      0                $                              k                                         &                                                       $                              l            X                             &                                                       $                              m                                          &                                                      $                              n            č                             &                                                       $                              o            0                            &                                           .            $                             p            x             	               &                                                   .            $                             q            Ą             
               &                                                   .           $                             r                                        &                   &                                                   .            $                             s            h                            &                   &                                                                 $                              t     ų       Č             #NETCDF_FILE_TYPE u                     @                         u     'ų                    #NCID v   #NTIMES w   #NTIMESMAX x   #RTIMES y   #TIMES z   #FNAME {   #MODEL_MOD_WILL_WRITE_STATE_VARIABLES |   #DIAG_ID }                 $                              v                                 $                              w                                $                              x                              $                             y                             
            &                                                       $                              z            X                    #TIME_TYPE ?             &                                                         $                             {     P                                   $                              |     š                                    ķ                                                                          $                              }     ō                                    ķ                                         ’’’’’’’’            #         @                                   ~                    #FILE_INFO    #ENS_SIZE    #MY_COPY_START              
D @                                    ų              #FILE_INFO_TYPE              
                                                       
                                             #         @                                                       #FILE_INFO    #ROUTINE_NAME              
                                       ų             #FILE_INFO_TYPE              
  @                                                 1 #         @                                                       #RESTART_NAMES    #ROUTINE_NAME              
                                       Ą             #STAGE_METADATA_TYPE g             
  @                                                 1 #         @                                                       #FILE_INFO    #CONTEXT              
                                       ų             #FILE_INFO_TYPE              
 @                                                 1 &         @                                      ų                     #FILE_INFO_ARRAY    #FILE_INFO_TYPE              
                                             ų                     &                                           #FILE_INFO_TYPE    #         @                                                       #FILE_INFO    #ENS_HANDLE              
                                      ų              #FILE_INFO_TYPE              
                                       h             #ENSEMBLE_TYPE B   $         @                                                           #NAME_HANDLE    #COPY    #DOMAIN                      
                                       Ą             #STAGE_METADATA_TYPE g             
                                                       
                                             %         @                                                            #FILE_INFO              
                                       ų             #FILE_INFO_TYPE    %         @                                                            #FILE_INFO              
                                       ų             #FILE_INFO_TYPE    $         @                                                           #NAME_HANDLE    #COPY    #DOMAIN                      
                                       Ą             #STAGE_METADATA_TYPE g             
                                                       
                                             $         @                                                           #FILE_HANDLE    #CNUM                      
                                       ų             #FILE_INFO_TYPE              
                                             &         @                                      Ą                     #FILE_INFO     #STAGE_METADATA_TYPE g             
                                        ų             #FILE_INFO_TYPE    %         @                                 ”                           #FILE_HANDLE ¢             
                                 ¢     ų              #FILE_INFO_TYPE    %         @                                 £                           #NAME_HANDLE ¤   #COPY „             
                                  ¤     Ą             #STAGE_METADATA_TYPE g             
                                  „           %         @                                 ¦                           #NAME_HANDLE §   #COPY Ø             
                                  §     Ą             #STAGE_METADATA_TYPE g             
                                  Ø           %         @                                 ©                           #NAME_HANDLE Ŗ   #COPY «             
                                  Ŗ     Ą             #STAGE_METADATA_TYPE g             
                                  «           %         @                                 ¬                           #FILE_HANDLE ­             
                                  ­     ų             #FILE_INFO_TYPE    %         @                                 ®                           #NAME_HANDLE Æ   #C °             
  @                               Æ     Ą             #STAGE_METADATA_TYPE g             
  @                               °           %         @                                 ±                           #NAME_HANDLE ²   #C ³             
  @                               ²     Ą             #STAGE_METADATA_TYPE g             
  @                               ³           %         @                                 “                           #COPY µ             
                                  µ                                                        ¶                                                      1                                             ·                                                      2                                             ø                                                      3                                             ¹                                          ’’’’’’’’                                                     ŗ                                          ’’’’’’’’                     fn#fn &      h  b   uapp(IO_FILENAMES_MOD      @   J  NETCDF    Č  g   J  TYPES_MOD    /     J  UTILITIES_MOD !   Ķ  J   J  TIME_MANAGER_MOD "     K   J  MPI_UTILITIES_MOD $   b  e  J  STATE_STRUCTURE_MOD %   Ē  N   J  ENSEMBLE_MANAGER_MOD %     I   J  NETCDF_UTILITIES_MOD %   ^  y       gen@SET_IO_COPY_FLAG '   ×  Ą      SET_IO_COPY_FLAG_RANGE 1     \   a   SET_IO_COPY_FLAG_RANGE%FILE_INFO *   ó  @   a   SET_IO_COPY_FLAG_RANGE%C1 *   3	  @   a   SET_IO_COPY_FLAG_RANGE%C2 /   s	  @   a   SET_IO_COPY_FLAG_RANGE%IO_FLAG 6   ³	  @   a   SET_IO_COPY_FLAG_RANGE%NUM_OUTPUT_ENS 5   ó	  @   a   SET_IO_COPY_FLAG_RANGE%INHERIT_UNITS 2   3
  @   a   SET_IO_COPY_FLAG_RANGE%CLAMP_VARS 7   s
  @   a   SET_IO_COPY_FLAG_RANGE%FORCE_COPY_BACK (   ³
  ¦      SET_IO_COPY_FLAG_SINGLE 2   Y  \   a   SET_IO_COPY_FLAG_SINGLE%FILE_INFO -   µ  @   a   SET_IO_COPY_FLAG_SINGLE%CNUM 0   õ  @   a   SET_IO_COPY_FLAG_SINGLE%IO_FLAG 6   5  @   a   SET_IO_COPY_FLAG_SINGLE%INHERIT_UNITS 3   u  @   a   SET_IO_COPY_FLAG_SINGLE%CLAMP_VARS 8   µ  @   a   SET_IO_COPY_FLAG_SINGLE%FORCE_COPY_BACK &   õ  }       gen@SET_FILE_METADATA +   r        SET_EXPLICIT_FILE_METADATA 5   ÷  \   a   SET_EXPLICIT_FILE_METADATA%FILE_INFO 0   S  @   a   SET_EXPLICIT_FILE_METADATA%CNUM 2        a   SET_EXPLICIT_FILE_METADATA%FNAMES 4   #  L   a   SET_EXPLICIT_FILE_METADATA%BASENAME 0   o  L   a   SET_EXPLICIT_FILE_METADATA%DESC (   »        SET_STAGE_FILE_METADATA 2   K  \   a   SET_STAGE_FILE_METADATA%FILE_INFO -   §  @   a   SET_STAGE_FILE_METADATA%CNUM .   ē  L   a   SET_STAGE_FILE_METADATA%STAGE 1   3  L   a   SET_STAGE_FILE_METADATA%BASENAME -     L   a   SET_STAGE_FILE_METADATA%DESC /   Ė  @   a   SET_STAGE_FILE_METADATA%OFFSET :            gen@GET_MISSING_VALUE+STATE_STRUCTURE_MOD 9     v      GET_MISSING_VALUE_R8+STATE_STRUCTURE_MOD @     @   a   GET_MISSING_VALUE_R8%DOM_ID+STATE_STRUCTURE_MOD @   P  @   a   GET_MISSING_VALUE_R8%VAR_ID+STATE_STRUCTURE_MOD J     @   a   GET_MISSING_VALUE_R8%MISSING_VALUE_R8+STATE_STRUCTURE_MOD 9   Š  v      GET_MISSING_VALUE_R4+STATE_STRUCTURE_MOD @   F  @   a   GET_MISSING_VALUE_R4%DOM_ID+STATE_STRUCTURE_MOD @     @   a   GET_MISSING_VALUE_R4%VAR_ID+STATE_STRUCTURE_MOD J   Ę  @   a   GET_MISSING_VALUE_R4%MISSING_VALUE_R4+STATE_STRUCTURE_MOD :     w      GET_MISSING_VALUE_INT+STATE_STRUCTURE_MOD A   }  @   a   GET_MISSING_VALUE_INT%DOM_ID+STATE_STRUCTURE_MOD A   ½  @   a   GET_MISSING_VALUE_INT%VAR_ID+STATE_STRUCTURE_MOD L   ż  @   a   GET_MISSING_VALUE_INT%MISSING_VALUE_INT+STATE_STRUCTURE_MOD 6   =  w       gen@GET_FILLVALUE+STATE_STRUCTURE_MOD 1   “  n      GET_SPVAL_R8+STATE_STRUCTURE_MOD 8   "  @   a   GET_SPVAL_R8%DOM_ID+STATE_STRUCTURE_MOD 8   b  @   a   GET_SPVAL_R8%VAR_ID+STATE_STRUCTURE_MOD :   ¢  @   a   GET_SPVAL_R8%SPVAL_R8+STATE_STRUCTURE_MOD 1   ā  n      GET_SPVAL_R4+STATE_STRUCTURE_MOD 8   P  @   a   GET_SPVAL_R4%DOM_ID+STATE_STRUCTURE_MOD 8     @   a   GET_SPVAL_R4%VAR_ID+STATE_STRUCTURE_MOD :   Š  @   a   GET_SPVAL_R4%SPVAL_R4+STATE_STRUCTURE_MOD 2     o      GET_SPVAL_INT+STATE_STRUCTURE_MOD 9     @   a   GET_SPVAL_INT%DOM_ID+STATE_STRUCTURE_MOD 9   æ  @   a   GET_SPVAL_INT%VAR_ID+STATE_STRUCTURE_MOD <   ’  @   a   GET_SPVAL_INT%SPVAL_INT+STATE_STRUCTURE_MOD +   ?  g      TIME_TYPE+TIME_MANAGER_MOD ;   ¦  H   %   TIME_TYPE%SECONDS+TIME_MANAGER_MOD=SECONDS 5   ī  H   %   TIME_TYPE%DAYS+TIME_MANAGER_MOD=DAYS 3   6  x     ENSEMBLE_TYPE+ENSEMBLE_MANAGER_MOD <   ®  H   a   ENSEMBLE_TYPE%NUM_VARS+ENSEMBLE_MANAGER_MOD >   ö  H   a   ENSEMBLE_TYPE%NUM_COPIES+ENSEMBLE_MANAGER_MOD A   >  H   a   ENSEMBLE_TYPE%MY_NUM_COPIES+ENSEMBLE_MANAGER_MOD ?     H   a   ENSEMBLE_TYPE%MY_NUM_VARS+ENSEMBLE_MANAGER_MOD =   Ī     a   ENSEMBLE_TYPE%MY_COPIES+ENSEMBLE_MANAGER_MOD ;   b     a   ENSEMBLE_TYPE%MY_VARS+ENSEMBLE_MANAGER_MOD :   ö  ¬   a   ENSEMBLE_TYPE%COPIES+ENSEMBLE_MANAGER_MOD 8   ¢  ¬   a   ENSEMBLE_TYPE%VARS+ENSEMBLE_MANAGER_MOD 8   N   £   a   ENSEMBLE_TYPE%TIME+ENSEMBLE_MANAGER_MOD E   ń   H   a   ENSEMBLE_TYPE%DISTRIBUTION_TYPE+ENSEMBLE_MANAGER_MOD 9   9!  H   a   ENSEMBLE_TYPE%VALID+ENSEMBLE_MANAGER_MOD :   !  H   a   ENSEMBLE_TYPE%ID_NUM+ENSEMBLE_MANAGER_MOD C   É!     a   ENSEMBLE_TYPE%TASK_TO_PE_LIST+ENSEMBLE_MANAGER_MOD C   ]"     a   ENSEMBLE_TYPE%PE_TO_TASK_LIST+ENSEMBLE_MANAGER_MOD 9   ń"  H   a   ENSEMBLE_TYPE%MY_PE+ENSEMBLE_MANAGER_MOD ?   9#  H   a   ENSEMBLE_TYPE%LAYOUT_TYPE+ENSEMBLE_MANAGER_MOD B   #  H   a   ENSEMBLE_TYPE%TRANSPOSE_TYPE+ENSEMBLE_MANAGER_MOD >   É#  H   a   ENSEMBLE_TYPE%NUM_EXTRAS+ENSEMBLE_MANAGER_MOD @   $  _   a   ENSEMBLE_TYPE%CURRENT_TIME+ENSEMBLE_MANAGER_MOD "   p$  Ä       IO_FILENAMES_INIT ,   4%  \   a   IO_FILENAMES_INIT%FILE_INFO *   %  @   a   IO_FILENAMES_INIT%NCOPIES *   Š%  @   a   IO_FILENAMES_INIT%CYCLING .   &  @   a   IO_FILENAMES_INIT%SINGLE_FILE 0   P&  Ø   a   IO_FILENAMES_INIT%RESTART_FILES ,   ų&  L   a   IO_FILENAMES_INIT%ROOT_NAME =   D'  @   a   IO_FILENAMES_INIT%CHECK_OUTPUT_COMPATIBILITY &   '  W       IO_FILENAMES_FINALIZE 0   Ū'  \   a   IO_FILENAMES_FINALIZE%FILE_INFO    7(  Ž       FILE_INFO_TYPE +   )  ¤   a   FILE_INFO_TYPE%INITIALIZED 6   ¹)  ¤   a   FILE_INFO_TYPE%SINGLEFILE_INITIALIZED :   ]*  ¤   a   FILE_INFO_TYPE%CHECK_OUTPUT_COMPATIBILITY '   +  ¤   a   FILE_INFO_TYPE%CYCLING +   „+  ¤   a   FILE_INFO_TYPE%SINGLE_FILE )   I,  Ż   a   FILE_INFO_TYPE%ROOT_NAME .   &-  i   a   FILE_INFO_TYPE%STAGE_METADATA $   -  ,      STAGE_METADATA_TYPE 0   ».  ¤   a   STAGE_METADATA_TYPE%INITIALIZED 0   _/  „   a   STAGE_METADATA_TYPE%NOUTPUT_ENS /   0  „   a   STAGE_METADATA_TYPE%NUM_COPIES /   ©0     a   STAGE_METADATA_TYPE%CLAMP_VARS 2   =1     a   STAGE_METADATA_TYPE%INHERIT_UNITS 4   Ń1     a   STAGE_METADATA_TYPE%FORCE_COPY_BACK ,   e2     a   STAGE_METADATA_TYPE%IO_FLAG 3   ł2     a   STAGE_METADATA_TYPE%MY_COPY_NUMBER .   3     a   STAGE_METADATA_TYPE%COPY_NAME .   )4     a   STAGE_METADATA_TYPE%LONG_NAME .   Å4  “   a   STAGE_METADATA_TYPE%FILENAMES 5   y5  “   a   STAGE_METADATA_TYPE%FILE_DESCRIPTION -   -6  f   a   STAGE_METADATA_TYPE%NCFILEID !   6  Ī       NETCDF_FILE_TYPE &   a7  H   a   NETCDF_FILE_TYPE%NCID (   ©7  H   a   NETCDF_FILE_TYPE%NTIMES +   ń7  H   a   NETCDF_FILE_TYPE%NTIMESMAX (   98     a   NETCDF_FILE_TYPE%RTIMES '   Ķ8  £   a   NETCDF_FILE_TYPE%TIMES '   p9  P   a   NETCDF_FILE_TYPE%FNAME F   Ą9  ¤   a   NETCDF_FILE_TYPE%MODEL_MOD_WILL_WRITE_STATE_VARIABLES )   d:  ¤   a   NETCDF_FILE_TYPE%DIAG_ID )   ;  x       SET_MEMBER_FILE_METADATA 3   ;  \   a   SET_MEMBER_FILE_METADATA%FILE_INFO 2   Ü;  @   a   SET_MEMBER_FILE_METADATA%ENS_SIZE 7   <  @   a   SET_MEMBER_FILE_METADATA%MY_COPY_START -   \<  i       ASSERT_FILE_INFO_INITIALIZED 7   Å<  \   a   ASSERT_FILE_INFO_INITIALIZED%FILE_INFO :   !=  L   a   ASSERT_FILE_INFO_INITIALIZED%ROUTINE_NAME 1   m=  m       ASSERT_RESTART_NAMES_INITIALIZED ?   Ś=  a   a   ASSERT_RESTART_NAMES_INITIALIZED%RESTART_NAMES >   ;>  L   a   ASSERT_RESTART_NAMES_INITIALIZED%ROUTINE_NAME    >  d       FILE_INFO_DUMP )   ė>  \   a   FILE_INFO_DUMP%FILE_INFO '   G?  L   a   FILE_INFO_DUMP%CONTEXT "   ?  y       COMBINE_FILE_INFO 2   @      a   COMBINE_FILE_INFO%FILE_INFO_ARRAY /   ¬@  g       CHECK_FILE_INFO_VARIABLE_SHAPE 9   A  \   a   CHECK_FILE_INFO_VARIABLE_SHAPE%FILE_INFO :   oA  [   a   CHECK_FILE_INFO_VARIABLE_SHAPE%ENS_HANDLE %   ŹA         GET_RESTART_FILENAME 1   IB  a   a   GET_RESTART_FILENAME%NAME_HANDLE *   ŖB  @   a   GET_RESTART_FILENAME%COPY ,   źB  @   a   GET_RESTART_FILENAME%DOMAIN     *C  _       GET_SINGLE_FILE *   C  \   a   GET_SINGLE_FILE%FILE_INFO    åC  _       GET_CYCLING &   DD  \   a   GET_CYCLING%FILE_INFO %    D         GET_FILE_DESCRIPTION 1   E  a   a   GET_FILE_DESCRIPTION%NAME_HANDLE *   E  @   a   GET_FILE_DESCRIPTION%COPY ,   ĄE  @   a   GET_FILE_DESCRIPTION%DOMAIN     F  s       GET_COPY_NAME *   sF  \   a   GET_COPY_NAME%FILE_HANDLE #   ĻF  @   a   GET_COPY_NAME%CNUM #   G  x       GET_STAGE_METADATA -   G  \   a   GET_STAGE_METADATA%FILE_INFO (   ćG  a       SINGLE_FILE_INITIALIZED 4   DH  \   a   SINGLE_FILE_INITIALIZED%FILE_HANDLE #    H  k       INHERIT_COPY_UNITS /   I  a   a   INHERIT_COPY_UNITS%NAME_HANDLE (   lI  @   a   INHERIT_COPY_UNITS%COPY     ¬I  k       COPY_IS_CLAMPED ,   J  a   a   COPY_IS_CLAMPED%NAME_HANDLE %   xJ  @   a   COPY_IS_CLAMPED%COPY     øJ  k       FORCE_COPY_BACK ,   #K  a   a   FORCE_COPY_BACK%NAME_HANDLE %   K  @   a   FORCE_COPY_BACK%COPY (   ÄK  a       NOUTPUT_STATE_VARIABLES 4   %L  \   a   NOUTPUT_STATE_VARIABLES%FILE_HANDLE     L  h       QUERY_READ_COPY ,   éL  a   a   QUERY_READ_COPY%NAME_HANDLE "   JM  @   a   QUERY_READ_COPY%C !   M  h       QUERY_WRITE_COPY -   ņM  a   a   QUERY_WRITE_COPY%NAME_HANDLE #   SN  @   a   QUERY_WRITE_COPY%C #   N  Z       QUERY_COPY_PRESENT (   ķN  @   a   QUERY_COPY_PRESENT%COPY    -O  q       READ_COPY    O  q       WRITE_COPY     P  q       READ_WRITE_COPY    P  p       NO_IO !   šP  p       COPY_NOT_PRESENT 