  Ź  3   k820309    ,          2021.5.0    {Ťúc                                                                                                          
       /glade/u/home/kkurosaw/WORK/WRF_DART_Feb2023/DART_kk/assimilation_code/modules/utilities/distributed_state_mod.f90 DISTRIBUTED_STATE_MOD              GET_STATE_ARRAY GET_STATE CREATE_STATE_WINDOW FREE_STATE_WINDOW CREATE_MEAN_WINDOW FREE_MEAN_WINDOW                      @                              
       MY_TASK_ID GET_FROM_FWD GET_FROM_MEAN                      @                              
       R8 I8                      @                              
       ENSEMBLE_TYPE MAP_PE_TO_TASK GET_VAR_OWNER_INDEX GET_ALLOW_TRANSPOSE                      @                              
       CREATE_MEAN_WINDOW CREATE_STATE_WINDOW FREE_MEAN_WINDOW FREE_STATE_WINDOW MEAN_ENS_HANDLE DATA_COUNT NO_WINDOW MEAN_WINDOW STATE_WINDOW MEAN_WIN STATE_WIN CURRENT_WIN                      @                              
       ERROR_HANDLER E_ERR                   @               @                'h                   #NUM_VARS    #NUM_COPIES    #MY_NUM_COPIES 	   #MY_NUM_VARS 
   #MY_COPIES    #MY_VARS    #COPIES    #VARS    #TIME    #DISTRIBUTION_TYPE    #VALID    #ID_NUM    #TASK_TO_PE_LIST    #PE_TO_TASK_LIST    #MY_PE    #LAYOUT_TYPE    #TRANSPOSE_TYPE    #NUM_EXTRAS    #CURRENT_TIME                  $                                                              $                                                              $                              	                                $                              
                              $                                                                       &                                                       $                                         `                             &                                                       $                                         ¨                 	            &                   &                                                       $                                                         	            &                   &                                                       $                                          h             	      #TIME_TYPE              &                                                         Ŕ  @                              '                    #SECONDS    #DAYS                  D                                                              D                                                             $                                   °      
                    $                                   ´                          $                                   ¸                        $                                          Ŕ                            &                                                      $                                                                      &                                                         $                                   P                          $                                   T                          $                                   X                          $                                   \                          $                                          `             #TIME_TYPE    #        @                                                       #STATE_ENS_HANDLE    #MEAN_COPY    #DISTRIBUTE_MEAN               
                                       h             #ENSEMBLE_TYPE              
                                                       
                                              #        @                                   !                    #STATE_ENS_HANDLE "   #FWD_OP_ENS_HANDLE #   #QC_ENS_HANDLE $             
                                 "     h              #ENSEMBLE_TYPE              
                                 #     h              #ENSEMBLE_TYPE              
                                 $     h              #ENSEMBLE_TYPE    #        @                                   %                     #        @                                   &                    #STATE_ENS_HANDLE '   #FWD_OP_ENS_HANDLE (   #QC_ENS_HANDLE )             
                                 '     h              #ENSEMBLE_TYPE              
                                 (     h              #ENSEMBLE_TYPE              
                                 )     h              #ENSEMBLE_TYPE    #         @                                   *                    #X +   #MY_INDEX -   #STATE_ENS_HANDLE .            D                                +                    	     p          5 r ,       5 r ,                              
  @                              -                        p          5 r ,       5 r ,                               
  @                               .     h             #ENSEMBLE_TYPE    (        `                                /                                    	    #MY_INDEX 0   #ENS_HANDLE 1   p          5 r ,       5 r ,                               
  @                              0                     
  @                               1     h             #ENSEMBLE_TYPE                 @@                              ,                         fn#fn +   1  t   b   uapp(DISTRIBUTED_STATE_MOD "   Ľ  f   J  MPI_UTILITIES_MOD      F   J  TYPES_MOD %   Q     J  ENSEMBLE_MANAGER_MOD    Ö  ç   j  WINDOW_MOD    ˝  T   J  UTILITIES_MOD 3     x      ENSEMBLE_TYPE+ENSEMBLE_MANAGER_MOD <     H   a   ENSEMBLE_TYPE%NUM_VARS+ENSEMBLE_MANAGER_MOD >   Ń  H   a   ENSEMBLE_TYPE%NUM_COPIES+ENSEMBLE_MANAGER_MOD A     H   a   ENSEMBLE_TYPE%MY_NUM_COPIES+ENSEMBLE_MANAGER_MOD ?   a  H   a   ENSEMBLE_TYPE%MY_NUM_VARS+ENSEMBLE_MANAGER_MOD =   Š     a   ENSEMBLE_TYPE%MY_COPIES+ENSEMBLE_MANAGER_MOD ;   =     a   ENSEMBLE_TYPE%MY_VARS+ENSEMBLE_MANAGER_MOD :   Ń  Ź   a   ENSEMBLE_TYPE%COPIES+ENSEMBLE_MANAGER_MOD 8   }  Ź   a   ENSEMBLE_TYPE%VARS+ENSEMBLE_MANAGER_MOD 8   )	  Ł   a   ENSEMBLE_TYPE%TIME+ENSEMBLE_MANAGER_MOD +   Ě	  g      TIME_TYPE+TIME_MANAGER_MOD ;   3
  H   %   TIME_TYPE%SECONDS+TIME_MANAGER_MOD=SECONDS 5   {
  H   %   TIME_TYPE%DAYS+TIME_MANAGER_MOD=DAYS E   Ă
  H   a   ENSEMBLE_TYPE%DISTRIBUTION_TYPE+ENSEMBLE_MANAGER_MOD 9     H   a   ENSEMBLE_TYPE%VALID+ENSEMBLE_MANAGER_MOD :   S  H   a   ENSEMBLE_TYPE%ID_NUM+ENSEMBLE_MANAGER_MOD C        a   ENSEMBLE_TYPE%TASK_TO_PE_LIST+ENSEMBLE_MANAGER_MOD C   /     a   ENSEMBLE_TYPE%PE_TO_TASK_LIST+ENSEMBLE_MANAGER_MOD 9   Ă  H   a   ENSEMBLE_TYPE%MY_PE+ENSEMBLE_MANAGER_MOD ?     H   a   ENSEMBLE_TYPE%LAYOUT_TYPE+ENSEMBLE_MANAGER_MOD B   S  H   a   ENSEMBLE_TYPE%TRANSPOSE_TYPE+ENSEMBLE_MANAGER_MOD >     H   a   ENSEMBLE_TYPE%NUM_EXTRAS+ENSEMBLE_MANAGER_MOD @   ă  _   a   ENSEMBLE_TYPE%CURRENT_TIME+ENSEMBLE_MANAGER_MOD .   B         CREATE_MEAN_WINDOW+WINDOW_MOD ?   Ä  [   a   CREATE_MEAN_WINDOW%STATE_ENS_HANDLE+WINDOW_MOD 8     @   a   CREATE_MEAN_WINDOW%MEAN_COPY+WINDOW_MOD >   _  @   a   CREATE_MEAN_WINDOW%DISTRIBUTE_MEAN+WINDOW_MOD /            CREATE_STATE_WINDOW+WINDOW_MOD @   '  [   a   CREATE_STATE_WINDOW%STATE_ENS_HANDLE+WINDOW_MOD A     [   a   CREATE_STATE_WINDOW%FWD_OP_ENS_HANDLE+WINDOW_MOD =   Ý  [   a   CREATE_STATE_WINDOW%QC_ENS_HANDLE+WINDOW_MOD ,   8  H       FREE_MEAN_WINDOW+WINDOW_MOD -            FREE_STATE_WINDOW+WINDOW_MOD >     [   a   FREE_STATE_WINDOW%STATE_ENS_HANDLE+WINDOW_MOD ?   c  [   a   FREE_STATE_WINDOW%FWD_OP_ENS_HANDLE+WINDOW_MOD ;   ž  [   a   FREE_STATE_WINDOW%QC_ENS_HANDLE+WINDOW_MOD       s       GET_STATE_ARRAY "        a   GET_STATE_ARRAY%X )         a   GET_STATE_ARRAY%MY_INDEX 1   ´  [   a   GET_STATE_ARRAY%STATE_ENS_HANDLE      Â       GET_STATE #   Ń  @   a   GET_STATE%MY_INDEX %     [   a   GET_STATE%ENS_HANDLE &   l  @      DATA_COUNT+WINDOW_MOD 