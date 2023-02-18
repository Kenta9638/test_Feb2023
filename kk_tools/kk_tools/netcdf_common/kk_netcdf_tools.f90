MODULE kk_netcdf_tools

USE kk_netcdf_common
IMPLICIT NONE

CONTAINS

!==============================================================
!
! READ 
!
!==============================================================
!----------------------------------
!  GET DIM LENGTH
!----------------------------------
SUBROUTINE kk_netcdf_get_dimlen(fname,var_name,out_dimlen)

  IMPLICIT NONE
!  INCLUDE 'netcdf.inc'
  CHARACTER(*),INTENT(IN) :: fname,var_name
  INTEGER,INTENT(OUT)     :: out_dimlen
  INTEGER :: ierr,ncid,varid,ndims
  ierr = nf_open (trim(fname), nf_nowrite, ncid)
  ierr = nf_inq_dimid (ncid,trim(var_name), varid)
  ierr = nf_inq_dimlen(ncid, varid, out_dimlen)
  ierr = nf_close(ncid)
  RETURN
END SUBROUTINE kk_netcdf_get_dimlen
!----------------------------------
!  GET VARIABLE
!----------------------------------
SUBROUTINE kk_netcdf_get_var_single(fname,var_name,var_dim,          &
                                    dimlen1,dimlen2,dimlen3,dimlen4, &
                                    outvar1,outvar2,outvar3,outvar4) 
  IMPLICIT NONE
!  INCLUDE 'netcdf.inc'
  CHARACTER(*),INTENT(IN)  :: fname,var_name
  INTEGER     ,INTENT(IN)  :: var_dim
  INTEGER     ,INTENT(IN)  :: dimlen1,dimlen2,dimlen3,dimlen4
  REAL(4)     ,INTENT(OUT) :: outvar1(dimlen1)
  REAL(4)     ,INTENT(OUT) :: outvar2(dimlen1,dimlen2)
  REAL(4)     ,INTENT(OUT) :: outvar3(dimlen1,dimlen2,dimlen3)
  REAL(4)     ,INTENT(OUT) :: outvar4(dimlen1,dimlen2,dimlen3,dimlen4)
  INTEGER :: ierr,ncid,varid,ndims
  INTEGER :: i,out_ierr
  ierr = nf_open (trim(fname), nf_nowrite, ncid) ; CALL check_error(ierr,'read',out_ierr)
  ierr = nf_inq_varid (ncid,trim(var_name), varid)
  SELECT CASE(var_dim)
    CASE (1)
      ierr = nf_get_var_real(ncid, varid, outvar1)
    CASE (2)
      ierr = nf_get_var_real(ncid, varid, outvar2)
    CASE (3)
      ierr = nf_get_var_real(ncid, varid, outvar3)
    CASE (4)
      ierr = nf_get_var_real(ncid, varid, outvar4)
  ENDSELECT
  ierr = nf_close(ncid)
  RETURN
END SUBROUTINE kk_netcdf_get_var_single

SUBROUTINE kk_netcdf_get_var_double(fname,var_name,var_dim,          &
                                    dimlen1,dimlen2,dimlen3,dimlen4, &
                                    outvar1,outvar2,outvar3,outvar4) 
  IMPLICIT NONE
!  INCLUDE 'netcdf.inc'
  CHARACTER(*),INTENT(IN)  :: fname,var_name
  INTEGER     ,INTENT(IN)  :: var_dim
  INTEGER     ,INTENT(IN)  :: dimlen1,dimlen2,dimlen3,dimlen4
  REAL(8)     ,INTENT(OUT) :: outvar1(dimlen1)
  REAL(8)     ,INTENT(OUT) :: outvar2(dimlen1,dimlen2)
  REAL(8)     ,INTENT(OUT) :: outvar3(dimlen1,dimlen2,dimlen3)
  REAL(8)     ,INTENT(OUT) :: outvar4(dimlen1,dimlen2,dimlen3,dimlen4)
  INTEGER :: ierr,ncid,varid,ndims
  INTEGER :: i,out_ierr
  ierr = nf_open (trim(fname), nf_nowrite, ncid) ; CALL check_error(ierr,'read',out_ierr)
  ierr = nf_inq_varid (ncid,trim(var_name), varid)
  SELECT CASE(var_dim)
    CASE (1)
      ierr = nf_get_var_double(ncid, varid, outvar1)
    CASE (2)
      ierr = nf_get_var_double(ncid, varid, outvar2)
    CASE (3)
      ierr = nf_get_var_double(ncid, varid, outvar3)
    CASE (4)
      ierr = nf_get_var_double(ncid, varid, outvar4)
  ENDSELECT
  ierr = nf_close(ncid)
  RETURN
END SUBROUTINE kk_netcdf_get_var_double


!==============================================================
!
! CREATE 
!
!==============================================================
!----------------------------------
!  DEFINE DIMS
!----------------------------------
SUBROUTINE kk_netcdf_define_dims(fname,dim_num,  &
                                 var_name1,dim1, &
                                 var_name2,dim2, &
                                 var_name3,dim3, &
                                 var_name4,dim4, &
                                 var_name5,dim5, &
                                 var_name6,dim6, &
                                 var_name7,dim7, &
                                 var_name8,dim8 )
  IMPLICIT NONE
!  INCLUDE 'netcdf.inc'
  CHARACTER(256) :: tmp_var_name
  INTEGER      :: tmp_dim
  CHARACTER(*),INTENT(IN) :: fname,var_name1
  CHARACTER(*),INTENT(IN),OPTIONAL :: var_name2,var_name3,var_name4,var_name5,var_name6,var_name7,var_name8
  INTEGER :: ierr,ncid,varid,ndims
  INTEGER :: i,ii,out_ierr
  INTEGER :: OUTVAR_DIM(1),start(1),count(1)
  INTEGER,INTENT(IN) :: dim_num,dim1
  INTEGER,INTENT(IN),OPTIONAL :: dim2,dim3,dim4,dim5,dim6,dim7,dim8

  ierr = nf_create (trim(fname), nf_clobber, ncid)                   
  do ii = 1,dim_num
     IF (ii==1) THEN; tmp_var_name = var_name1; tmp_dim = dim1; ENDIF
     IF (ii==2) THEN; tmp_var_name = var_name2; tmp_dim = dim2; ENDIF
     IF (ii==3) THEN; tmp_var_name = var_name3; tmp_dim = dim3; ENDIF
     IF (ii==4) THEN; tmp_var_name = var_name4; tmp_dim = dim4; ENDIF
     IF (ii==5) THEN; tmp_var_name = var_name5; tmp_dim = dim5; ENDIF
     IF (ii==6) THEN; tmp_var_name = var_name6; tmp_dim = dim6; ENDIF
     IF (ii==7) THEN; tmp_var_name = var_name7; tmp_dim = dim7; ENDIF
     IF (ii==8) THEN; tmp_var_name = var_name8; tmp_dim = dim8; ENDIF
     start = 1; count(1) = tmp_dim
     ierr = nf_def_dim(ncid, trim(tmp_var_name), tmp_dim, varid)
     OUTVAR_DIM(1) = varid
 !    write(*,*) '--------------------------------------------------------------'
 !    write(*,*) 'COMPLETED [DEFINE] : ', trim(tmp_var_name), ' ===> ', trim(fname)
 !    write(*,*) '--------------------------------------------------------------'
   enddo
   ierr = nf_enddef (ncid)
   ierr = nf_close(ncid)
   RETURN
END SUBROUTINE kk_netcdf_define_dims

!----------------------------------
!  PUT
!----------------------------------
SUBROUTINE kk_netcdf_put_var_single(fname,var_dim,out_var_name,            &
                                    title_name,                            &
                                    out_var_name_dim1,out_var_name_dim2,   &
                                    out_var_name_dim3,out_var_name_dim4,   &
                                    dim1,dim2,dim3,dim4,                   &
                                    out_var1,out_var2,out_var3,out_var4)
  IMPLICIT NONE
!  INCLUDE 'netcdf.inc'
  CHARACTER(*),INTENT(IN) :: fname,out_var_name,title_name
  CHARACTER(*),INTENT(IN) :: out_var_name_dim1,out_var_name_dim2,out_var_name_dim3,out_var_name_dim4
  INTEGER,     INTENT(IN) :: var_dim
  INTEGER,     INTENT(IN) :: dim1,dim2,dim3,dim4
  REAL(4),     INTENT(IN) :: out_var1(dim1)
  REAL(4),     INTENT(IN) :: out_var2(dim1,dim2)
  REAL(4),     INTENT(IN) :: out_var3(dim1,dim2,dim3)
  REAL(4),     INTENT(IN) :: out_var4(dim1,dim2,dim3,dim4)
  INTEGER :: ierr,ncid,varid,ndims
  INTEGER :: i,out_ierr
  INTEGER :: varid_dim1,varid_dim2,varid_dim3,varid_dim4,name_len
  INTEGER :: OUTVAR_DIM(var_dim),start(var_dim),count(var_dim)

  out_ierr = 0
  start = 1
  SELECT CASE(var_dim)
    CASE (1); count(1) = dim1
    CASE (2); count(1) = dim1; count(2) = dim2
    CASE (3); count(1) = dim1; count(2) = dim2; count(3) = dim3
    CASE (4); count(1) = dim1; count(2) = dim2; count(3) = dim3; count(4) = dim4
  ENDSELECT
  ierr = nf_open (trim(fname), nf_write, ncid)                   ; CALL check_error(ierr,'add',out_ierr)
  ierr = nf_redef(ncid)                                          ; CALL check_error(ierr,'add',out_ierr)
  SELECT CASE(var_dim)
    CASE (1)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
    CASE (2)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim2), varid_dim2) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
      OUTVAR_DIM(2) = varid_dim2
    CASE (3)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim2), varid_dim2) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim3), varid_dim3) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
      OUTVAR_DIM(2) = varid_dim2
      OUTVAR_DIM(3) = varid_dim3
    CASE (4)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim2), varid_dim2) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim3), varid_dim3) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim4), varid_dim4) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
      OUTVAR_DIM(2) = varid_dim2
      OUTVAR_DIM(3) = varid_dim3
      OUTVAR_DIM(4) = varid_dim4
  ENDSELECT
  ierr = nf_def_var(ncid, trim(out_var_name), nf_real, var_dim, OUTVAR_DIM, varid) ; CALL check_error(ierr,'add',out_ierr)
  name_len = len_trim(title_name)
  ierr = nf_put_att_text(ncid,varid, 'title', name_len, trim(title_name))    ; CALL check_error(ierr,'add',out_ierr)
  ierr = nf_enddef (ncid)                                                    ; CALL check_error(ierr,'add',out_ierr)
  SELECT CASE(var_dim)
    CASE (1); ierr = nf_put_vara_real(ncid, varid, start,count, out_var1)  ; CALL check_error(ierr,'add',out_ierr)
    CASE (2); ierr = nf_put_vara_real(ncid, varid, start,count, out_var2)  ; CALL check_error(ierr,'add',out_ierr)
    CASE (3); ierr = nf_put_vara_real(ncid, varid, start,count, out_var3)  ; CALL check_error(ierr,'add',out_ierr)
    CASE (4); ierr = nf_put_vara_real(ncid, varid, start,count, out_var4)  ; CALL check_error(ierr,'add',out_ierr)
  ENDSELECT
  ierr = nf_close(ncid)                                                      ; CALL check_error(ierr,'add',out_ierr)
  write(*,*) '--------------------------------------------------------------'
  write(*,*) 'COMPLETED [ADD] : ', trim(out_var_name), ' ===> ', trim(fname)
  write(*,*) '--------------------------------------------------------------'
  RETURN
END SUBROUTINE kk_netcdf_put_var_single

SUBROUTINE kk_netcdf_put_var_double(fname,var_dim,out_var_name,            &
                                    title_name,                            &
                                    out_var_name_dim1,out_var_name_dim2,   &
                                    out_var_name_dim3,out_var_name_dim4,   &
                                    dim1,dim2,dim3,dim4,                   &
                                    out_var1,out_var2,out_var3,out_var4)
  IMPLICIT NONE
!  INCLUDE 'netcdf.inc'
  CHARACTER(*),INTENT(IN) :: fname,out_var_name,title_name
  CHARACTER(*),INTENT(IN) :: out_var_name_dim1,out_var_name_dim2,out_var_name_dim3,out_var_name_dim4
  INTEGER,     INTENT(IN) :: var_dim
  INTEGER,     INTENT(IN) :: dim1,dim2,dim3,dim4
  REAL(8),     INTENT(IN) :: out_var1(dim1)
  REAL(8),     INTENT(IN) :: out_var2(dim1,dim2)
  REAL(8),     INTENT(IN) :: out_var3(dim1,dim2,dim3)
  REAL(8),     INTENT(IN) :: out_var4(dim1,dim2,dim3,dim4)
  INTEGER :: ierr,ncid,varid,ndims
  INTEGER :: i,out_ierr
  INTEGER :: varid_dim1,varid_dim2,varid_dim3,varid_dim4,name_len
  INTEGER :: OUTVAR_DIM(var_dim),start(var_dim),count(var_dim)

  out_ierr = 0
  start = 1
  SELECT CASE(var_dim)
    CASE (1); count(1) = dim1
    CASE (2); count(1) = dim1; count(2) = dim2
    CASE (3); count(1) = dim1; count(2) = dim2; count(3) = dim3
    CASE (4); count(1) = dim1; count(2) = dim2; count(3) = dim3; count(4) = dim4
  ENDSELECT
  ierr = nf_open (trim(fname), nf_write, ncid)                   ; CALL check_error(ierr,'add',out_ierr)
  ierr = nf_redef(ncid)                                          ; CALL check_error(ierr,'add',out_ierr)
  SELECT CASE(var_dim)
    CASE (1)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
    CASE (2)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim2), varid_dim2) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
      OUTVAR_DIM(2) = varid_dim2
    CASE (3)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim2), varid_dim2) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim3), varid_dim3) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
      OUTVAR_DIM(2) = varid_dim2
      OUTVAR_DIM(3) = varid_dim3
    CASE (4)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim1), varid_dim1) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim2), varid_dim2) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim3), varid_dim3) ; CALL check_error(ierr,'add',out_ierr)
      ierr = nf_inq_dimid(ncid, trim(out_var_name_dim4), varid_dim4) ; CALL check_error(ierr,'add',out_ierr)
      OUTVAR_DIM(1) = varid_dim1
      OUTVAR_DIM(2) = varid_dim2
      OUTVAR_DIM(3) = varid_dim3
      OUTVAR_DIM(4) = varid_dim4
  ENDSELECT
  ierr = nf_def_var(ncid, trim(out_var_name), nf_double, var_dim, OUTVAR_DIM, varid) ; CALL check_error(ierr,'add',out_ierr)
  name_len = len_trim(title_name)
  ierr = nf_put_att_text(ncid,varid, 'title', name_len, trim(title_name))    ; CALL check_error(ierr,'add',out_ierr)
  ierr = nf_enddef (ncid)                                                    ; CALL check_error(ierr,'add',out_ierr)
  SELECT CASE(var_dim)
    CASE (1); ierr = nf_put_vara_double(ncid, varid, start,count, out_var1)  ; CALL check_error(ierr,'add',out_ierr)
    CASE (2); ierr = nf_put_vara_double(ncid, varid, start,count, out_var2)  ; CALL check_error(ierr,'add',out_ierr)
    CASE (3); ierr = nf_put_vara_double(ncid, varid, start,count, out_var3)  ; CALL check_error(ierr,'add',out_ierr)
    CASE (4); ierr = nf_put_vara_double(ncid, varid, start,count, out_var4)  ; CALL check_error(ierr,'add',out_ierr)
  ENDSELECT
  ierr = nf_close(ncid)                                                      ; CALL check_error(ierr,'add',out_ierr)
  write(*,*) '--------------------------------------------------------------'
  write(*,*) 'COMPLETED [ADD] : ', trim(out_var_name), ' ===> ', trim(fname)
  write(*,*) '--------------------------------------------------------------'
  RETURN
END SUBROUTINE kk_netcdf_put_var_double











!----------------------------------
!  ADD Lat&Lon
!----------------------------------
SUBROUTINE kk_netcdf_add_LatLon(fname,latmin,latmax,latdim,lonmin,lonmax,londim)

  IMPLICIT NONE
!  INCLUDE 'netcdf.inc'
  CHARACTER(*),INTENT(IN) :: fname
  INTEGER,INTENT(IN) :: latdim,londim
  REAL(4),INTENT(IN) :: latmin,latmax,lonmin,lonmax
  INTEGER :: ierr,ncid,dimid
  REAL(4),ALLOCATABLE :: Lon(:),Lat(:)
  INTEGER :: i
  REAL(4) :: diff
  REAL(4), DIMENSION(1)       :: dummy_dim1 = real(-9.999d33)
  REAL(4), DIMENSION(1,1)     :: dummy_dim2 = real(-9.999d33)
  REAL(4), DIMENSION(1,1,1)   :: dummy_dim3 = real(-9.999d33)
  REAL(4), DIMENSION(1,1,1,1) :: dummy_dim4 = real(-9.999d33)

  ALLOCATE(Lon(londim))
  diff = abs(lonmax-lonmin)/real(londim)
  DO i = 1,londim
    Lon(i) = lonmin + real((i-1))*diff
  ENDDO
  ALLOCATE(Lat(latdim))
  diff = abs(latmax-latmin)/real(latdim)
  DO i = 1,latdim
    Lat(i) = latmin + real((i-1))*diff
  ENDDO
  CALL kk_netcdf_put_var_single (trim(fname),1,'Lon','Longitude','Lon','UNDEF','UNDEF','UNDEF', &
                                 londim,9999,9999,9999,Lon,dummy_dim2,dummy_dim3,dummy_dim4)
  CALL kk_netcdf_put_var_single (trim(fname),1,'Lat','Latitude' ,'Lat','UNDEF','UNDEF','UNDEF', &
                                 latdim,9999,9999,9999,Lat,dummy_dim2,dummy_dim3,dummy_dim4)
END SUBROUTINE kk_netcdf_add_LatLon

!----------------------------------
!  Error Trap
!----------------------------------
SUBROUTINE check_error(ierr,mode,out_ierr)
  INTEGER,INTENT(IN) :: ierr
  CHARACTER(*),INTENT(IN) :: mode
  INTEGER,INTENT(INOUT) :: out_ierr

     out_ierr = out_ierr + 1
     IF (ierr.ne.nf_noerr) then
       if (trim(mode) == 'add' ) then
         write(*,*) 'ERROR add_variable_NetCDF file at ', out_ierr, ' !!! '
!         print *; read *
       elseif (trim(mode) == 'create' ) then
         write(*,*) 'ERROR create_NetCDF file at ', out_ierr, ' !!! '
 !        print *; read *
       elseif (trim(mode) == 'read' ) then
         write(*,*) 'ERROR read_NetCDF file at ', out_ierr, ' !!! '
  !       print *; read *
       elseif (trim(mode) == 'getdim' ) then
         write(*,*) 'ERROR get_dimension_NetCDF file at ', out_ierr, ' !!! '
   !      print *; read *
       elseif (trim(mode) == 'define' ) then
         write(*,*) 'ERROR define_dimension_NetCDF file at ', out_ierr, ' !!!'
    !     print *; read *
       elseif (trim(mode) == 'put' ) then
         write(*,*) 'ERROR put_variable_NetCDF file at ', out_ierr, ' !!! '
     !    print *; read *
       endif
     endif
END SUBROUTINE check_error
  

END MODULE kk_netcdf_tools
