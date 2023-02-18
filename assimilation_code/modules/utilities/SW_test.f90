MODULE SW_test
!=======================================================================
!
! [PURPOSE:] Shapiro-Wilk test & Royston's test
!
! [CREATED:] 09/28/2021 Kenta Kurosawa
!
!=======================================================================
  USE common
  IMPLICIT NONE
  PUBLIC 

CONTAINS 

!-----------------------------------------------------------------------
! Royston's Multivariate Normality Test.
! Kenta Kurosawa, June 21, 2021
! ORIGINAL: Roystest.m
!           https://jp.mathworks.com/matlabcentral/fileexchange/17811-roystest
!-----------------------------------------------------------------------
! ... The Shapiro-Wilk test (Shapiro and Wilk, 1965), is generally considered
! to be an excellent test of univariate normality. It is only natural to
! extend it to the multivariate case, as done by Royston (1982). ...
!-----------------------------------------------------------------------
SUBROUTINE SW_ROYSTON_TEST(IN_X,SAMPLE_SIZE,dim1,alpha,OUT_RESULT,OUT_W,OUT_KURT,OUT_Z)
  IMPLICIT NONE
  !-- in & out
  INTEGER     ,INTENT(IN)   :: SAMPLE_SIZE
  INTEGER     ,INTENT(IN)   :: dim1  ! < --
  REAL(r_size),INTENT(IN)   :: IN_X(dim1,SAMPLE_SIZE)
  REAL(r_size),INTENT(IN)   :: alpha != 0.05d0     ! significance level (default = 0.05)
  REAL(r_size),INTENT(OUT)  :: OUT_RESULT      ! 1 or 0,  1:not rejected (gaussian), 0:rejected (non-gaussian)
  REAL(r_size),INTENT(OUT),DIMENSION(dim1),OPTIONAL :: OUT_W,OUT_KURT,OUT_Z
  !-- Shapiro-Wilk test
  REAL(r_size) :: x, g, m, s
  REAL(r_size) :: u, v, l
  REAL(r_size) :: T, mC, e, H, P
  REAL(r_size),DIMENSION(dim1) :: tmp_W, tmp_Z, tmp_R, tmp_kurt
  REAL(r_size),DIMENSION(dim1,dim1) :: C  ! correlation matrix
  REAL(r_size),DIMENSION(dim1,dim1) :: NC ! transformed correlation matrix
  INTEGER,DIMENSION(dim1) :: flag1
  REAL(r_size) :: tmp_mean
  !-- cdf
  REAL(r_size) :: r8_normal_01_cdf_inverse, alnorm
  LOGICAL  :: upper
  REAL(r_size) :: tmp_cdf,bound
  INTEGER(kind=4) :: status
  !--
  INTEGER :: i,j,k
  REAL(r_size)                        :: work01, work02, work03, work04, work05
  REAL(r_size)                        :: work91, work92, work93, work94, work95
  REAL(r_size),DIMENSION(SAMPLE_SIZE) :: work11

  !--- QC (KKUROSAWA)
  flag1 = 0
  DO i = 1,dim1
    CALL com_mean(SAMPLE_SIZE,IN_X(i,:),tmp_mean)
    IF (abs(tmp_mean-minval(IN_X(i,:))) < 0.001d0 .AND. abs(tmp_mean-maxval(IN_X(i,:))) < 0.001d0) THEN
      flag1(i) = 1
    ENDIF
  ENDDO

  IF (SAMPLE_SIZE<3) THEN
    WRITE(*,*) 'sample size is too small in SW_ROYSTON_TEST.'
    write(*,*) SAMPLE_SIZE
    PRINT *
    READ *
  ELSEIF (SAMPLE_SIZE >= 4 .AND. SAMPLE_SIZE <= 11) THEN
    x = dble(SAMPLE_SIZE)
    g = -2.273d0 + 0.459d0*x
    m = 0.5440d0 - 0.39978d0*x + 0.025054d0*x**2 - 0.0006714d0*x**3
    s = exp(1.3822d0 - 0.77857d0*x + 0.062767d0*x**2 - 0.0020322d0*x**3)
    DO j = 1,dim1
      CALL ShaWilstat(IN_X(j,:),tmp_W(j),SAMPLE_SIZE,flag1(j),tmp_kurt(j))
      tmp_Z(j) = (-log(g-(log(1-tmp_W(j))))-m)/s
    ENDDO
  ELSEIF (SAMPLE_SIZE >= 12 .AND. SAMPLE_SIZE <= 2000) THEN
    x = log(dble(SAMPLE_SIZE))
    g = 0.0d0
    m = -1.5861d0 - 0.31082d0*x - 0.083751d0*x**2 + 0.0038915d0*x**3
    s = exp(-0.4803d0 -0.082676d0*x + 0.0030302d0*x**2)
    DO j = 1,dim1
      CALL ShaWilstat(IN_X(j,:),tmp_W(j),SAMPLE_SIZE,flag1(j),tmp_kurt(j))
      tmp_Z(j) = ((log(1-tmp_W(j)))+g-m)/s
 !     IF (tmp_W(j)>0.95d0 .AND. tmp_Z(j)>2.0d0) tmp_Z(j) = 0.0d0 ! KKUROSAWA
    ENDDO
 !!!   write(*,*) 'tmp_Z',tmp_Z
 !!!   write(*,*) 'g',g
 !!!   write(*,*) 'm',m
 !!!   write(*,*) 's',s
 !!!   write(*,*) 'x',x
 !!!   write(*,*) 'SAMPLE_SIZE',SAMPLE_SIZE
  ELSE
    WRITE(*,*) 'sample size is too large in SW_ROYSTON_TEST.'
    PRINT *
    READ *
  ENDIF

  DO j = 1,dim1
    CALL cal_alnorm(-1.0d0*dble(tmp_Z(j)),.TRUE.,work01)
    work91 = work01
    work01 = work01/2.0d0
    work02 = r8_normal_01_cdf_inverse(work01)
    work92 = work02
    work02 = work02**2
    tmp_R(j) = work02
  ENDDO

  u = 0.715d0
  v = 0.21364d0 + 0.015124d0*(log(dble(SAMPLE_SIZE)))**2 - 0.0018034d0*(log(dble(SAMPLE_SIZE)))**3 ! Royston 1983
  l = 5.0d0

  DO i = 1,dim1
    C(i,i) = 1
  ENDDO

  IF     (dim1==1) THEN
  ELSEIF (dim1==2) THEN
    CALL com_correl(SAMPLE_SIZE,IN_X(1,:),IN_X(2,:),C(1,2))
    CALL com_correl(SAMPLE_SIZE,IN_X(1,:),IN_X(2,:),C(2,1))
  ELSEIF (dim1==3) THEN
    CALL com_correl(SAMPLE_SIZE,IN_X(1,:),IN_X(2,:),C(1,2))
    CALL com_correl(SAMPLE_SIZE,IN_X(1,:),IN_X(2,:),C(2,1))
    CALL com_correl(SAMPLE_SIZE,IN_X(1,:),IN_X(3,:),C(1,3))
    CALL com_correl(SAMPLE_SIZE,IN_X(1,:),IN_X(3,:),C(3,1))
    CALL com_correl(SAMPLE_SIZE,IN_X(2,:),IN_X(3,:),C(2,3))
    CALL com_correl(SAMPLE_SIZE,IN_X(2,:),IN_X(3,:),C(3,2))
  ELSE
    WRITE(*,*) 'ERROR in SW_test dim1 !!!'
    PRINT *
    READ *
  ENDIF

  DO i = 1,dim1
    IF (flag1(i)==1) THEN
      C(1:dim1,i) = 1
      C(i,1:dim1) = 1
    ENDIF
  ENDDO

    work04 = 0.0d0
    DO i = 1,dim1
      DO j = 1,dim1
        NC(i,j) = (C(i,j)**l)*(1.0d0-(u*(1.0d0-C(i,j))**u)/v)
        work04  = work04 + NC(i,j)
      ENDDO
    ENDDO
    T  = work04-dble(dim1) ! total
    IF (dim1==1) THEN
      e = 1.0d0
    ELSE
      mC = T/(dble(dim1)**2-dble(dim1)) ! average correlation
      e  = dble(dim1)/(1.0d0+(dble(dim1)-1.0d0)*mC) ! equivalent degrees of freedom
    eNDIF
    H  = e*sum(tmp_R)/dble(dim1)
    IF (isnan(H) .OR. isnan(e)) THEN
      WRITE(*,*) 'ERROR in SW_ROYSTON_TEST!!'
      write(*,*) 'tmp_W', tmp_W
      write(*,*) 'tmp_Z', tmp_Z
      write(*,*) 'tmp_R', tmp_R
      write(*,*) 'IN_X(1,:)', IN_X(1,:)
      write(*,*) '---------------------------'
      IF (dim1>1) write(*,*) 'IN_X(2,:)', IN_X(2,:)
      write(*,*) 'work01', work01
      write(*,*) 'work02', work02
      write(*,*) 'NC', NC
      write(*,*) 'C', C
      write(*,*) 'T', T
      write(*,*) 'mC', mC
      PRINT *
      READ *
    ENDIF
    CALL cdfchi(1,tmp_cdf,work05,H,e,status,bound)
    P = work05
  !--- Finalize ---
  IF (P>=alpha) THEN ! Data analyzed have a normal distribution.
    OUT_RESULT = 1.0d0
  ELSE ! Data analyzed do not have a normal distribution.
    OUT_RESULT = 0.0d0
  ENDIF

  IF (PRESENT(OUT_W)) THEN
    OUT_W    = tmp_W
    OUT_KURT = tmp_kurt
    OUT_Z    = tmp_Z
  ENDIF
!!!  IF ((tmp_W(1) < 0.9904) .AND. (P>=alpha)) THEN
!!!    write(*,*) 'tttttttets'
!!!    write(*,*) 'tmp_cdf',tmp_cdf
!!!    write(*,*) 'work05',work05
!!!    write(*,*) 'H',H
!!!    write(*,*) 'e',e
!!!    write(*,*) 'R',tmp_R(1)
!!!    write(*,*) 'W',tmp_W(1)
!!!    write(*,*) 'Z',tmp_Z(1)
!!!    write(*,*) 'tmp_var1',work91
!!!    write(*,*) 'work01',work01
!!!    write(*,*) 'tmp_var2',work92
!!!    write(*,*) 'work02',work02
!!!    write(*,*) 'kkurosawa1---------'
!!!    print * 
!!!    read *
!!!  ENDIF
  RETURN
END SUBROUTINE SW_ROYSTON_TEST

!-----------------------------------------------------------------------
! Shapiro-Wilk' W statistic for assessing a sample normality
! Kenta Kurosawa, June 21, 2021
! ORIGINAL: ShaWilstat in Roystest.m
!           https://jp.mathworks.com/matlabcentral/fileexchange/17811-roystest
!-----------------------------------------------------------------------
SUBROUTINE ShaWilstat(IN_X,OUT_W,SAMPLE_SIZE,flag1,OUT_KURT)
  IMPLICIT NONE
  !-- in & out
  INTEGER     ,INTENT(IN)  :: SAMPLE_SIZE, flag1
  REAL(r_size),INTENT(IN)  :: IN_X(SAMPLE_SIZE)
  REAL(r_size),INTENT(OUT) :: OUT_W
  REAL(r_size),INTENT(OUT) :: OUT_KURT
  !-- Shapiro-Wilk test
  REAL(r_size) :: tmp_x(SAMPLE_SIZE)
  REAL(r_size) :: tmp_m(SAMPLE_SIZE)
  REAL(r_size) :: tmp_w(SAMPLE_SIZE)
  REAL(r_size) :: tmp_c(SAMPLE_SIZE)
  REAL(r_size) :: tmp_u, phi
  INTEGER      :: ct
  REAL(r_size) :: kurt
  REAL(r_size) :: W
  REAL(r_size),DIMENSION(6) :: p1, p2
  !--
  INTEGER :: i,j,k
  REAL(r_size)                        :: work01, work02
  REAL(r_size),DIMENSION(SAMPLE_SIZE) :: work11
  !--
  real ( kind = 8 ) r8_normal_01_cdf_inverse

  !--- QC (KKUROSAWA)
  IF (flag1 == 1) THEN
    OUT_W = 0.0001d0
    RETURN
  ENDIF

  tmp_x = IN_X
  CALL qsort(SAMPLE_SIZE,tmp_x)
  DO i = 1,SAMPLE_SIZE
    work01 = (dble(i)-3.0d0/8.0d0)/(dble(SAMPLE_SIZE)+0.25d0)
    tmp_m(i) = r8_normal_01_cdf_inverse(work01)
  ENDDO
  tmp_w = 0.0d0
  CALL CAL_KURTOSIS(SAMPLE_SIZE,tmp_x,kurt)
  IF (kurt > 3.0d0) THEN ! %Shapiro-Francia test is better for leptokurtic samples
    work01   = dot_product(tmp_m,tmp_m)
    tmp_w    = 1/sqrt(work01)*tmp_m
    CALL com_mean(SAMPLE_SIZE,tmp_x,work01)
    work11 = tmp_x - work01
    work01 = dot_product(work11,work11)
    work02 = dot_product(tmp_w ,tmp_x)
    work02 = work02**2
    W = work02/work01
  ELSE ! Shapiro-Wilk test is better for platykurtic samples
    work01 = dot_product(tmp_m,tmp_m)
    tmp_c  = 1/sqrt(work01)*tmp_m
    tmp_u  = 1/sqrt(dble(SAMPLE_SIZE))
    p1 = (/-2.706056d0,4.434685d0,-2.071190d0,-0.147981d0,0.221157d0,tmp_c(SAMPLE_SIZE)/)
    p2 = (/-3.582633d0,5.682633d0,-1.752461d0,-0.293762d0,0.042981d0,tmp_c(SAMPLE_SIZE-1)/)
    tmp_w(SAMPLE_SIZE) = p1(1)*tmp_u**5+p1(2)*tmp_u**4+p1(3)*tmp_u**3+p1(4)*tmp_u**2+p1(5)*tmp_u**1+p1(6)
    tmp_w(1) = -tmp_w(SAMPLE_SIZE)
    IF (SAMPLE_SIZE==3) THEN
      tmp_w(1) = 0.707106781d0
      tmp_w(SAMPLE_SIZE) = -tmp_w(1)
    ENDIF
    IF (SAMPLE_SIZE>=6) THEN
      ct = 3
      tmp_w(SAMPLE_SIZE-1) = p2(1)*tmp_u**5+p2(2)*tmp_u**4+p2(3)*tmp_u**3+p2(4)*tmp_u**2+p2(5)*tmp_u**1+p2(6)
      tmp_w(2) = -tmp_w(SAMPLE_SIZE-1)
      work01 = dot_product(tmp_m,tmp_m) - 2*tmp_m(SAMPLE_SIZE)**2 - 2*tmp_m(SAMPLE_SIZE-1)**2
      work02 = 1.0d0 - 2*tmp_w(SAMPLE_SIZE)**2 - 2*tmp_w(SAMPLE_SIZE-1)**2
      phi = work01/work02
    ELSE
      ct = 2
      work01 = dot_product(tmp_m,tmp_m) - 2*tmp_m(SAMPLE_SIZE)**2
      work02 = 1.0d0 - 2*tmp_w(SAMPLE_SIZE)**2
      phi = work01/work02
    ENDIF
    tmp_w(ct:SAMPLE_SIZE-ct+1) = tmp_m(ct:SAMPLE_SIZE-ct+1)/sqrt(phi)
    CALL com_mean(SAMPLE_SIZE,tmp_x,work01)
    work11 = tmp_x - work01
    work01 = dot_product(work11,work11)
    work02 = dot_product(tmp_w ,tmp_x)
    work02 = work02**2
    W = work02/work01
  !    write(*,*) 'phi',IN_x
  ENDIF

  OUT_W = W
  OUT_KURT = kurt
  RETURN

END SUBROUTINE ShaWilstat

!-----------------------------------------------------------------------
! KURTOSIS
! Kenta Kurosawa, June 21, 2021
! ORIGINAL: kurtosis.m
!-----------------------------------------------------------------------
SUBROUTINE CAL_KURTOSIS(dim1,IN_X,OUT_KURT)
  IMPLICIT NONE
  !-- in & out
  INTEGER,INTENT(IN)   :: dim1
  REAL(r_size),INTENT(IN)  :: IN_X(dim1)
  REAL(r_size),INTENT(OUT) :: OUT_KURT
  !--
  INTEGER :: j, n
  REAL(r_size) :: tmp_var1, tmp_var2(dim1)
  REAL(r_size) :: x0(dim1), s2, m4, k

  CALL com_mean(dim1,IN_X,tmp_var1)
  x0 = IN_X - tmp_var1
  CALL com_mean(dim1,x0**2,s2)
  CALL com_mean(dim1,x0**4,m4)
  k = m4/s2**2

  OUT_KURT = k
  RETURN
END SUBROUTINE CAL_KURTOSIS
END MODULE SW_test
