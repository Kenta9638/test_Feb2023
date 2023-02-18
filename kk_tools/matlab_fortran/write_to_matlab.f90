program test


  implicit none
  CHARACTER(256) :: file ='./test.dat'
  integer :: b
  REAL(4) ,ALLOCATABLE:: tmp_work3d(:,:)
  integer :: irec,ios
  integer :: nobstotal
  integer :: i,j,time_step
  integer :: AaA,BbB,CcC,DdD,EeE
  CHARACTER(6) :: AA,BB,n,n2
  CHARACTER(256) :: filename,filename2

  integer :: iterm
  integer :: ilev,ij,k,m,l

!----------------------------------------
!  read grads file qs
!----------------------------------------
   ALLOCATE (tmp_work3d(243,1984))

   do i = 1,243
     do j = 1,1984
       tmp_work3d(i,j) = i
     enddo
   enddo

   filename = trim(file)

   write(*,*) trim(filename)
   write(*,*) '====================================='

   do i = 1,243
     open(10,file=filename,action='write')!,recl=1984)
     write(10,*) tmp_work3d(i,:)
!     write(*,*) i
   enddo
   close(10)

   write(*,*) tmp_work3d(1,1)
   write(*,*) '====================================='
   write(*,*) tmp_work3d(1,2)
   write(*,*) '====================================='
   write(*,*) tmp_work3d(1,30)
   write(*,*) '====================================='
   write(*,*) tmp_work3d(111,130)
   write(*,*) '====================================='
   write(*,*) tmp_work3d(221,230)
   write(*,*) '====================================='

stop




end program
