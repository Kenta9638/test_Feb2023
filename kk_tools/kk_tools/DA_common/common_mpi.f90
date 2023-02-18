MODULE common_mpi
!=======================================================================
!
! [PURPOSE:] General MPI procedures
!
! [HISTORY:]
!   09/06/2005 Takemasa MIYOSHI  created
!
!=======================================================================
  USE common, only: r_size, r_dble, r_sngl
  IMPLICIT NONE
  PUBLIC
  INCLUDE 'mpif.h'

  INTEGER,SAVE :: nprocs
  INTEGER,SAVE :: myrank
  INTEGER,SAVE :: MPI_r_size
  INTEGER,SAVE :: MPI_PARA_LOOP_NUM
  INTEGER,SAVE :: mpi_str
  INTEGER,SAVE :: mpi_end

CONTAINS
SUBROUTINE initialize_mpi
  IMPLICIT NONE
  INTEGER :: ierr
  CALL MPI_INIT(ierr)
  CALL MPI_COMM_SIZE(MPI_COMM_WORLD,nprocs,ierr)
  CALL MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ierr)
  WRITE(6,'(A,I6.6,A,I6.6)') 'Hello from MYRANK ',myrank,'/',nprocs-1
  IF(r_size == r_dble) THEN
    MPI_r_size = MPI_DOUBLE_PRECISION
  ELSE IF(r_size == r_sngl) THEN
    MPI_r_size = MPI_REAL
  END IF

  RETURN
END SUBROUTINE initialize_mpi

SUBROUTINE finalize_mpi
  IMPLICIT NONE
  INTEGER :: ierr
  CALL MPI_FINALIZE(ierr)

  RETURN
END SUBROUTINE finalize_mpi

!--- KKUROSAWA ---
SUBROUTINE get_str_end_num_mpi(IN_DIM)
  IMPLICIT NONE
  INTEGER :: ierr, nij1max,i
  INTEGER,INTENT(IN) ::  IN_DIM

  i = MOD(IN_DIM,nprocs)
  nij1max = (IN_DIM - i)/nprocs + 1
  IF(myrank < i) THEN
    MPI_PARA_LOOP_NUM = nij1max
    mpi_str = 1+nij1max*myrank
  ELSE
    MPI_PARA_LOOP_NUM = nij1max - 1
    mpi_str = nij1max*i+1+(myrank-i)*(nij1max-1)
  ENDIF
  mpi_end = mpi_str + MPI_PARA_LOOP_NUM-1
  WRITE(6,'(A,I3.3,A,I6)') 'MYRANK ',myrank,' number of loops: MPI_PARA_LOOP_NUM= ',MPI_PARA_LOOP_NUM
  WRITE(*,*) 'MYRANK ',myrank,' mpi_str= ',mpi_str,' mpi_end= ',mpi_end
  WRITE(*,*) '-------------------------------------------------------------------------'
ENDSUBROUTINE get_str_end_num_mpi



END MODULE common_mpi
