!--------------------------------------------------
! program to handle strain-stress
! Enlai Gao, Jun. 2016, Tsinghua
!--------------------------------------------------
PROGRAM strainstress
  IMPLICIT NONE
  INTEGER :: i,j,l,m,n,nmin,nmax,nline
  DOUBLE PRECISION :: tmp,average,total,tmp1,total1
  INTEGER, DIMENSION(:,:), ALLOCATABLE :: rawdata
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: outdata
  CHARACTER(LEN=80) :: c,g
  m=4608
  n=3288
  ALLOCATE(rawdata(m,n),outdata(n))
    rawdata=0
    outdata=0d0
  OPEN (14, FILE = 'out.txt', STATUS='UNKNOWN') 
  READ(14,*)
  DO j = 1,n
    DO i=1,m
    READ(14,*) l,c,g,rawdata(i,j)
    ENDDO
  ENDDO
  CLOSE(14) 
  DO j = 1,n
       nmax=m+1
       nmin=m+1
   DO i=1,m
      IF(rawdata(i,j) .eq. 255 .and. rawdata(i+1,j) .eq. 255) THEN
        nmax=i
      ENDIF
   ENDDO                          
   DO i=1,m
      IF(rawdata(i,j) .eq. 255 .and. rawdata(i+1,j) .eq. 255) goto 30
   ENDDO
   30 continue
       nmin=i
       outdata(j)=dble(nmax-nmin)
       !WRITE(*,*) nmin,outdata(j)
  ENDDO
   average=sum(outdata(801:1000))/200d0
   tmp=0d0
   tmp1=0d0
   total=0d0
   total1=0d0
   DO j = 1,n   
    if (outdata(j) .gt. 0.9d0*average .and. outdata(j) .lt. 1.1d0*average) then
       total=outdata(j)+total
       tmp=tmp+1d0
    endif
    if (dble(j) .gt. dble(n)/3d0 .and. dble(j) .lt. 2d0*dble(n)/3d0 .and. outdata(j) .gt. 0.9d0*average .and. outdata(j) .lt. 1.1d0*average) then
       total1=outdata(j)+total1
       tmp1=tmp1+1d0
    endif
   ENDDO
   write(*,*) sum(outdata(801:1000))/200d0,total/tmp,total1/tmp1



!    WRITE(*,*) rawdata(nmin,2),rawdata(nmin,3)
!    WRITE(*,*) rawdata(nmax,2),rawdata(nmax,3)
 END PROGRAM strainstress
