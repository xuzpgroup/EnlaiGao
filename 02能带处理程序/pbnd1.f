C     +--------------------------------------------------------
C     |  Purpose: Convert EIGENVAL for band plot using gnuplot
C     |  Program: pbnd.f 
C     |  Author:  Hou Zhufeng
C     |  Date:    10. 29. 2003
C     |  Revise: Enlai Gao enlaigao@163.com
C     |  Email:  enlaigao@163.com
C     |  Date:    12. 29. 2015
C     +--------------------------------------------------------
      program pbnd 
      real*8  eigup,eigdn,kp,xxx,xx
      integer  iii,nb,nk,nn,jj,i,j,ispin,k
      dimension eigup(1000,1000),eigdn(1000,1000),iii(3),xxx(5)
      real*8  eig
      dimension eig(1000,1000),kp(1000,3)
      character*30  name
      real*8 xk1,yk1,zk1,xk2,yk2,zkz,a,b,x,y,z,disk,dish
      dimension a(3,3),b(3,3),disk(1000),dish(10000)
      character*2 labhk
      real*8 phighk,dlx,dd,efermi
      integer ndiv,ii,pskp
      dimension phighk(100,3)
      dimension labhk(100)
      dimension ndiv(100),pskp(100)
c
c---------read EIGENVAL
c
      open(4,file='EIGENVAL',status='old')
      read(4,*) (iii(i),i=1,3),ispin
      read(4,*) (xxx(i),i=1,5)
      read(4,*) xx
      read(4,*) name
      read(4,*) name
      read(4,*) nb,nk,nn
      if(ispin.eq.2) then
        open(7,file='BANDSUP')
        open(8,file='BANDSDN')
        do i=1,nk
          read(4,*) 
          read(4,*) (kp(i,j),j=1,3),w
          do  j=1,nn
             read(4,*) jj,eigup(i,j),eigdn(i,j)
          enddo
          write(7,100) i,(eigup(i,j),j=1,nn)
          write(8,100) i,(eigdn(i,j),j=1,nn)
        enddo
      else
        open(9,file='BANDS')
        do i=1,nk
          read(4,*) 
          read(4,*) (kp(i,j),j=1,3),w
          do j=1,nn
             read(4,*) jj,eig(i,j)
          enddo
          write(9,100) i,(eig(i,j),j=1,nn)
        enddo
      endif 
c---option:
100   format(i4,5x,80f10.4)
c 
c----- Read direct and reciprocal lattice vectors ------
c
      open(5,file='syml')
      read(5,*) nhighk
      read(5,*) (ndiv(i),i=1,nhighk-1)
      do i =1,3
         read(5,*) (a(i,j),j=1,3),(b(i,j),j=1,3)
c        read(5,*) (a(i,j),b(i,j),j=1,3)
      enddo
      read(5,*)efermi
      print*, efermi
c
c---- Calculate disk(i) for band plot------
c
      disk(1)=0.d0
      dish(1)=0.d0
      dd = 0.d0
      ii=1
      do i=2, nk
       x = kp(i-1,1)
       y = kp(i-1,2)
       z = kp(i-1,3)
       xk1 = x*b(1,1) + y*b(2,1) + z*b(3,1)
       yk1 = x*b(1,2) + y*b(2,2) + z*b(3,2)
       zk1 = x*b(1,3) + y*b(2,3) + z*b(3,3)
       x = kp(i,1)
       y = kp(i,2)
       z = kp(i,3)
       xk2 = x*b(1,1) + y*b(2,1) + z*b(3,1)
       yk2 = x*b(1,2) + y*b(2,2) + z*b(3,2)
       zk2 = x*b(1,3) + y*b(2,3) + z*b(3,3)
       dish(i) = sqrt((xk2-xk1)**2+(yk2-yk1)**2+(zk2-zk1)**2)
       dd=dd+dish(i)
       disk(i) = dd
      enddo
c
c---- Write out data for  origin 6.0 -----
c
      print*, efermi
      if(ispin.eq.2) then
        open(10,file='upbnd.dat')
        open(11,file='dnbnd.dat')
       do j=1,nn
         do i=1,nk
          write(10,200) disk(i),eigup(i,j)-efermi
          write(11,200) disk(i),eigdn(i,j)-efermi
         enddo
          write(10,200)
          write(11,200)
       enddo
      else
        open(12,file='bnd.dat')
         do j=1,nn
           do i=1,nk
            write(12,200) disk(i),eig(i,j)-efermi
           enddo
            write(12,200) 
        enddo 
      endif
c option---:
 200  format(50f9.4)
      open(13,file='highk.dat')
      do i=1,nhighk-1
      k=0
      do j=1,i
         k=ndiv(j)+k
      enddo
         write(13,300) disk(k)
      enddo
c option----
 300  format(50f9.4)
c
      stop
      end
c----------------------- end ---------------------------
