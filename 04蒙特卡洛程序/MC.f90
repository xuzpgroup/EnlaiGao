!--------------------------------------------------
! program to simulate graphene growth
! Enlai Gao
!--------------------------------------------------
PROGRAM graphene
  IMPLICIT NONE
  INTEGER :: i,j,k,k1,k2,l,n,xl,xh,yl,yh,xtem,ytem,temn,diff,step,temp
  INTEGER,DIMENSION(:,:), ALLOCATABLE :: m,t
  DOUBLE PRECISION :: c,x1,y1,x2,y2,x3,y3,r,bond,dcut,ecut,temper,Kb,T1,Ea1,Ea2,Ez,w
  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE :: phi0,phi,dphidt,xi,dxidt
  DOUBLE PRECISION :: pi = 4.D0*DATAN(1.D0),dx,dt
  DOUBLE PRECISION :: c2
  character( len = 512 ) :: cFile

 c= 0.2
 n=6
 temn = 1 
 step = 100	  !evolution steps	
 diff = 1000000  !diffusion Hz
 bond = 1.42d0
 dcut = 4.d0*bond
 Kb = 8.6173324d-5
 T1 = 900.d0    !Temperature
 Ea1 = 2.016d0
 Ea2 = 1.112d0
 Ez  = 0.579d0
 w   = 1000
 temper = exp(-Ea2/(Kb*T1))
! temper = Kb*T1
!  print*, T1,temper
 ALLOCATE(m(2000000,4),t(1000000,4))

!--------------------------------------------------------------------
! nucleation
!--------------------------------------------------------------------
   
   m(1,1) = 0
   m(1,2) = 0
   m(1,3) = 2
   m(2,1) = 1
   m(2,2) = 0
   m(2,3) = 2
   m(3,1) = 0
   m(3,2) = 1
   m(3,3) = 2
   m(4,1) = 1
   m(4,2) = 1
   m(4,3) = 2
   m(5,1) = 0
   m(5,2) = 2
   m(5,3) = 2
   m(6,1) = 1
   m(6,2) = 2
   m(6,3) = 2
   k1 = 0
!--------------------------------------------------------------------
! growth
!--------------------------------------------------------------------
Do l=1,step
print*,l

 DO i = 1,n   

  if (MOD(abs(m(i,1)),2) .eq. 0 .and. MOD(abs(m(i,2)),2) .eq. 0 ) then
   m(i,4) = 1





   xtem=m(i,1) 
   ytem=m(i,2)+1

   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do

   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)=3
   temn=temn+1
   else
   end if
   k1 = 0
   k2=  0

  

 



   xtem=m(i,1)+1 
   ytem=m(i,2)
   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do


   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 2
   temn=temn+1

 
   else
   end if
   k1 = 0
   k2=  0



   xtem=m(i,1) 
   ytem=m(i,2)-1
   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do
   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 3
   temn=temn+1 
 
   else
   end if
   k1 = 0
   k2=  0

   
 elseif (MOD(abs(m(i,1)),2) .eq. 1 .and. MOD(abs(m(i,2)),2) .eq. 0 ) then
   m(i,4) = 2

   xtem=m(i,1)
   ytem=m(i,2)+1
   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do
   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)=4
   temn=temn+1
   else
   end if
   k1 = 0
   k2=  0
   xtem=m(i,1)-1 
   ytem=m(i,2)

   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do
   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 1
   temn=temn+1
   else
   end if
   k1 = 0
   k2=  0







   xtem=m(i,1) 
   ytem=m(i,2)-1 
   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do
   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 4
   temn=temn+1
 
 
   else
   end if
   k1 = 0
   k2=  0
  

elseif (MOD(abs(m(i,1)),2) .eq. 0 .and. MOD(abs(m(i,2)),2) .eq. 1 ) then
   m(i,4) = 3

   xtem=m(i,1)
   ytem=m(i,2)+1

   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do

   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 1
   temn=temn+1
   else
   end if
   k1 = 0
   k2=  0
  

 



   xtem=m(i,1)-1 
   ytem=m(i,2)

   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do
   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 4
   temn=temn+1 
   else
   end if
   k1 = 0
   k2=  0







   xtem=m(i,1) 
   ytem=m(i,2)-1

   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do

   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 1
   temn=temn+1

 
   else
   end if
   k1 = 0
   k2=  0


 elseif (MOD(abs(m(i,1)),2) .eq. 1 .and. MOD(abs(m(i,2)),2) .eq. 1 ) then
   m(i,4) = 4

      xtem=m(i,1) 
   ytem=m(i,2)+1
   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do


   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)=2
   temn=temn+1
   else
   end if
   k1 = 0
   k2=  0
  

 



   xtem=m(i,1)+1 
   ytem=m(i,2)

   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do
   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 3
   temn=temn+1

 
   else
   end if
   k1 = 0
   k2=  0







   xtem=m(i,1) 
   ytem=m(i,2)-1

   do j =1,n
   if (xtem .ne. m(j,1) .or. ytem .ne. m(j,2)) then
   k1=k1+1
   end if
   end do

   do k =1,temn
   if (xtem .ne. t(k,1) .or. ytem .ne. t(k,2)) then
   k2=k2+1
   end if
   end do
   if (k1 .eq. n .and. k2 .eq. temn ) then
   t(temn,1) = xtem
   t(temn,2)= ytem
   t(temn,4)= 2
   temn=temn+1
 
 
   else
   end if
   k1 = 0
   k2=  0
   end if
 
  end do

  
  

  do i =1,temn-1
  call random_number(r)
  if (r .lt. c ) then
  t(i,3)=1
  end if
  end do
 
 
    
do k =1,diff
 do i = 1,temn-1

 if (t(i,3) .eq. 1  ) then  
  do j = 1,temn-1
    if(t(j,3) .ne. 1 ) then
  select case(t(i,4)) 
  case(1)
  x1=dble(t(i,1))*1.5d0*bond
  y1=dble(t(i,2))*sqrt(3.d0)*bond/2.d0
  case(2)
  x1=dble(t(i,1)-1)*1.5d0*bond+bond
  y1=dble(t(i,2))*dsqrt(3.d0)*bond/2.d0
  case(3)
  x1=dble(t(i,1))*1.5d0*bond-0.5d0*bond
  y1=dble(t(i,2))*dsqrt(3.d0)*bond/2.d0
  case(4)
  x1=dble(t(i,1)-1)*1.5d0*bond+1.5d0*bond
  y1=dble(t(i,2))*dsqrt(3.d0)*bond/2.d0
  case default

  end select
  select case(t(j,4)) 
  case(1)
  x2=dble(t(j,1))*1.5d0*bond
  y2=dble(t(j,2))*sqrt(3.d0)*bond/2.d0
  case(2)
  x2=dble(t(j,1)-1)*1.5d0*bond+bond
  y2=dble(t(j,2))*dsqrt(3.d0)*bond/2.d0
  case(3)
  x2=dble(t(j,1))*1.5d0*bond-0.5d0*bond
  y2=dble(t(j,2))*dsqrt(3.d0)*bond/2.d0
  case(4)
  x2=dble(t(j,1)-1)*1.5d0*bond+1.5d0*bond
  y2=dble(t(j,2))*dsqrt(3.d0)*bond/2.d0
  case default
  end select


        if ((x1-x2)**2+(y1-y2)**2 .lt. (1.2d0*bond)**2 )then
        
        call random_number(r)
        temper = exp(-Ea1/(Kb*T1))
        if (r .lt. temper) then
        temp   = t(i,1)
        t(i,1) = t(j,1)
        t(j,1) = temp  
        temp   = t(i,2)
        t(i,2) = t(j,2)
        t(j,2) = temp  
        temp   = t(i,3)
        t(i,3) = t(j,3)
        t(j,3) = temp  
        temp   = t(i,4)
        t(i,4) = t(j,4)
        t(j,4) = temp  
        end if

        elseif ((x1-x2)**2+(y1-y2)**2 .gt. (1.2d0*bond)**2 .and. (x1-x2)**2+(y1-y2)**2 .lt. (1.9d0*bond)**2) then
       
        call random_number(r)
        temper = exp(-Ez/(Kb*T1))
        if (r .lt. temper) then
        temp   = t(i,1)
        t(i,1) = t(j,1)
        t(j,1) = temp  
        temp   = t(i,2)
        t(i,2) = t(j,2)
        t(j,2) = temp  
        temp   = t(i,3)
        t(i,3) = t(j,3)
        t(j,3) = temp  
        temp   = t(i,4)
        t(i,4) = t(j,4)
        t(j,4) = temp  
        end if 
        
        elseif  ( (x1-x2)**2+(y1-y2)**2 .gt. (1.9d0*bond)**2) then
       
        call random_number(r)
        temper = exp(-Ea2/(Kb*T1))
        if (r .lt. temper) then
        temp   = t(i,1)
        t(i,1) = t(j,1)
        t(j,1) = temp  
        temp   = t(i,2)
        t(i,2) = t(j,2)
        t(j,2) = temp  
        temp   = t(i,3)
        t(i,3) = t(j,3)
        t(j,3) = temp  
        temp   = t(i,4)
        t(i,4) = t(j,4)
        t(j,4) = temp  
        end if

        else 
        end if

      else
    end if
  end do

     else  
   end if
 end do

end do
 
 temp = n
 do i =1,temn-1
    if (t(i,3) .eq. 1  ) then
    temp = temp+1
    m(temp,1)=t(i,1)
    m(temp,2)=t(i,2)
    m(temp,3)=t(i,3)
    m(temp,4)=t(i,4) 
    
    end if
 end do
 

n=temp
temn =1

 write( cFile , * ) l+10000
 cFile = 'data.' // Trim(AdjustL(cFile)) // ''
 OPEN (1, FILE =cFile, STATUS='UNKNOWN')
     DO i = 1,n
     select case(m(i,4)) 
     case(1)
     x1=dble(m(i,1))*1.5d0*bond
     y1=dble(m(i,2))*sqrt(3.d0)*bond/2.d0
     case(2)
     x1=dble(m(i,1)-1)*1.5d0*bond+bond
     y1=dble(m(i,2))*dsqrt(3.d0)*bond/2.d0
     case(3)
     x1=dble(m(i,1))*1.5d0*bond-0.5d0*bond
     y1=dble(m(i,2))*dsqrt(3.d0)*bond/2.d0
     case(4)
     x1=dble(m(i,1)-1)*1.5d0*bond+1.5d0*bond
     y1=dble(m(i,2))*dsqrt(3.d0)*bond/2.d0
     case default
     end select

     WRITE(1,'(I7,5F20.5)') l,x1,y1
     END DO
 CLOSE(1)

end do

 END PROGRAM graphene



