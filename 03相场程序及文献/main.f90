program main
implicit none
real*8::k,eg,ANISO,phi,F,D,TAU,taus
integer::NX,NY
real*8::H,DT
integer::timesteps,outstep
real*8,parameter::pi=dacos(-1d0)
real*8,allocatable::psi(:,:),ksi(:,:)
integer::ii,jj
real*8,allocatable::grad_psi_X(:,:),grad_psi_Y(:,:),aX(:,:),aY(:,:),kappa2(:,:),angle(:,:),&
                 &  kappa_prime(:,:),kappa(:,:),dXdY(:,:),dYdX(:,:),&
                 &  grad_kappa2_X(:,:),grad_kappa2_Y(:,:),lap_psi(:,:),lap_ksi(:,:)
real*8,allocatable::scal_mat(:,:),dyna_mat(:,:),dpsi_mat(:,:)
integer::istep
integer::ip,im,jp,jm,ipp,imm,jpp,jmm
real*8::po,ko,scal,dyna,dpsi
character*200::filename,filenum
real*8::Fp,ksi_eq,ksi_inf
real*4::time_begin,time_end
real*8::sump,sump_done(9)
integer::isum
real*8,parameter::c2=-1d0/30d0,c3=-1d0/60d0,c4=4d0/15d0,c5=13d0/15d0,c6=-21d0/5d0

open(10,file='input.txt',action='read')
read(10,*)Fp
read(10,*)ksi_eq
read(10,*)DT
close(10)

ksi_inf=ksi_eq*3d0;
taus=ksi_inf/Fp;
F=Fp

TAU =1d0;    !%PF relaxation time
k =sqrt(2d0);  !%interfacial width
eg= 0.04d0;    !%modulation of the interfacial width
ANISO= 6d0; !%anisotropy %2*PI/ANISO
phi= 10d0*20d0!10.0;
D= 12d0;
NX =512;    !%size of the mesh NX*NY
NY =512;

H= 1d0; !%spatial %resolution
timesteps =100 
outstep = 100

sump_done=NX*NY*(-1d0)+2*NX*NY* &
    & (/ 1d0/1024,1d0/512,1d0/256,1d0/128,1d0/64,1d0/32,1d0/16,1d0/8,1d0/4 /)

!%intial temperature and phase %field information
allocate(psi(NY,NX)); psi=-1d0
allocate(ksi(NY,NX)); ksi=ksi_inf
do ii=1,NX
  do jj=1,NY
    if (((ii-NX/2d0)**2+(jj-NY/2d0)**2)*H**2<10d0**2) then
      psi(ii,jj) = 1d0;
    endif
  enddo
enddo
!%preallocation of matrices for %faster calculations
allocate(grad_psi_X(NY,NX)); grad_psi_X=0d0 
allocate(grad_psi_Y(NY,NX)); grad_psi_Y=0d0
allocate(aX(NY,NX)); aX=0d0
allocate(aY(NY,NX)); aY=0d0
allocate(kappa2(NY,NX)); kappa2=0d0
allocate(angle(NY,NX)); angle=0d0 
allocate(kappa_prime(NY,NX)); kappa_prime=0d0 
allocate(kappa(NY,NX)); kappa=0d0
allocate(dXdY(NY,NX)); dXdY=0d0
allocate(dYdX(NY,NX)); dYdX=0d0 
allocate(grad_kappa2_X(NY,NX)); grad_kappa2_X=0d0 
allocate(grad_kappa2_Y(Ny,NX)); grad_kappa2_Y=0d0
allocate(lap_psi(NY,NX)); lap_psi=0d0
allocate(lap_ksi(NY,NX)); lap_ksi=0d0
allocate(scal_mat(NY,NX)); scal_mat=0d0
allocate(dyna_mat(NY,NX)); dyna_mat=0d0
allocate(dpsi_mat(NY,NX)); dpsi_mat=0d0

call cpu_time(time_begin)

istep=0
isum=1
do while(.true.) 
  istep=istep+1

  do jj=1,NY
    do ii=1,NX
      ip = mod(ii,NX)+1;
      im = mod((NX+ii-2),NX)+1;
      jp = mod(jj,NY)+1;
      jm = mod((NY+jj-2),NY)+1;
      grad_psi_X(ii,jj) = ((psi(ip,jj) - psi(im,jj))/(2d0*H));
      grad_psi_Y(ii,jj) = ((psi(ii,jp) - psi(ii,jm))/(2d0*H));
      lap_psi(ii,jj) = (4.0*(psi(ip,jj)+psi(im,jj)+psi(ii,jp)+psi(ii,jm)) &
                   & +  psi(ip,jp)+psi(im,jm)+psi(im,jp)+psi(ip,jm) &
                   & -  20.0*psi(ii,jj))/(6.0*H**2);
      lap_ksi(ii,jj) = (4.0*(ksi(ip,jj)+ksi(im,jj)+ksi(ii,jp)+ksi(ii,jm)) &
                   & +  ksi(ip,jp)+ksi(im,jm)+ksi(im,jp)+ksi(ip,jm) &
                   & -  20.0*ksi(ii,jj))/(6.0*H**2);
      !ipp = mod(ip,NX)+1;
      !imm = mod((NX+im-2),NX)+1;
      !jpp = mod(jp,NY)+1;
      !jmm = mod((NY+jm-2),NY)+1;
      !lap_psi(ii,jj)=c2*(psi(imm,jm)+psi(imm,jp)+psi(ipp,jp)+psi(ipp,jm) &
      !                  +psi(ip,jmm)+psi(im,jmm)+psi(ip,jpp)+psi(im,jpp)) &
      !              +c3*(psi(imm,jj)+psi(ipp,jj)+psi(ii,jmm)+psi(ii,jpp)) &
      !              +c4*(psi(ip,jp)+psi(ip,jm)+psi(im,jp)+psi(im,jm)) &
      !              +c5*(psi(ip,jj)+psi(im,jj)+psi(ii,jp)+psi(ii,jm)) &
      !              +c6*psi(ii,jj)
      !lap_ksi(ii,jj)=c2*(ksi(imm,jm)+ksi(imm,jp)+ksi(ipp,jp)+ksi(ipp,jm) &
      !                  +ksi(ip,jmm)+ksi(im,jmm)+ksi(ip,jpp)+ksi(im,jpp)) &
      !              +c3*(ksi(imm,jj)+ksi(ipp,jj)+ksi(ii,jmm)+ksi(ii,jpp)) &
      !              +c4*(ksi(ip,jp)+ksi(ip,jm)+ksi(im,jp)+ksi(im,jm)) &
      !              +c5*(ksi(ip,jj)+ksi(im,jj)+ksi(ii,jp)+ksi(ii,jm)) &
      !              +c6*ksi(ii,jj)
    enddo
  enddo

  angle=datan2(grad_psi_Y,grad_psi_X)
    
  kappa=k*(1d0+eg*dcos(ANISO*angle))
  kappa_prime=-k*ANISO*eg*dsin(ANISO*angle)
    
  aY=-kappa*kappa_prime*grad_psi_Y
  aX=kappa*kappa_prime*grad_psi_X
  kappa2=kappa**2
    
  do jj=1,NY
    do ii=1,NX
      ip = mod(ii,NX)+1;
      im = mod((NX+ii-2),NX)+1;
      jp = mod(jj,NY)+1;
      jm = mod((NY+jj-2),NY)+1;
      dXdY(ii,jj) = (aY(ip,jj) - aY(im,jj))/(2d0*H);
      dYdX(ii,jj) = (aX(ii,jp) - aX(ii,jm))/(2d0*H);
      grad_kappa2_X(ii,jj) = (kappa2(ip,jj) - kappa2(im,jj))/(2d0*H);
      grad_kappa2_Y(ii,jj) = (kappa2(ii,jp) - kappa2(ii,jm))/(2d0*H);
    enddo
  enddo

  scal_mat=grad_kappa2_X*grad_psi_X+grad_kappa2_Y*grad_psi_Y
  dyna_mat=dsin(pi*psi)+phi*(ksi-ksi_eq)*(1d0+dcos(pi*psi))
  dpsi_mat=(dXdY+dYdX+kappa2*lap_psi+scal_mat+dyna_mat)*DT/TAU
  ksi=ksi+(D*lap_ksi+(F-ksi/taus)*(1d0-psi)/2d0)*DT-0.5d0*dpsi_mat
  psi=psi+dpsi_mat

  sump=sum(psi)
  write(*,*)istep,sump
  if(sump>=sump_done(isum)) then
    write(filenum,'(i)')istep
    filename='p_'//trim(adjustl(filenum))//'.dat'
    call output(filename,NY,NX,psi)
    filename='c_'//trim(adjustl(filenum))//'.dat'
    call output(filename,NY,NX,ksi)
    isum=isum+1
    if(isum>size(sump_done))exit
  endif
    
enddo

write(*,*)'Program end'

end program main
