subroutine output(filename,nx,ny,f)
implicit none
character*200,intent(in)::filename
integer,intent(in)::nx,ny
real*8,intent(in)::f(nx,ny)
integer::ii,jj
open(10,file=trim(adjustl(filename)))
do jj=1,ny
  do ii=1,nx
    write(10,"(1x,f100.5,\)")f(ii,jj)
  enddo
  write(10,"(1x,F100.5,/)")
enddo
close(10)
end subroutine output
