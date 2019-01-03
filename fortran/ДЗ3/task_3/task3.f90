subroutine str(forw,back)
implicit none
    integer i
    character(26) forw,back
do i=1,26
    forw(i:i)=char(ichar('a')+i-1)
    back(i:i)=char(ichar('z')-(i-1))
enddo
end subroutine

program alphabet
  implicit none

  character(26) forw,back
  call str(forw,back)
  write(*,*) 'A-Z'
  write(*,*) forw
  write(*,*) 'Z-A'
  write(*,*) back
end program
