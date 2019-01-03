module functions
contains
  subroutine sumstr(a,b,s)
    implicit none

    character (*) s,a,b
    integer i,r
    s='';
    if (len_trim(a)<len_trim(b)) then
       s=a; a=b; b=s; s=''
    endif
    a=adjustr(a); b=adjustr(b);
    i=len(a); r=0;
    do while (index(a(i:i),' ').eq.0 .and. i.ne.0)
       if (chr(a(i:i))+chr(b(i:i))+r<10.and.index(b(i:i),' ').eq.0) then
          s(i:i)=ord(chr(a(i:i))+chr(b(i:i))+r)
          r=0;
       else
          if (index(b(i:i),' ').eq.0) then
             s(i:i)=ord(chr(a(i:i))+chr(b(i:i))+r-10)
             r=(chr(a(i:i))+chr(b(i:i))+r)/10
          endif
       endif
       if (index(b(i:i),' ').ne.0 .and. r.ne.0) then
          if (chr(a(i:i))+r<10) then
             s(i:i)=ord(chr(a(i:i))+r)
             r=0;
          else
             s(i:i)=ord(chr(a(i:i))+r-10)
             r=(chr(a(i:i))+r)/10
          endif
       endif
       if (index(b(i:i),' ').ne.0 .and. r.eq.0) then
          exit
       endif
       i=i-1
    enddo
    if (r.ne.0) then
       s(i:i)=ord(r)
    endif
    s(1:i-1)=a(1:i-1)
    s=adjustl(s)
  end subroutine sumstr

  !int from char
  function chr(s) result(i)
    implicit none
    integer i
    character(1) s
    i=ichar(s)-48
  end function chr

  !char from int
  function ord(i) result(s)
    implicit none
    integer i
    character(1) s
    s=char(ichar('0')+i)
  end function ord
end module functions

program sumchars
  use functions
  implicit none
  character(200) first, second, sum
  read(*,*) first, second
  call sumstr(first, second, sum)
  write(*,*) first, ' + '
  write(*,*) second, ' = '
  write(*,*) sum
end program
