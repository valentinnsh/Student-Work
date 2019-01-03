!
! Copyright (c) 2018 V.Shishkin
!


program tester
  use Function
  implicit none
  integer :: i
  real*8 :: a,b,n, step
  !program starts here
  i = 0
  open (unit=99, file='input', status='old', action='read')
  do  while(i .le. 2)
     read(99,*) a,b,n
     i = i+1
     write(*,*) 'N#', i, '  a=', a, ' b=', b, ' n=', n
     step = (b-a)/n
     do while(a .le. b)
        write(*,*) 'f(', a, ') = ', f(a)
        a = a + step
     end do
  end do
  close(99)
end program tester
