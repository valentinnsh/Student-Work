!
! Copyright (c) 2018 V.Shishkin
!

!Тестер к Задаче №6
module poly
contains
  function mysin(x) result(res)
    real*8 :: res,x
    res = sin(x)
  end function mysin
end module poly

program autotester
  use qudratura
  use poly
  implicit none

  integer :: n, ier, k
  real*8 :: a,b, eps, res
  write(*,*) 'Enter n, left and right corners of [a,b]'
  read(*,*) n, a, b

  write(*,*) 'Ok, now enter eps'
  read(*,*) eps

  !write(*,*) 'My polynomium function f(x)=x^2-x+2'
  call quadrauto(rectan, mysin,a,b,eps,n,k,ier,res)
  if(ier .eq. 0) then
     write(*,*) '1).  Rectan =  ', res, 'k = ', k
  else
     write(*,*) 'EPS не достигается за разумное число удвоений'
  end if

  k = 0
  call quadrauto(trap, mysin,a,b,eps,n,k,ier,res)
  if(ier .eq. 0) then
     write(*,*) '2).  Trap =  ', res, 'k = ', k
  else
     write(*,*) 'EPS не достигается за разумное число удвоений'
  end if

  k = 0
  call quadrauto(sym, mysin,a,b,eps,n,k,ier,res)
  if(ier .eq. 0) then
     write(*,*) '3).  Sympson =  ', res, 'k = ', k
  else
     write(*,*) 'EPS не достигается за разумное число удвоений'
  end if
end program
