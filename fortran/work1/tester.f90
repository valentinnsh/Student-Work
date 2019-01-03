!
! Copyright (c) 2018 V.Shishkin
!



module poly
contains
  function mypolynom(x) result(res)
    real*8 :: res,x
    res = x**2 - x +2
  end function mypolynom
end module poly


!---------------------TESTING----------------------------------
program tester_quadro
  use qudratura
  use poly
  implicit none

  integer :: n
  real*8 :: a,b
  write(*,*) 'Enter n, left and right corners of [a,b]'
  read(*,*) n, a, b

  ![Задача №1]тестируем на полиноме x^2-x+2. полином задан ниже, в модуле poly
  write(*,*) 'My polynomium function f(x)=x^2-x+2'
  write(*,*) '1). S(rectan) = ', rectan(mypolynom,a,b,n)
  write(*,*) '2). S(trap) = ', trap(mypolynom,a,b,n)
  write(*,*) '3). S(sym) = ', sym(mypolynom,a,b,n)

  ![Задача №2] Проверяем работоспособность функции quadro
  write(*,*) 'quadra(sym,mypolynom,a,b,n) = ', quadra(sym,mypolynom,a,b,n)
end program tester_quadro



