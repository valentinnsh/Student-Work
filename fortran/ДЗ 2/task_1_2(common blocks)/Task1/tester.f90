module my_prec
  implicit none
  integer, parameter :: mp=16
end module my_prec

module poly
contains
  function mypolynom(x) result(res)
    use my_prec
    real(mp) :: res,x
    res = x**2 - x +2
  end function mypolynom
end module poly

function rectan(f,a,b,n) result(res)  !Формула средних прямоугольников
  use my_prec
  implicit none

  include 'parameters'
  integer :: n
  real(mp)  :: a,b,res, delta, tmpa
  real(mp), external :: f
  res = 0
  delta = (b-a)/n    !В задаче delta обозначается как h. delta - шаг дискретизаци
  tmpa = a
  do while(tmpa .lt. b)
     res = res + f(tmpa-0.5*delta) + 2*q/p
     tmpa = tmpa + delta
  end do
  res = res*delta
end function rectan

function trap(f,a,b,n) result(res)
  use my_prec
  implicit none

  integer :: n
  real(mp)  :: a,b,res, delta, tmpa
  real(mp), external :: f
  include 'parameters'
    
  res = (f(a) + f(b) + 4*q/p)/2
  delta = (b-a)/n
  tmpa = a
  do while(tmpa .lt. b)
     res = res + f(tmpa) + 2*q/p
     tmpa = tmpa + delta
  end do
  res = res*delta
end function trap

function sym(f,a,b,n) result(res)
  use my_prec
  include 'parameters'
  
  integer :: n, count
  real(mp)  :: a,b,res, delta, tmpa
  real(mp), external :: f
  delta = (b-a)/n
  if (mod(n,2) .ne. 0) then
     write(*,*) 'Error: n have to be even'
     res = -1
  else
     res = f(a) + f(b) + 4*q/p
     count  = 2
     tmpa = a + delta
     
     do while(tmpa .lt. b)
        if(count .eq. 2) then
           res = res + f(tmpa)*4 + 8*q/p
           count = 1
           tmpa = tmpa + delta
        else
           res = res + f(tmpa)*2 + 4*q/p
           count = 2
           tmpa = tmpa + delta
        end if
     end do
     res = res*delta/3
  end if
end function sym

!---------------------TESTING----------------------------------
program tester_quadro
  use poly
  use my_prec
  interface
     function rectan(f,a,b,n) result(res)
       use my_prec
       implicit none
       include 'parameters'
       integer :: n
       real(mp)  :: a,b,res, delta, tmpa
       real(mp), external :: f
     end function rectan
     function trap(f,a,b,n) result(res)
       use my_prec
       implicit none
       integer :: n
       real(mp)  :: a,b,res, delta, tmpa
       real(mp), external :: f
       include 'parameters'
     end function trap
     function sym(f,a,b,n) result(res)
       use my_prec
       include 'parameters'
       integer :: n, count
       real(mp)  :: a,b,res, delta, tmpa
       real(mp), external :: f
     end function sym
  end interface
  integer :: n
  real(mp) :: a,b, start, finish, rect_time, trap_time, sym_time
  real(mp) :: res_rect, res_trap, res_sym
  include 'parameters'
  read(*,*) n, a, b, p, q

  !Тестируем на полиноме x^2-x+2. полином задан выше, в модуле poly
  call cpu_time(start)
  res_rect = rectan(mypolynom,a,b,n)
  call cpu_time(finish)
  rect_time = finish - start

  call cpu_time(start)
  res_trap = trap(mypolynom,a,b,n)
  call cpu_time(finish)
  trap_time = finish - start

  call cpu_time(start)
  res_sym = sym(mypolynom,a,b,n)
  call cpu_time(finish)
  sym_time = finish - start

  write(*,*) 'trap_time/rect_time = ', trap_time/rect_time, ' sym_time/rect_time', sym_time/rect_time
  ![Задача №2] Проверяем работоспособность функции quadro
  !write(*,*) 'quadra(sym,mypolynom,a,b,n) = ', quadra(sym,mypolynom,a,b,n)
end program tester_quadro



