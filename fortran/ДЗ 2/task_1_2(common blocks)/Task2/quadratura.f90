module quadratura
contains

! Задача №1

  function rectan(f,a,b,n) result(res)  !Формула средних прямоугольников
    use my_prec

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

! Задача №2

  function quadra(fqua,f,a,b,n) result(res)
    use my_prec
    include 'parameters'

    integer :: n
    real(mp)  :: a,b,res
    real(mp), external :: f, fqua
    res = fqua(f,a,b,n)
  end function quadra


  !Процедура выполняет метод двойного пересчета для Задачи №6
  subroutine quadrauto(fqua,f,a,b,eps,n,k,ier,res)
    use my_prec

    integer :: n, ier, k
    real(mp) :: a,b,res, eps, last_res, tmp_res
    real(mp) :: eps_ier
    real(mp), external :: f, fqua
    k = 0
    tmp_res = 0
    eps_ier = 1e-13
    res = fqua(f,a,b,n); last_res = res + 1.0
    do while(.TRUE.)
       k = k + 1
       last_res = res
       res = fqua(f,a,b,n*2**k)
       if(abs(res-last_res) .lt. eps) then
          ier = 0
          return
       end if
       if(abs(tmp_res - abs(res-last_res)) .lt. eps) then
          ier = 1
          return
       end if
       write(*,*) abs(tmp_res - abs(res-last_res))
       tmp_res =  abs(res-last_res)
    end do
  end subroutine quadrauto
end module quadratura
