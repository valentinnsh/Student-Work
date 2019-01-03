!
! Copyright (c) 2018 V.Shishkin
!

module qudratura
contains
  function rectan(f,a,b,n) result(res)
    integer :: n
    real*8  :: a,b,res, delta, tmpa
    real*8, external :: f

    res = 0
    delta = (b-a)/n
    tmpa = a
    !В задаче delta обозначается как h. delta - шаг дискретизаци
    do while(tmpa .lt. b)
       res = res + f(tmpa-0.5*delta)
       tmpa = tmpa + delta
    end do
    res = res*delta
  end function rectan

  function trap(f,a,b,n) result(res)
    integer :: n
    real*8  :: a,b,res, delta, tmpa
    real*8, external :: f

    res = (f(a) + f(b))/2
    delta = (b-a)/n
    tmpa = a
    !В задаче delta обозначается как h. delta - шаг дискретизаци
    do while(tmpa .lt. b)
       res = res + f(tmpa)
       tmpa = tmpa + delta
    end do
    res = res*delta
  end function trap

  function sym(f,a,b,n) result(res)
    integer :: n, count
    real*8  :: a,b,res, delta, tmpa
    real*8, external :: f
    delta = (b-a)/n
    if (mod(n,2) .ne. 0) then
       write(*,*) 'Error: n have to be even'
       res = -1
    else
       res = f(a) + f(b)
       count  = 2
       tmpa = a + delta

       do while(tmpa .lt. b)
          if(count .eq. 2) then
             res = res + f(tmpa)*4
             count = 1
             tmpa = tmpa + delta
          else
             res = res + f(tmpa)*2
             count = 2
             tmpa = tmpa + delta
          end if
       end do
       res = res*delta/3
    end if
  end function sym



  function quadra(fqua,f,a,b,n) result(res)
    integer :: n
    real*8  :: a,b,res
    real*8, external :: f, fqua
    res = fqua(f,a,b,n)
  end function quadra


  !Процедура выполняет метод двойного пересчета для Задачи №6
  subroutine quadrauto(fqua,f,a,b,eps,n,k,ier,res)
    integer :: n, ier, k
    real*8 :: a,b,res, eps, last_res, tmp_res
    real*8 :: eps_ier
    real*8, external :: f, fqua

    k = 0
    tmp_res = 0
    eps_ier = 1e-13_8
    res = fqua(f,a,b,n); last_res = res + 1.0_8
    do while(.TRUE.)
       k = k + 1
       last_res = res
       res = fqua(f,a,b,n*2**k)
       if(abs(res-last_res) .le. eps) then
          ier = 0
          return
       end if
       if(abs(tmp_res - abs(res-last_res)) .lt. eps) then
          ier = 1
          return
       end if
       tmp_res =  abs(res-last_res)
    end do
  end subroutine quadrauto
end module qudratura
