!
! Copyright (c) 2018 V.Shishkin
!


module Function
contains
  function f(x) result(res)
    real*8 :: tmp_x4, x, res
    !В формуле приходится несколько раз вычислять х в 4 степени
    !Раз так, посчитаем один раз и сохранима значение в памяти
    tmp_x4 = x**4

    res = 2187.0_8/364.0_8/x**12*(exp(-tmp_x4) - exp(-tmp_x4/9.0_8) + 8_8/9.0_8*tmp_x4*(1.0_8-5.0_8/9_8*tmp_x4))
  end function f
end module Function
