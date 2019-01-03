!
! Copyright (c) 2018 V.Shishkin
!
module horse_stuff
  type horse_info
     integer, allocatable :: horse_x(:)
     integer, allocatable :: horse_y(:)
     integer :: num
  end type horse_info
contains
  function dist_betwing(x0,y0,x,y,N) result(res)
    integer x0,y0,x,y,res,max,min,tmp,tmax,tmin,turn1,turn2

    max = abs(x-x0)
    min = abs(y-y0)
    if(max .lt. min) then
       tmp = max; max = min; min = tmp
    end if
    if((max.eq.2).and.(min.eq.2)) then; res = 4; return
    end if
    if(max.eq.1) then
       if(min.eq.0) then; res = 3; return;
       else
          if(((x0.eq.0).or.(x0.eq.(N-1))).and.((y0.eq.0).or.(y0.eq.(N-1)))) then
             res = 4; return
          end if
          if(((x1.eq.0).or.(x1.eq.(N-1))).and.((y1.eq.0).or.(y1.eq.(N-1)))) then
             res = 4; return
          end if
       end if
    end if

    tmax = max; tmin = min
    if(2*min.lt.max) then
       res = max/2;
       max = mod(max,2)
       min = mod((min+res),2)
    else
       turn1 = (2*max-min)/3; turn2 = (2*min-max)/3;
       res = turn1+turn2
       max = max-2*turn1-turn2; min = min-2*turn2-turn1;
    end if

    if((max.gt.0).or.(min.gt.0)) then
       if(mod(res,2).gt.0) then; res=res+1+mod((tmax+tmin),2);
       else
          res=res+1+mod((tmax+tmin-1),2);
       end if
    end if
    return
  end function dist_betwing

  subroutine find_cells(beg,end,info,N,min)
    integer i,j,k,beg,end, N, max,min, tmp, cell_num
    integer :: cells(1000,2)
    type(horse_info) :: info
    i = beg
    do while(i .lt. end)
       j = 1
       do while(j .le. N)
          max = 0
          k = 1
          do while(k .le. info%num)
             tmp = dist_betwing(info%horse_x(k),info%horse_y(k), i,j,N)
             if(tmp .gt. max) then; max = tmp;
             end if
             k = k+1
          end do
          if(max.lt.min) then
             min = max
             cell_num = 1
             cells(1,1) = i
             cells(1,2) = j
          else
             if(max.eq.min) then
                cell_num = cell_num+1
                cells(cell_num,1) = i
                cells(cell_num,2) = j
             end if
          end if
          j = j+1
       end do
       i = i + 1
    end do
    i = 1
    write(*,*) 'Итого: ',min, 'шагов'
    do while(i .le. cell_num)
       write(*,*) i, ' ячейка: [', cells(i,1), ', ', cells(i,2), ']'
       i = i+1
    end do
  end subroutine find_cells

end module horse_stuff


program chess_knights
  use horse_stuff
  implicit none
  real start, finish
  !integer, allocatable :: board(:,:)
  integer i,j, N, horse_num, min
  type(horse_info) :: local_info
  real mid, check(10)
  min = 10000
  open (unit=99, file='input', status='old', action='read')

  read(99,*) N, horse_num
  !allocate(board(N,N))
  local_info%num = horse_num
  allocate(local_info%horse_x(horse_num))
  allocate(local_info%horse_y(horse_num))
  read(99,*) local_info%horse_x
  read(99,*) local_info%horse_y

  write(*,*) local_info%horse_x
  write(*,*) local_info%horse_y

  i = 1
  do while(i .le. 10)
     min = 10000
     call cpu_time(start)
     call find_cells(0,N,local_info, N, min)
     call cpu_time(finish)

     write(*,*) 'Заняло времени:'
     check(i) = finish - start
     write(*, "(f10.6)") finish - start
     i = i + 1
  end do

  i = 1
  mid = 0
  do while(i .le. 10)
     write(*,*) check(i)
     mid = mid + check(i)
     i = i + 1
  end do
  mid = mid/10.0

  write(*,*) 'Среднее значение = ', mid
  close(99)

end program
