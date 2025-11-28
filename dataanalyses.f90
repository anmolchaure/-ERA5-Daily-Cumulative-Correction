module dataanalyes
    use types
    implicit none

    public :: diff1, correct_era5_daily_cum

    contains



! era5 gives us data in the form of
! cummulative result (total accumulation over the day), but we want hourly rainfall
            ! Example :  11am -> 40mm
            !            12 am -> 20mm + 40 mm(from
            !                  previous hour) = 60mm
            ! therefore, tp = [0mm, 40mm, 60mm, 70mm] instead of hourly rainfall tp = [0mm, 40mm, 20mm, 10mm], so we use this function to calculate the hourly rainfall

! computes the hourly differences from the cumulative
    subroutine diff1(x, dx)
        real(kind=rkind), dimension(:), intent(in) :: x
        real(kind=rkind), dimension(:), intent(out) :: dx
        integer(kind=ikind) :: n, i

        n = ubound(x,1)

        if (n <= 0) return
        dx(1) = x(1)

        do i = 2, n
                dx(i) = x(i) - x(i-1)
        end do
    end subroutine diff1

! corrects the era5 data by calculating hourly increments from the given cumulative in the input data

    subroutine correct_era5_daily_cum(tp_cum, e_cum, tp_inc, e_inc)
        real(kind=rkind), dimension(:), intent(in) :: tp_cum, e_cum
        real(kind=rkind), dimension(:), intent(out) :: tp_inc, e_inc
        integer(kind=ikind) :: n,i,hrs
        real(kind=rkind), dimension(:), allocatable :: tp_diff, e_diff

        n = ubound(tp_cum,1)
        allocate(tp_diff(n), e_diff(n))

        ! if the array is empty, exit
        if(n == 0) return
! call the difference subroutine

        call diff1(tp_cum,tp_diff)
        call diff1(e_cum, e_diff)


! initialise

        hrs = 1 ! to keep count of hours so that we can reset at 24th step
        tp_inc = 0.0_rkind
        e_inc = 0.0_rkind

        do i = 1,n
            if (hrs == 1) then
                tp_inc(i) = tp_cum(i)
                e_inc(i) = e_cum(i)
            ! if hrs is 1 we set the cummulative directly as the cumulative resets at midnight i.e, 00:00
            else
            ! otherwise calculate the difference
                tp_inc(i) = tp_diff(i)
                e_inc(i) = e_diff(i)
            end if


            ! cliping data as percipitation cannot be negative
            if (tp_inc(i) < 0.0_rkind) then
                tp_inc(i) = 0.0_rkind
            end if

            ! cliping data as ERA5 uses negative sign, so evaporation cannot be positive
            if (e_inc(i) > 0.0_rkind) then
                e_inc(i) = 0.0_rkind
            end if


            ! increment the hour counter and move to next sample
            if (hrs == 24) then
                hrs = 1
            else
                hrs = hrs + 1
            end if

        end do


        deallocate(tp_diff,e_diff)

    end subroutine correct_era5_daily_cum

end module dataanalyes









