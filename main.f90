program main
    use types
    use dataanalyes
    implicit none


    integer(kind=rkind) :: i, n, io_status, id
    real(kind=rkind), dimension(:), allocatable :: tp_cum, e_cum, tp_inc, e_inc
    character(len=200) :: ts
    real(kind=rkind) :: temp_tp, temp_e

! open the file and check if opened
    n = 0
    open(newunit=id, file="era5_input.txt", status="old", action='read',iostat=io_status)
    if (io_status /= 0) then
        print*, "file was not opened"
        stop
    end if

    ! if file opened succesfully count the number of lines(records) in the file

    do
        read(id, *, iostat=io_status) ts
        if (io_status /= 0) exit
        n = n + 1
    end do
    close(id)

    print*, "number of lines in file", n

! to verify the file was not empty

    if (n<=0) then
        print*, "nothing inside rain-evap-era5.txt"
        stop 0
    end if


! allocate arrays to match the size of the number of records (no. of lines) found in the file
    allocate(tp_cum(n), e_cum(n), tp_inc(n), e_inc(n))


! open the file again
! opening it once and using it creates error, therefore opening it again and check if it opened again

    open(newunit=id, file="era5_input.txt", status="old", action='read',iostat=io_status)
     if (io_status /= 0) then
        print*, "file was not opened"
        stop
    end if

    ! if the file opened assign the cummulative records in the file to temporary variables
    do i =1,n
        read(id, *, iostat=io_status) ts, temp_tp, temp_e
        if (io_status == 0) then
            tp_cum(i) = temp_tp
            e_cum(i)  = temp_e
        else
            ! If read failed, reuse previous values (or zero if first record)
            if (i > 1) then
                tp_cum(i) = tp_cum(i-1)
                e_cum(i)  = e_cum(i-1)
            else
                tp_cum(i) = 0.0
                e_cum(i)  = 0.0
            end if
        end if
    end do
    close(id)


! call subroutine to make corrections on the input data

    call correct_era5_daily_cum(tp_cum, e_cum, tp_inc, e_inc)

    ! print the first corrected valuess to check
    print *, "first 10 corrected hourly values:"
    print *, "idx    tp_cum     tp_inc     e_cum     e_inc"
    do i = 1, min(10, n)
        write(*,'(I4, 4F10.4)') i, tp_cum(i), tp_inc(i), e_cum(i), e_inc(i)
    end do


! write the hourly incremented values to the era5_corrected file

    open(newunit=id, file='era5_corrected.txt', status='replace', action='write', iostat=io_status)
    ! check if file exists and opens
    if (io_status /= 0) then
        print *, "ERROR: cannot open era5_corrected.txt for writing"
        stop 1
    end if
    ! write the values of hourly precipitation and evaporation using the do loop
    do i = 1, n
        write(id, '(2F14.6)') tp_inc(i), e_inc(i)
    end do
    close(id)

    print *, "the data is saved to era5_corrected.txt"




    deallocate(tp_cum, e_cum, tp_inc, e_inc)



end program main
