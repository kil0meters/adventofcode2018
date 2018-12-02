program frequencies
    implicit none

    integer, allocatable :: frequencies_arr(:), found_frequencies(:)
    integer              :: i, j, f, total

    open(unit = 1, file = '1.data', status = 'old', action = 'read')
    allocate(frequencies_arr(1021))

    ! I have no idea how to do dynamic memory allocation in fortran
    allocate(found_frequencies(500000))

    read(1,*)frequencies_arr

    total = 0
    f = 0
    do
        do i = lbound(frequencies_arr, 1), ubound(frequencies_arr, 1)
            total = total + frequencies_arr(i)
            found_frequencies(f) = total
            do j = 0, f - 1
                if (total == found_frequencies(j)) then
                    print *,total
                    return
                end if
            end do
            f = f + 1
        end do
    end do
end program
