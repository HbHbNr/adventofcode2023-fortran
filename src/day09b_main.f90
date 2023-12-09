program day09b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day09b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day09_example.txt')
    result = solve('inputfiles/day09_input.txt')
    call printresultline_int64('09b', result)
end program day09b_main
