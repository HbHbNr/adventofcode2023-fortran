program day09a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day09a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day09_example.txt')
    result = solve('inputfiles/day09_input.txt')
    call printresultline_int64('09a', result)
end program day09a_main
