program day06b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day06b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day06_example.txt')
    result = solve('inputfiles/day06_input.txt')
    call printresultline_int64('06b', result)
end program day06b_main
