program day10a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day10a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day10_example.txt')
    ! result = solve('inputfiles/day10_example2.txt')
    result = solve('inputfiles/day10_input.txt')
    call printresultline_int64('10a', result)
end program day10a_main
