program day10b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day10b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day10_example.txt')
    ! result = solve('inputfiles/day10_example2.txt')
    ! result = solve('inputfiles/day10_example3.txt')
    ! result = solve('inputfiles/day10_example4.txt')
    ! result = solve('inputfiles/day10_example5.txt')
    result = solve('inputfiles/day10_input.txt')
    call printresultline_int64('10b', result)
end program day10b_main
