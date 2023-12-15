program day15b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day15b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day15_example.txt')
    result = solve('inputfiles/day15_input.txt')
    call printresultline_int64('15b', result)
end program day15b_main
