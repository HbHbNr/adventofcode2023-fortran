program day15a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day15a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day15_example.txt')
    result = solve('inputfiles/day15_input.txt')
    call printresultline_int64('15a', result)
end program day15a_main
