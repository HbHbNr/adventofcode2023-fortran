program day13a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day13a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day13_example.txt')
    result = solve('inputfiles/day13_input.txt')
    call printresultline_int64('13a', result)
end program day13a_main
