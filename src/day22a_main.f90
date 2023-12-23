program day22a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day22a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day22_example.txt', 2)
    result = solve('inputfiles/day22_input.txt', 9)
    call printresultline_int64('22a', result)
end program day22a_main
