program day22b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day22b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day22_example.txt', 2)
    result = solve('inputfiles/day22_input.txt', 9)
    call printresultline_int64('22b', result)
end program day22b_main
