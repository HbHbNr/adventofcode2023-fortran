program day13b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day13b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day13_example.txt')
    result = solve('inputfiles/day13_input.txt')
    call printresultline_int64('13b', result)
end program day13b_main
