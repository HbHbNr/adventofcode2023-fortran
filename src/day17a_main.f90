program day17a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day17a, only : solve
    implicit none

    integer(int64) :: result

    result = solve('inputfiles/day17_example.txt')
    ! result = solve('inputfiles/day17_input.txt')
    call printresultline_int64('17a', result)
end program day17a_main
