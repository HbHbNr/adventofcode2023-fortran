program day01a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day01a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day01_example.txt')
    result = solve('inputfiles/day01_input.txt')
    call printresultline_int64('01a', result)
end program day01a_main
