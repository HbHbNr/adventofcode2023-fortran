program day05b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day05b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day05_example.txt')
    result = solve('inputfiles/day05_input.txt')
    call printresultline_int64('05b', result)
end program day05b_main
