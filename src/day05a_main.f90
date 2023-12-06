program day05a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day05a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day05_example.txt')
    result = solve('inputfiles/day05_input.txt')
    call printresultline_int64('05a', result)
end program day05a_main
