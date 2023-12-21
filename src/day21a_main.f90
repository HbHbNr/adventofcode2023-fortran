program day21a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day21a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day21_example.txt', 6)
    result = solve('inputfiles/day21_input.txt', 64)
    call printresultline_int64('21a', result)
end program day21a_main
