program day21b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day21b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day21_example.txt', [6, 50, 100])
    result = solve('inputfiles/day21_example.txt', [6, 6, 6])
    ! result = solve('inputfiles/day21_input.txt', [65, 65 + 131, 65 + 131 + 131])
    call printresultline_int64('21b', result)
end program day21b_main
