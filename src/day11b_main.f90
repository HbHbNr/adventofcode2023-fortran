program day11b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day11b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day11_example.txt')
    result = solve('inputfiles/day11_input.txt')
    call printresultline_int64('11b', result)
end program day11b_main
