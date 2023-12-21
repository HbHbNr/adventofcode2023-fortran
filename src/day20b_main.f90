program day20b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day20b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day20_example1.txt')
    ! result = solve('inputfiles/day20_example2.txt')
    result = solve('inputfiles/day20_input.txt')
    call printresultline_int64('20b', result)
end program day20b_main
