program day20a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day20a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day20_example1.txt')
    ! result = solve('inputfiles/day20_example2.txt')
    result = solve('inputfiles/day20_input.txt')
    call printresultline_int64('20a', result)
end program day20a_main
