program day12b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day12b, only : solve
    implicit none

    integer(int64) :: result

    result = solve('inputfiles/day12_example.txt')
    ! result = solve('inputfiles/day12_input.txt')
    call printresultline_int64('12b', result)
end program day12b_main
