program day16b_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day16b, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day16_example.txt')
    result = solve('inputfiles/day16_input.txt')
    call printresultline_int64('16b', result)
end program day16b_main
