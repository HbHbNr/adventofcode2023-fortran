program day16a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day16a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day16_example.txt')
    result = solve('inputfiles/day16_input.txt')
    call printresultline_int64('16a', result)
end program day16a_main
