program day14a_main
    use iso_fortran_env, only : int64
    use util, only : printresultline_int64
    use day14a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day14_example.txt')
    result = solve('inputfiles/day14_input.txt')
    call printresultline_int64('14a', result)
end program day14a_main
