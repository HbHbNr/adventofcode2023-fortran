program day24a_main
    use iso_fortran_env, only : int64, real128
    use util, only : printresultline_int64
    use day24a, only : solve
    implicit none

    integer(int64) :: result

    ! result = solve('inputfiles/day24_example.txt', 7.0_real128, 27.0_real128)
    result = solve('inputfiles/day24_input.txt', 200000000000000.0_real128, 400000000000000.0_real128)
    call printresultline_int64('24a', result)
end program day24a_main
