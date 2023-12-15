!> Solution for https://adventofcode.com/2023/day/15 part a
module day15a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asline
    implicit none
    private

    integer, parameter :: code_comma = iachar(',')

    public :: solve

contains

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line
        integer(int64)                :: total_sum
        integer                       :: i, hash, code

        line = readinputfile_asline(filename)

        total_sum = 0
        hash = 0
        do i = 1, len(line)
            code = iachar(line(i:i))
            if (code == code_comma) then
                total_sum = total_sum + hash
                hash = 0
                cycle
            end if
            hash = hash + code
            hash = hash * 17
            hash = modulo(hash, 256)
        end do
        if (hash > 0) then
            ! also for the last character:
            total_sum = total_sum + hash
        end if

        solve = total_sum
    end function

end module day15a
