!> Solution for https://adventofcode.com/2023/day/18 part a
module day18a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 60

    public :: solve

contains

    function sumnumbers(lines) result(calibration_value)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer(int64)                :: calibration_value
        character(len=:), allocatable :: line
        integer                       :: i, j, digit

        calibration_value = 0
        do i = 1, size(lines)
            line = lines(i)
            do j = 1, len_trim(line)
                if (iachar(line(j:j)) >= code_0) then
                    if (iachar(line(j:j)) <= code_9) then
                        digit = (iachar(line(j:j)) - code_0) * 10
                        exit
                    end if
                end if
            end do
            do j = len_trim(line), 1, -1
                if (iachar(line(j:j)) >= code_0) then
                    if (iachar(line(j:j)) <= code_9) then
                        digit = digit + (iachar(line(j:j)) - code_0)
                        exit
                    end if
                end if
            end do
            calibration_value = calibration_value + digit
        end do
    end function

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: calibration_value

        lines = readinputfile_asstringarray(filename, maxlinelength)
        calibration_value = sumnumbers(lines)

        ! solve = calibration_value
        solve = -1
    end function

end module day18a
