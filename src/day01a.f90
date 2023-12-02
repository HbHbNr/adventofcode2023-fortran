!> Solution for https://adventofcode.com/2023/day/1 part a
module day01a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 60
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')

    public :: solve

contains

    function sumnumbers(lines) result(calibration_value)
        implicit none
        character(len=*), intent(in)  :: lines(:)
        integer                       :: calibration_value
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

    end function sumnumbers

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: calibration_value

        lines = readinputfile_asstringarray(filename, maxlinelength)
        calibration_value = sumnumbers(lines)

        solve = calibration_value
    end function

end module day01a
