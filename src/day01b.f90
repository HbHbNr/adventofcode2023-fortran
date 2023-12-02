!> Solution for https://adventofcode.com/2023/day/1 part b
module day01b
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 60
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')

    public :: solve

contains

    function finddigit(line, from, to, step) result(digit)
        implicit none

        character(len=*), intent(in) :: line
        integer, intent(in)          :: from, to, step
        integer                      :: digit
        character(len=5), parameter  :: words(9) = ['one  ', 'two  ', 'three', 'four ', 'five ', &
                                                    'six  ', 'seven', 'eight', 'nine ']
        character(len=5)             :: word
        integer                      :: j, jw, w

        jloop: do j = from, to, step
            if (iachar(line(j:j)) >= code_0) then
                if (iachar(line(j:j)) <= code_9) then
                    digit = iachar(line(j:j)) - code_0
                    exit jloop
                end if
            end if
            do w = 1, size(words)
                word = words(w)
                jw = min(j+len_trim(word)-1, len_trim(line))
                if (line(j:jw) == trim(word)) then
                    digit = w
                    exit jloop
                end if
            end do
        end do jloop
    end function

    function sumnumbers(lines) result(calibration_value)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer                       :: calibration_value
        character(len=:), allocatable :: line
        integer                       :: i, digit1, digit2

        calibration_value = 0
        do i = 1, size(lines)
            line = lines(i)
            ! print *, line
            digit1 = finddigit(line, 1, len_trim(line), 1)
            digit2 = finddigit(line, len_trim(line), 1, -1)
            calibration_value = calibration_value + digit1 * 10 + digit2
            ! print *, digit1 * 10 + digit2, calibration_value
        end do
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: calibration_value

        lines = readinputfile_asstringarray(filename, maxlinelength)
        calibration_value = sumnumbers(lines)

        solve = calibration_value
    end function

end module day01b
