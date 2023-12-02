!> Solution for https://adventofcode.com/2023/day/2 part b
module day02b
    use regex_module, only : regex
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 170
    integer, parameter :: red_cubes = 12
    integer, parameter :: green_cubes = 13
    integer, parameter :: blue_cubes = 14

    public :: solve

contains

    function power_of_game(orgline) result(power)
        implicit none

        character(len=*), intent(in) :: orgline
        integer                      :: power
        ! type(regex_op)               :: re
        character(len=:), allocatable :: line
        character(len=*), parameter  :: re = '[0-9][0-9]? [rgb]'
        integer                      :: indx, length, amount, red, green, blue
        character(len=1)             :: colour

        line = orgline
        red = 0
        green = 0
        blue = 0

        indx = regex(line, re, length)
        do while (indx > 0)
            if (length == 3) then
                ! amount has only one digit
                read (line(indx:indx), *) amount
                colour = line(indx + 2:indx + 2)
            else
                ! amount has two digits
                read (line(indx:indx + 1), *) amount
                colour = line(indx + 3:indx + 3)
            end if

            select case(colour)
            case ('r')
                red = max(red, amount)
            case ('g')
                green = max(green, amount)
            case ('b')
                blue = max(blue, amount)
            end select

            line = line(indx + length:)
            indx = regex(line, re, length)
        end do
        power = red * green * blue
    end function

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: i, total_power

        lines = readinputfile_asstringarray(filename, maxlinelength)

        total_power = 0
        do i = 1, size(lines)
            total_power = total_power + power_of_game(lines(i))
        end do

        solve = total_power
    end function

end module day02b
