!> Solution for https://adventofcode.com/2023/day/3 part a
module day03a
    use util, only : readinputfile_asstringarray
    implicit none
    private

    integer, parameter :: maxlinelength = 140
    integer, parameter :: code_0 = iachar('0')
    integer, parameter :: code_9 = iachar('9')
    character(len=1), parameter :: empty = '.'

    public :: solve

contains

    function parse_number(lines, row, col) result(number)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer, intent(in)          :: row, col
        integer                      :: number, col_start, col_end, i

        number = 0
        col_start = col
        col_end = col
        ! search to the left
        do i = col-1, 1, -1
            if (iachar(lines(row)(i:i)) < code_0) exit
            if (iachar(lines(row)(i:i)) > code_9) exit
            col_start = i
        end do
        do i = col+1, len(lines(row))
            if (iachar(lines(row)(i:i)) < code_0) exit
            if (iachar(lines(row)(i:i)) > code_9) exit
            col_end = i
        end do
        ! print *, row, col_start, col_end
        read (lines(row)(col_start:col_end), *) number
        ! print *, 'found', number
    end function

    function sum_local_numbers(lines, row, col) result(local_number_sum)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer, intent(in)          :: row, col
        integer                      :: local_number_sum

        local_number_sum = 0
        ! two symbols are never next to each other, so non-empty fields are digits
        if (lines(row-1)(col:col) /= empty) then
            ! middle above
            local_number_sum = local_number_sum + parse_number(lines, row-1, col)
        else
            if (lines(row-1)(col-1:col-1) /= empty) then
                ! left above
                local_number_sum = local_number_sum + parse_number(lines, row-1, col-1)
            endif
            if (lines(row-1)(col+1:col+1) /= empty) then
                ! right above
                local_number_sum = local_number_sum + parse_number(lines, row-1, col+1)
            endif
        end if
        if (lines(row)(col-1:col-1) /= empty) then
            ! left side
            local_number_sum = local_number_sum + parse_number(lines, row, col-1)
        end if
        if (lines(row)(col+1:col+1) /= empty) then
            ! right side
            local_number_sum = local_number_sum + parse_number(lines, row, col+1)
        end if
        if (lines(row+1)(col:col) /= empty) then
            ! middle below
            local_number_sum = local_number_sum + parse_number(lines, row+1, col)
        else
            if (lines(row+1)(col-1:col-1) /= empty) then
                ! left below
                local_number_sum = local_number_sum + parse_number(lines, row+1, col-1)
            endif
            if (lines(row+1)(col+1:col+1) /= empty) then
                ! right below
                local_number_sum = local_number_sum + parse_number(lines, row+1, col+1)
            endif
        end if
    end function

    function sum_numbers(lines) result(number_sum)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer                      :: number_sum
        integer                      :: row, column, digit

        number_sum = 0
        ! no symbols in first or last line
        do row = 2, size(lines) - 1
            ! no symbols in first or last column
            do column = 2, len(lines(row)) - 1
                if (lines(row)(column:column) /= empty) then
                    if (iachar(lines(row)(column:column)) < code_0 .or. &
                        iachar(lines(row)(column:column)) > code_9) then
                        ! symbol found
                        number_sum = number_sum + sum_local_numbers(lines, row, column)
                    end if
                end if
            end do
        end do

    end function sum_numbers

    integer function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: number_sum

        lines = readinputfile_asstringarray(filename, maxlinelength)
        number_sum = sum_numbers(lines)

        solve = number_sum
    end function

end module day03a
