!> Solution for https://adventofcode.com/2023/day/9 part a
module day09a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, string_extract_int64s
    implicit none
    private

    integer, parameter :: maxlinelength = 121

    public :: solve

contains

    function sum_extrapolated_values(lines) result(extrapolated_sum)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer(int64)                :: extrapolated_sum
        ! character(len=:), allocatable :: line
        integer(int64), allocatable   :: values(:), grid(:,:)
        integer(int64)                :: diff
        integer                       :: valuecount, i, j, k
        logical                       :: allzeroes

        ! allocate array for single line and full interpolation grid
        call string_extract_int64s(lines(1), values)
        valuecount = size(values)
        ! allocate one extra number for the topmost line to add the interpolated value
        allocate(grid(valuecount + 1, valuecount))

        ! extrapolate all lines
        extrapolated_sum = 0
        do i = 1, size(lines)
            ! clear grid
            grid = 0
            read (lines(i), *) grid(1:valuecount, 1)
            ! print *, grid(:, 1)

            ! interpolate down until all numbers are zeros
            do j = 2, valuecount
                allzeroes = .true.
                do k = 1, valuecount - j + 1
                    diff = grid(k+1, j-1) - grid(k, j-1)
                    if (diff /= 0) allzeroes = .false.
                    grid(k, j) = diff
                end do
                ! print *, grid(1:valuecount - j + 1, j)
                if (allzeroes) exit
            end do
            ! interpolate up until the first line
            do while (j > 1)
                grid(valuecount - j + 3, j - 1) = grid(valuecount - j + 2, j) + grid(valuecount - j + 2, j - 1)
                ! print *, grid(:, j - 1)
                j = j - 1
            end do
            extrapolated_sum = extrapolated_sum + grid(valuecount + 1, 1)
        end do
    end function sum_extrapolated_values

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: extrapolated_sum

        lines = readinputfile_asstringarray(filename, maxlinelength)
        extrapolated_sum = sum_extrapolated_values(lines)

        solve = extrapolated_sum
    end function

end module day09a
