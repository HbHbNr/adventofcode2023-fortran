!> Solution for https://adventofcode.com/2023/day/13 part a
module day13a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 17

    public :: solve

contains

    function create_map(lines) result(map)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer                      :: row, col
        logical, allocatable         :: map(:,:)

        allocate(map(size(lines), len_trim(lines(1))))
        map = .false.

        do row = 1, size(map, 1)
            do col = 1, size(map, 2)
                if (lines(row)(col:col) == '#') map(row, col) = .true.
            end do
        end do
    end function

    subroutine print_map(map)
        implicit none

        logical, intent(in) :: map(:,:)
        integer             :: row

        do row = 1, size(map, 1)
            print *, map(row, :)
        end do
    end subroutine

    function find_reflected_left_lines(map) result(summary)
        implicit none

        logical, intent(in) :: map(:,:)
        integer             :: summary
        integer             :: row, left, width, matchable

        width = size(map, 2)
        summary = 0
        leftloop: do left = 1, width - 1
            ! if the reflection line is not exactly in the middle, the smaller part
            ! defines how many columns could be matched
            matchable = min(left, width - left)
            ! print *, 'left:', left, ' matchable:', matchable
            do row = 1, size(map, 1)
                if (ANY(map(row,left:(left-matchable+1):-1) &
                        .neqv. &
                        map(row,left+1:left+matchable))) cycle leftloop
                ! print *, map(row,left:(left-matchable+1):-1), '|', map(row,left+1:left+matchable)
            end do
            ! print *, '******** reflection found, left:', left
            summary = left
            exit
        end do leftloop
    end function

    function find_reflected_top_lines(map) result(summary)
        implicit none

        logical, intent(in) :: map(:,:)
        integer             :: summary
        integer             :: col, top, height, matchable

        height = size(map, 1)
        summary = 0
        toploop: do top = 1, height - 1
            ! if the reflection line is not exactly in the middle, the smaller part
            ! defines how many columns could be matched
            matchable = min(top, height - top)
            ! print *, 'top:', top, ' matchable:', matchable
            do col = 1, size(map, 2)
                if (ANY(map(top:(top-matchable+1):-1,col) &
                        .neqv. &
                        map(top+1:top+matchable,col))) cycle toploop
                ! print *, map(top:(top-matchable+1):-1,col), '|', map(top+1:top+matchable,col)
            end do
            ! print *, '******** reflection found, top:', top
            summary = top
            exit
        end do toploop
    end function

    function find_reflection(lines) result(summary)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer(int64)               :: summary
        ! integer                      :: i, j, digit
        logical, allocatable         :: map(:,:)
        logical                      :: vertikal_line_reflection
        integer                      :: reflected_lines

        map = create_map(lines)

        reflected_lines = find_reflected_left_lines(map)
        if (reflected_lines > 0) then
            summary = reflected_lines
        else
            reflected_lines = find_reflected_top_lines(map)
            summary = reflected_lines * 100
        end if
    end function

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:), block(:)
        integer(int64)                :: summary
        integer                       :: i, first, last

        lines = readinputfile_asstringarray(filename, maxlinelength)

        ! iterate over patterns 
        summary = 0
        first = 1
        do while (first < size(lines))
            do i = first, size(lines)
                if (lines(i) == '') then
                    exit
                else
                    last = i
                end if
            end do
            ! print *, first, last
            block = lines(first:last)

            ! find reflection and add value to summary
            summary = summary + find_reflection(block)

            first = last + 2
        end do

        solve = summary
        ! solve = -1
    end function

end module day13a
