!> Solution for https://adventofcode.com/2023/day/13 part b
module day13b
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

        logical, intent(inout) :: map(:,:)
        integer                :: summary
        integer                :: summary1
        integer                :: row, left, width, matchable, fliprow, flipcol

        width = size(map, 2)

        ! try an unflipped run first, because a different solution is required afterwards
        summary1 = 0
        leftloop1: do left = 1, width - 1
            ! if the reflection line is not exactly in the middle, the smaller part
            ! defines how many columns could be matched
            matchable = min(left, width - left)
            do row = 1, size(map, 1)
                if (ANY(map(row,left:(left-matchable+1):-1) &
                        .neqv. &
                        map(row,left+1:left+matchable))) cycle leftloop1
            end do
            summary1 = left
            exit
        end do leftloop1

        ! do the runs with one field flipped and ignore the unflipped run's solution
        summary = 0
        fliprowloop: do fliprow = 1, size(map, 1)
            flipcolloop: do flipcol = 1, size(map, 2)
                ! flip one field
                map(fliprow, flipcol) = .not. map(fliprow, flipcol)

                leftloop: do left = 1, width - 1
                    ! if the reflection line is not exactly in the middle, the smaller part
                    ! defines how many columns could be matched
                    matchable = min(left, width - left)

                    ! speedup: flipcol must be included in the currently checked range, or
                    ! the change would not make any difference
                    if (flipcol < (left-matchable+1)) cycle leftloop
                    if (flipcol > (left+matchable)) cycle leftloop

                    do row = 1, size(map, 1)
                        if (ANY(map(row,left:(left-matchable+1):-1) &
                                .neqv. &
                                map(row,left+1:left+matchable))) cycle leftloop
                    end do

                    ! accept solution only if is different from the unflipped solution
                    if (summary1 /= left) then
                        summary = left
                        exit fliprowloop
                    end if
                end do leftloop

                ! flip field back
                map(fliprow, flipcol) = .not. map(fliprow, flipcol)
            end do flipcolloop
        end do fliprowloop
    end function

    function find_reflected_top_lines(map) result(summary)
        implicit none

        logical, intent(inout) :: map(:,:)
        integer                :: summary
        integer                :: summary1
        integer                :: col, top, height, matchable, fliprow, flipcol

        height = size(map, 1)

        ! try an unflipped run first, because a different solution is required afterwards
        summary1 = 0
        toploop1: do top = 1, height - 1
            ! if the reflection line is not exactly in the middle, the smaller part
            ! defines how many columns could be matched
            matchable = min(top, height - top)
            do col = 1, size(map, 2)
                if (ANY(map(top:(top-matchable+1):-1,col) &
                        .neqv. &
                        map(top+1:top+matchable,col))) cycle toploop1
            end do
            summary1 = top
            exit
        end do toploop1

        ! do the runs with one field flipped and ignore the unflipped run's solution
        summary = 0
        fliprowloop: do fliprow = 1, size(map, 1)
            flipcolloop: do flipcol = 1, size(map, 2)
                ! flip one field
                map(fliprow, flipcol) = .not. map(fliprow, flipcol)

                toploop: do top = 1, height - 1
                    ! if the reflection line is not exactly in the middle, the smaller part
                    ! defines how many columns could be matched
                    matchable = min(top, height - top)

                    ! speedup: fliprow must be included in the currently checked range, or
                    ! the change would not make any difference
                    if (fliprow < (top-matchable+1)) cycle toploop
                    if (fliprow > (top+matchable)) cycle toploop

                    do col = 1, size(map, 2)
                        if (ANY(map(top:(top-matchable+1):-1,col) &
                                .neqv. &
                                map(top+1:top+matchable,col))) cycle toploop
                    end do

                    ! accept solution only if is different from the unflipped solution
                    if (summary1 /= top) then
                        summary = top
                        exit fliprowloop
                    end if
                end do toploop

                ! flip field back
                map(fliprow, flipcol) = .not. map(fliprow, flipcol)
            end do flipcolloop
        end do fliprowloop
    end function

    function find_reflection(lines) result(summary)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer(int64)               :: summary
        logical, allocatable         :: map(:,:)
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
            
            ! find reflection and add value to summary
            block = lines(first:last)
            summary = summary + find_reflection(block)

            first = last + 2
        end do

        solve = summary
        ! solve = -1
    end function

end module day13b
