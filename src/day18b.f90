!> Solution for https://adventofcode.com/2023/day/18 part b
module day18b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, gausss_area_formular, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 14
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: down = 4
    ! offset is first row, second column
    integer, parameter :: offset(2, 4) = reshape([0, 1, -1, 0, 0, -1, 1, 0], [2, 4])
    character(len=*), parameter :: narrowvisitmapoutput = '(7L1)'
    character(len=*), parameter :: widevisitmapoutput = '(281L1)'

    public :: solve

contains

    subroutine create_map(lines, start, steps)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer, intent(inout)        :: start(2)
        integer, intent(out)          :: steps(:,:)
        integer                       :: i, direction, distance
        character(len=1)              :: directionchar
        integer                       :: sumhori, sumvert, minhori, minvert, maxhori, maxvert

        sumhori = 0
        sumvert = 0
        minhori = 0
        minvert = 0
        maxhori = 0
        maxvert = 0
        do i = 1, size(lines)
            if (lines(i)(3:4) == '10') then
                read (lines(i)(8:12), '(Z5)') distance
                directionchar = lines(i)(13:13)
            else
                read (lines(i)(7:11), '(Z5)') distance
                directionchar = lines(i)(12:12)
            end if
            select case (directionchar)
            case ('0')  ! ('R')
                direction = 1
                sumhori = sumhori + distance
                maxhori = max(maxhori, sumhori)
            case ('3')  ! ('U')
                direction = 2
                sumvert = sumvert - distance
                minvert = min(minvert, sumvert)
            case ('2')  ! ('L')
                direction = 3
                sumhori = sumhori - distance
                minhori = min(minhori, sumhori)
            case ('1')  !('D')
                direction = 4
                sumvert = sumvert + distance
                maxvert = max(maxvert, sumvert)
            end select
            steps(:, i) = [direction, distance]
        end do

        start = [1 - minvert, 1 - minhori]
    end subroutine

    function set_coords(steps, start, coords) result(total_distance)
        implicit none

        integer, intent(in)  :: steps(:,:), start(:)
        integer, intent(out) :: coords(:,:)
        integer(int64)       :: total_distance
        integer              :: i, curpos(2), direction, distance

        total_distance = 0
        coords(:, 1) = start
        curpos = start
        ! the last step leads back to the start
        do i = 1, size(steps, 2) - 1
            direction = steps(1, i)
            distance = steps(2, i)
            curpos = curpos + distance * offset(:, direction)
            coords(:, i + 1) = curpos
            total_distance = total_distance + distance
        end do
        ! add distance of the last step
        total_distance = total_distance + steps(2, size(steps, 2))
    end function

    subroutine print_lines(lines)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer                       :: row

        do row = 1, size(lines)
            print *, lines(row)
        end do
    end subroutine

    subroutine print_visitmap(visitmap)
        implicit none

        logical, intent(in)           :: visitmap(:,:)
        integer                       :: row
        character(len=:), allocatable :: visitmapoutput

        if (size(visitmap, 2) == 7) then
            visitmapoutput = narrowvisitmapoutput
        else
            visitmapoutput = widevisitmapoutput
        end if

        do row = 1, size(visitmap, 1)
            print visitmapoutput, visitmap(row, :)
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer                       :: start(2)
        integer(int64)                :: border, dug
        integer, allocatable          :: steps(:,:), coords(:,:)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        allocate(steps(2, size(lines)))

        ! search start position
        call create_map(lines, start, steps)

        allocate(coords(2, size(lines)))
        border = set_coords(steps, start, coords)

        dug = gausss_area_formular(coords)

        ! add only half of the border and add the start cube
        solve = (border / 2) + dug + 1
        ! solve = -1
    end function

end module day18b
