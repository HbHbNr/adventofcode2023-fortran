!> Solution for https://adventofcode.com/2023/day/18 part a
module day18a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
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

    function find_loop_orientation(steps, visitedmap, start) result(counter_clockwise_loop)
        implicit none

        integer, intent(in)    :: steps(:,:)
        logical, intent(inout) :: visitedmap(:,:)
        integer, intent(in)    :: start(2)
        logical                :: counter_clockwise_loop
        integer                :: curpos(2)
        integer                :: i, j, distance
        integer                :: direction, lastdirection, directionchange
        integer                :: counter_clockwise_turns_delta

        curpos = start
        lastdirection = 0
        counter_clockwise_turns_delta = 0
        visitedmap = .false.
        visitedmap(curpos(1), curpos(2)) = .true.
        do i = 1, size(steps, 2)
            direction = steps(1, i)
            distance = steps(2, i)
            do j = 1, distance
                curpos = curpos + offset(:, direction)
                visitedmap(curpos(1), curpos(2)) = .true.
            end do
            if (direction /= lastdirection) then
                if (lastdirection /= 0) then
                    ! positive direction change: counter-clockwise turn
                    ! negative direction change: clockwise turn
                    directionchange = direction - lastdirection
                    ! fix direction change from down to right
                    if (directionchange == -3) directionchange = 1
                    ! fix direction change from right to down
                    if (directionchange == 3) directionchange = -1

                    counter_clockwise_turns_delta = counter_clockwise_turns_delta + directionchange
                end if
                lastdirection = direction
            end if
        end do

        ! positive delta of turns: counter-clockwise loop
        ! negative delta of turns: clockwise loop
        counter_clockwise_loop = counter_clockwise_turns_delta > 0
    end function

    function is_diggable(visitedmap, pos) result(posdiggable)
        implicit none

        logical, intent(in) :: visitedmap(:,:)
        integer, intent(in) :: pos(2)
        logical             :: posdiggable

        posdiggable = .false.

        ! get cube only if inside of grid
        if (pos(1) >= 1) then
            if (pos(1) <= size(visitedmap, 1)) then
                if (pos(2) >= 1) then
                    if (pos(2) <= size(visitedmap, 2)) then
                        ! get cube only if not visited
                        if (.not. visitedmap(pos(1), pos(2))) then
                            ! ! get cube only if not yet painted
                            posdiggable = .true.
                        end if
                    end if
                end if
            end if
        end if
    end function

    recursive subroutine dig_cube_and_flood(visitedmap, pos, dug)
        implicit none

        logical, intent(inout) :: visitedmap(:,:)
        integer, intent(in)    :: pos(2)
        integer, intent(inout) :: dug
        integer                :: direction, nextpos(2)

        ! paint requested cube
        visitedmap(pos(1), pos(2)) = .true.
        dug = dug + 1

        ! start flooding
        do direction = 1, 4
            nextpos = pos + offset(:, direction)
            if (is_diggable(visitedmap, nextpos)) then
                call dig_cube_and_flood(visitedmap, nextpos, dug)
            end if
        end do
    end subroutine

    function dig_adjacent_cubes(steps, visitedmap, start, counter_clockwise_loop) result(dug)
        implicit none

        integer, intent(in)    :: steps(:,:)
        logical, intent(inout) :: visitedmap(:,:)
        integer, intent(in)    :: start(2)
        logical, intent(in)    :: counter_clockwise_loop
        integer                :: dug
        integer                :: curpos(2), neighbourpos(2)
        integer                :: i, j, distance
        integer                :: direction, neighbourdirection

        curpos = start
        dug = 0
        visitedmap(curpos(1), curpos(2)) = .true.
        do i = 1, size(steps, 2)
            direction = steps(1, i)
            distance = steps(2, i)
            if (counter_clockwise_loop) then
                neighbourdirection = direction + 1
            else
                neighbourdirection = direction -1
            end if
            ! fix neighbour direction from down to right
            if (neighbourdirection == 5) neighbourdirection = 1
            ! fix neighbour direction from right to down
            if (neighbourdirection == 0) neighbourdirection = 4
            ! first dig directly next to the current position
            neighbourpos = curpos + offset(:, neighbourdirection)
            if (is_diggable(visitedmap, neighbourpos)) then
                ! visited(neighbourpos(1), neighbourpos(2)) = .true.
                ! dug = dug + 1
                call dig_cube_and_flood(visitedmap, neighbourpos, dug)
            end if
            ! then dig along the way
            do j = 1, distance
                curpos = curpos + offset(:, direction)
                dug = dug + 1
                neighbourpos = curpos + offset(:, neighbourdirection)
                if (is_diggable(visitedmap, neighbourpos)) then
                    ! visited(neighbourpos(1), neighbourpos(2)) = .true.
                    ! dug = dug + 1
                    call dig_cube_and_flood(visitedmap, neighbourpos, dug)
                end if
            end do
        end do
    end function

    function create_map(lines, start, steps) result(visitedmap)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer, intent(inout)        :: start(2)
        integer, intent(out)          :: steps(:,:)
        logical, allocatable          :: visitedmap(:,:)
        integer                       :: i, direction, distance
        integer                       :: sumhori, sumvert, minhori, minvert, maxhori, maxvert

        sumhori = 0
        sumvert = 0
        minhori = 0
        minvert = 0
        maxhori = 0
        maxvert = 0
        do i = 1, size(lines)
            if (lines(i)(3:4) == '10') then
                distance = 10
            else
                distance = iachar(lines(i)(3:3)) - code_0
            end if
            select case (lines(i)(1:1))
            case ('R')
                direction = 1
                sumhori = sumhori + distance
                maxhori = max(maxhori, sumhori)
            case ('U')
                direction = 2
                sumvert = sumvert - distance
                minvert = min(minvert, sumvert)
            case ('L')
                direction = 3
                sumhori = sumhori - distance
                minhori = min(minhori, sumhori)
            case ('D')
                direction = 4
                sumvert = sumvert + distance
                maxvert = max(maxvert, sumvert)
            end select
            steps(:, i) = [direction, distance]
            ! print *, direction, distance
        end do
        ! print *, minvert, maxvert, minhori, maxhori
        ! print *, maxvert - minvert + 1, maxhori - minhori + 1

        start = [1 - minvert, 1 - minhori]
        ! print *, start
        allocate(visitedmap(maxvert - minvert + 1, maxhori - minhori + 1))
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
        logical                       :: counter_clockwise_loop
        integer                       :: start(2), dug
        logical, allocatable          :: visitedmap(:,:)
        integer, allocatable          :: steps(:,:)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        allocate(steps(2, size(lines)))
        ! search start position
        visitedmap = create_map(lines, start, steps)
        ! print '(I3, I3)', steps

        ! walk path for the first time, find the orientation of the loop (counter-clockwise or clockwise),
        ! and mark all visited cubes in the "visited" map
        counter_clockwise_loop = find_loop_orientation(steps, visitedmap, start)
        ! call print_visitmap(visitedmap)

        ! walk again and dig (and count) adjacent neighbours if they are on the correct side: left side if
        ! counter-clockwise loop, right side if clockwise loop
        dug = dig_adjacent_cubes(steps, visitedmap, start, counter_clockwise_loop)
        ! call print_visitmap(visitedmap)

        solve = dug
        ! solve = -1
    end function

end module day18a
