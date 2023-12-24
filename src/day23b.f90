!> Solution for https://adventofcode.com/2023/day/23 part b
module day23b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray
    use class_intringbuffer, only : IntRingBuffer
    implicit none
    private

    integer, parameter :: maxlinelength = 141
    integer, parameter :: max_backtracking_coords = 1000
    character(len=1), parameter :: land_path = '.'
    character(len=1), parameter :: land_forest = '#'
    integer, parameter :: right = 1
    integer, parameter :: top = 2
    integer, parameter :: left = 3
    integer, parameter :: down = 4
    ! offset is first row, second column
    integer, parameter :: offset(2, 4) = reshape([0, 1, -1, 0, 0, -1, 1, 0], [2, 4])
    character(len=*), parameter :: narrowmapoutput = '(23A1)'
    character(len=*), parameter :: widemapoutput = '(141A1)'
    character(len=*), parameter :: visitnarrowmapoutput = '(23L1)'
    character(len=*), parameter :: visitwidemapoutput = '(141L1)'

    public :: solve

contains

    logical function valid_target(map, pos)
        implicit none

        character(len=1), intent(in) :: map(:,:)
        integer, intent(in)          :: pos(2)

        valid_target = .false.
        if (pos(1) >= 1) then
            if (pos(1) <= size(map, 1)) then
                if (pos(2) >= 1) then
                    if (pos(2) <= size(map, 2)) then
                        ! only forst is not a valid target
                        valid_target = (map(pos(1), pos(2)) /= land_forest)
                    end if
                end if
            end if
        end if
    end function


    function create_map(lines) result(map)
        implicit none

        character(len=*), intent(in)  :: lines(:)
        integer                       :: row, col
        character(len=1), allocatable :: map(:,:)

        allocate(map(size(lines), len(lines(1))))

        do row = 1, size(map, 1)
            do col = 1, size(map, 2)
                map(row, col) = lines(row)(col:col)
            end do
        end do
    end function

    recursive subroutine walk_land(map, orgcurpos, visitmap, orgdistance, longest_walk)
        implicit none

        character(len=1), intent(in)  :: map(:,:)
        integer, intent(in)           :: orgcurpos(2)
        logical, intent(inout)        :: visitmap(:,:)
        integer(int64), intent(in)    :: orgdistance
        integer(int64), intent(inout) :: longest_walk
        integer                       :: curpos(2), nextpos(2), testpos(2), goal(2), direction, backtracking_row, backtracking_col
        logical                       :: choices(4), crawl
        type(IntRingBuffer)           :: backtracking_coords
        integer(int64)                :: curdistance

        goal = [size(map, 1), (size(map, 2) - 1)]

        ! goal will never be found during recursion, but during crawling

        call backtracking_coords%init(max_backtracking_coords)
        curpos = orgcurpos
        curdistance = orgdistance
        crawl = .true.
        do while(crawl)
            choices = .false.
            do direction = right, down
                testpos = curpos + offset(:, direction)
                if (valid_target(map, testpos)) then
                    if (.not. visitmap(testpos(1), testpos(2))) then  ! field not yet visited
                        choices(direction) = .true.
                    end if
                end if
            end do

            ! based on the number of choices the walk either stops, continues crawnling, or spreads
            if (count(choices) == 0) then
                ! dead end, start back tracking
                crawl = .false.
            else if (count(choices) == 1) then
                ! only one direction, so crawl without recursion until needed
                direction = findloc(choices, .true., 1)
                nextpos = curpos + offset(:, direction)
                curdistance = curdistance + 1
                if ((nextpos(1) == goal(1)) .and. (nextpos(2) == goal(2))) then
                    ! goal found during crawling, can only happen here because of the map structure
                    ! call print_visitmap(visitmap)
                    ! if (curdistance > longest_walk) print *, 'end of path:', curdistance
                    longest_walk = max(longest_walk, curdistance)
                    crawl = .false.
                else
                    visitmap(nextpos(1), nextpos(2)) = .true.
                    call backtracking_coords%addLast(nextpos(1))
                    call backtracking_coords%addLast(nextpos(2))
                    curpos = nextpos
                end if
            else
                ! multiple target choices, try all with recursion, and then start back tracking
                do direction = right, down
                    if (choices(direction)) then
                        ! this direction is already tested valid, and it is not visited yet
                        nextpos = curpos + offset(:, direction)
                        visitmap(nextpos(1), nextpos(2)) = .true.
                        call walk_land(map, nextpos, visitmap, curdistance + 1, longest_walk)
                        visitmap(nextpos(1), nextpos(2)) = .false.
                    end if
                end do    
                crawl = .false.
            end if
        end do  ! while(crawl)
        ! restore crawled field as unvisited
        ! print *, 'number of backtracking_coords:', backtracking_coords%length()
        do while(.not. backtracking_coords%empty())
            backtracking_row = backtracking_coords%removeFirst()
            backtracking_col = backtracking_coords%removeFirst()
            visitmap(backtracking_row, backtracking_col) = .false.
        end do
    end subroutine

    subroutine print_map(map)
        implicit none

        character(len=1), intent(in)  :: map(:,:)
        integer                       :: row
        character(len=:), allocatable :: mapoutput

        if (size(map, 2) == 10) then
            mapoutput = narrowmapoutput
        else
            mapoutput = widemapoutput
        end if

        do row = 1, size(map, 1)
            print mapoutput, map(row, :)
        end do
    end subroutine

    subroutine print_visitmap(visitmap)
        implicit none

        logical, intent(in)           :: visitmap(:,:)
        integer                       :: row
        character(len=:), allocatable :: visitmapoutput

        if (size(visitmap, 2) == 10) then
            visitmapoutput = visitnarrowmapoutput
        else
            visitmapoutput = visitwidemapoutput
        end if

        do row = 1, size(visitmap, 1)
            print visitmapoutput, visitmap(row, :)
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: longest_walk
        character(len=1), allocatable :: map(:,:)
        logical, allocatable          :: visitmap(:,:)
        integer                       :: start(2)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        map = create_map(lines)
        ! call print_map(map)
        start = [1, 2]
        allocate(visitmap(size(map, 1), size(map, 2)), source=.false.)
        visitmap(start(1), start(2)) = .true.

        call walk_land(map, start, visitmap, 0_int64, longest_walk)

        solve = longest_walk
    end function
end module day23b
