!> Solution for https://adventofcode.com/2023/day/14 part b
module day14b
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 100
    integer, parameter :: empty_space = 0
    integer, parameter :: rock_round = 1
    integer, parameter :: rock_cube = 2
    character(len=*), parameter :: narrowmapoutput = '(10I1)'
    character(len=*), parameter :: widemapoutput = '(100I1)'

    public :: solve

contains

    function sum_rocks(map) result(total_load)
        implicit none

        integer, intent(inout) :: map(:,:)
        integer(int64)         :: total_load
        integer                :: height, row, col

        total_load = 0
        height = size(map, 1)
        do row = 1, height
            do col = 1, height
                if (map(row, col) == rock_round) then
                    total_load = total_load + (height - row + 1)
                end if
            end do
        end do
    end function

    subroutine roll_rocks_north(map)
        implicit none

        integer, intent(inout) :: map(:,:)
        integer                :: height, row, col, newrow, testrow

        height = size(map, 1)
        do row = 1, height
            do col = 1, size(map, 2)
                if (map(row, col) == empty_space) cycle
                if (map(row, col) == rock_cube) cycle
                ! round cube on this place

                ! max north, so not possible to roll it
                if (row == 1) cycle

                ! not max north, so try to roll it
                newrow = row
                do testrow = row - 1, 1, -1
                    if (map(testrow, col) == empty_space) then
                        newrow = testrow
                    else
                        exit
                    end if
                end do
                if (newrow /= row) then
                    ! move rock to new position
                    map(row, col) = empty_space
                    map(newrow, col) = rock_round
                end if
            end do
        end do
    end subroutine

    subroutine full_spin_circle(map)
        implicit none

        integer, intent(inout) :: map(:,:)
        integer                :: i

        ! perform one full cycle of four tilts
        do i = 1, 4
            call roll_rocks_north(map)
            ! next two steps together lead to 90 degrees turn clockwise
            map = map(size(map, 1):1:-1, :)
            map = transpose(map)
        end do
    end subroutine

    function spin_rocks(map) result(total_load)
        implicit none

        integer, intent(inout) :: map(:,:)
        integer(int64)         :: total_load
        integer                :: currentcycle, cyclelength, missingcycles, i
        integer, parameter     :: mapsnapshotcycle = 200, targetcycles = 1000000000
        integer, allocatable   :: mapsnapshot(:,:)
        logical                :: no_changes

        ! allocate second map with same dimensions
        allocate(mapsnapshot, mold=map)

        do currentcycle = 1, mapsnapshotcycle + 100
            call full_spin_circle(map)

            if (currentcycle == mapsnapshotcycle) then
                ! make snapshot of the map in "mapsnapshotcycle" as base to find assumed cycle length 
                mapsnapshot(:,:) = map(:,:)
            else if (currentcycle > mapsnapshotcycle) then
                ! compare snapshotted map with current map
                no_changes = ALL(mapsnapshot == map)
                if (no_changes) then
                    ! found exactly the same pattern -> found cycle length -> calculate missing cycles with modulo
                    cyclelength = currentcycle - mapsnapshotcycle
                    missingcycles = modulo(targetcycles - currentcycle, cyclelength)
                    if (missingcycles > 0) then
                        do i = 1, missingcycles
                            call full_spin_circle(map)
                        end do
                    end if

                    ! not at cycle 1000000000, but due to the cycle length the result is the same
                    total_load = sum_rocks(map)
                    exit
                end if
            end if
        end do
    end function

    function create_map(lines) result(map)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer                      :: row, col
        integer, allocatable         :: map(:,:)

        allocate(map(size(lines), len_trim(lines(1))))
        map = empty_space

        do row = 1, size(map, 1)
            do col = 1, size(map, 2)
                select case (lines(row)(col:col))
                case ('O')
                    map(row, col) = rock_round
                case ('#')
                    map(row, col) = rock_cube
                end select
            end do
        end do
    end function

    subroutine print_map(map)
        implicit none

        integer, intent(in) :: map(:,:)
        integer             :: row
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

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: total_load
        integer, allocatable          :: map(:,:)
        
        lines = readinputfile_asstringarray(filename, maxlinelength)
        map = create_map(lines)
        total_load = spin_rocks(map)

        solve = total_load
    end function

end module day14b
