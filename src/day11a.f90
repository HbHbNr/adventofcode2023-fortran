!> Solution for https://adventofcode.com/2023/day/11 part a
module day11a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 140

    public :: solve

contains

    subroutine enumerate_galaxies(lines, map, double_rows, double_cols, galaxies, galaxypos)
        implicit none

        character(len=*), intent(in) :: lines(:)
        integer, intent(inout)       :: map(:,:)
        logical, intent(inout)       :: double_rows(:), double_cols(:)
        integer, intent(out)         :: galaxies, galaxypos(:,:)
        integer                      :: row, col

        galaxies = 0
        galaxypos = 0
        do row = 1, size(lines)
            do col = 1, len(lines(1))
                if (lines(row)(col:col) == '#') then
                    galaxies = galaxies + 1
                    map(row, col) = galaxies
                    galaxypos(:,galaxies) = [row, col]
                    ! print *, galaxies, ':', galaxypos(:,galaxies)
                    double_rows(row) = .false.
                    double_cols(col) = .false.
                end if
            end do
        end do
    end subroutine


    function expansion(doubles, start, end) result(length)
        implicit none

        logical, intent(in) :: doubles(:)
        integer, intent(in) :: start, end
        integer             :: length
        integer             :: step

        step = 1
        if (start > end) step = -1

        length = count(doubles(start:end:step), 1)
    end function

    function sum_shortest_paths(map, double_rows, double_cols, galaxies, galaxypos) result(path_sum)
        implicit none

        integer, intent(inout)       :: map(:,:)
        logical, intent(inout)       :: double_rows(:), double_cols(:)
        integer, intent(in)          :: galaxies, galaxypos(:,:)
        integer(int64)               :: path_sum
        integer                      :: g1, g2, length, pos1(2), pos2(2), distance(2)

        path_sum = 0
        do g1 = 1, galaxies - 1
            pos1 = galaxypos(:,g1)
            do g2 = g1 + 1, galaxies
                pos2 = galaxypos(:,g2)
                distance = pos2 - pos1
                ! manhattan distance on non-expanded map
                length = abs(distance(1)) + abs(distance(2))
                ! add expansion
                length = length + expansion(double_rows, pos1(1), pos2(1))
                length = length + expansion(double_cols, pos1(2), pos2(2))
                ! print *, g1, g2, length

                ! sum up final length
                path_sum = path_sum + length
            end do
        end do

    end function sum_shortest_paths

    subroutine print_map(map)
        implicit none

        integer, intent(in) :: map(:,:)
        integer             :: row

        do row = 1, size(map, 1)
            print *, map(row, :)
        end do
    end subroutine

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: lines(:)
        integer(int64)                :: path_sum
        integer, allocatable          :: map(:,:)
        logical, allocatable          :: double_rows(:), double_cols(:)
        integer                       :: galaxies, galaxypos(2,500)

        lines = readinputfile_asstringarray(filename, maxlinelength)
        allocate(map(size(lines), len(lines(1))))
        allocate(double_rows(size(lines)))
        allocate(double_cols(len(lines(1))))
        map = 0
        double_rows = .true.
        double_cols = .true.
        galaxies = 0

        call enumerate_galaxies(lines, map, double_rows, double_cols, galaxies, galaxypos)
        ! call print_map(map)
        ! print *, double_rows
        ! print *, double_cols

        path_sum = sum_shortest_paths(map, double_rows, double_cols, galaxies, galaxypos)

        solve = path_sum
        ! solve = -1
    end function

end module day11a
