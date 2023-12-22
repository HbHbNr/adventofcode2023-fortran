!> Solution for https://adventofcode.com/2023/day/22 part a
module day22a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 15
    integer, parameter :: x1i = 1
    integer, parameter :: y1i = 2 
    integer, parameter :: z1i = 3
    integer, parameter :: x2i = 4
    integer, parameter :: y2i = 5
    integer, parameter :: z2i = 6

    public :: solve

contains

    subroutine create_tower(lines, bricks, tower, maxx)
        implicit none

        character(len=*), intent(in)      :: lines(:)
        integer, allocatable, intent(out) :: bricks(:,:)
        integer, allocatable, intent(out) :: tower(:,:,:)
        integer, intent(in)               :: maxx
        character(len=:), allocatable     :: line
        integer                           :: i, j, maxz, tildepos, x, y, z, x1, y1, z1, x2, y2, z2

        ! for every brick, x1/y1/z1 and x2/y2/z2
        allocate(bricks(6, size(lines)), source=0)

        ! save bricks and find maximal z
        maxz = 0
        do i = 1, size(lines, 1)
            line = lines(i)
            tildepos = index(line, '~')
            read (line(:tildepos - 1), *) bricks(x1i:z1i, i)
            read (line(tildepos + 1:), *) bricks(x2i:z2i, i)
            maxz = max(maxz, max(bricks(z1i, i), bricks(z2i, i)))
            ! print *, bricks(:, i)
        end do

        allocate(tower(0:maxx, 0:maxx, 1:maxz), source=0)
        do i = 1, size(bricks, 2)
            ! print *, bricks(:, i)
            x1 = bricks(x1i, i)
            y1 = bricks(y1i, i)
            z1 = bricks(z1i, i)
            x2 = bricks(x2i, i)
            y2 = bricks(y2i, i)
            z2 = bricks(z2i, i)
            print *, x1, y1, z1, x2, y2, z2
            do x = x1, x2
                do y = y1, y2
                    do z = z1, z2
                        print *, '   ', x, y, z
                        tower(x, y, z) = i
                    end do
                end do
            end do
        end do
    end subroutine

    function count_disintegratable_bricks(bricks, tower) result(disintegration_count)
        implicit none

        integer, intent(in)           :: bricks(:,:)
        integer, intent(out)          :: tower(0:,0:,1:)
        integer(int64)                :: disintegration_count
        integer                       :: i

        disintegration_count = 0
        do i = 1, size(bricks, 2)
            disintegration_count = disintegration_count + 1
        end do
    end function

    subroutine print_tower(tower)
        implicit none

        integer, intent(in) :: tower(0:,0:,1:)
        integer             :: x, y, z

        do z = size(tower, 3), 1, -1
            do y = lbound(tower, 2), ubound(tower, 2)
                write (*, '(10Z1)', advance='no') tower(:, y, z)
                write (*, '(A)', advance='no') '   '
            end do
            print *
        end do
        print *
    end subroutine

    integer(int64) function solve(filename, maxx)
        implicit none

        character(len=*), intent(in)  :: filename
        integer, intent(in)           :: maxx
        character(len=:), allocatable :: lines(:)
        integer, allocatable          :: bricks(:,:)
        integer, allocatable          :: tower(:,:,:)
        integer(int64)                :: disintegration_count

        lines = readinputfile_asstringarray(filename, maxlinelength)
        call create_tower(lines, bricks, tower, maxx)
        call print_tower(tower)
        ! disintegration_count = count_disintegratable_bricks(bricks, tower)

        solve = disintegration_count
    end function

end module day22a
