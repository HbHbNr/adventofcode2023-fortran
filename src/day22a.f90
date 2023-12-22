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
            ! print *, x1, y1, z1, x2, y2, z2
            do x = x1, x2
                do y = y1, y2
                    do z = z1, z2
                        ! print *, '   ', x, y, z
                        tower(x, y, z) = i
                    end do
                end do
            end do
        end do
    end subroutine

    subroutine drop_bricks(bricks, tower)
        implicit none

        integer, intent(inout) :: bricks(:,:)
        integer, intent(inout) :: tower(0:,0:,1:)
        integer                :: b, x, y, z, x1, y1, z1, x2, y2, z2, ztest
        logical                :: ztest_free, dropped_one

        ! do b = 1, size(bricks, 2)
        !     print *, bricks(:, b)
        ! end do

        dropped_one = .true.
        do while(dropped_one)
            dropped_one = .false.
            do b = 1, size(bricks, 2)
                ! print *, bricks(:, b)

                ! get coordinate of the current brick
                z1 = bricks(z1i, b)  ! guaranteed to be the lowest point, always z1 <= z2
                if (z1 < 2) cycle  ! dropping deeper is not possible
                x1 = bricks(x1i, b)
                y1 = bricks(y1i, b)
                x2 = bricks(x2i, b)
                y2 = bricks(y2i, b)
                z2 = bricks(z2i, b)

                ! test if area directly below the current brick is totally free
                ztest = z1 - 1
                ztest_free = .true.
                xloop: do x = x1, x2
                    do y = y1, y2
                        ! do not iterate over z, use ztest
                        if (tower(x, y, ztest) /= 0) then
                            ztest_free = .false.
                            exit xloop
                        end if
                    end do
                end do xloop

                ! if area directly below the current brick is totally free, drop it by one unit
                if (ztest_free) then
                    print *, 'dropping brick', b
                    dropped_one = .true.
                    bricks(:, b) = bricks(:, b) - [0, 0, 1, 0, 0, 1]
                    if (z1 == z2) then
                        ! horizontal brick because z1==z2
                        do x = x1, x2
                            do y = y1, y2
                                tower(x, y, z1) = 0  ! clear old brick
                                tower(x, y, ztest) = b  ! add new brick
                            end do
                        end do
                    else
                        ! vertikal brick, so x1==x2 and y1==y2
                        tower(x1, y1, z2) = 0  ! clear topmost brick
                        tower(x1, y1, ztest) = b  ! add new lowest brick
                    end if
                end if
            end do
        end do
        ! do b = 1, size(bricks, 2)
        !     print *, bricks(:, b)
        ! end do
    end subroutine

    function count_disintegratable_bricks(bricks, tower) result(disintegration_count)
        implicit none

        integer, intent(in) :: bricks(:,:)
        integer, intent(in) :: tower(0:,0:,1:)
        integer(int64)      :: disintegration_count

        disintegration_count = 0
    end function

    subroutine print_tower(tower)
        implicit none

        integer, intent(in) :: tower(0:,0:,1:)
        integer             :: y, z

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
        call drop_bricks(bricks, tower)
        call print_tower(tower)
        disintegration_count = count_disintegratable_bricks(bricks, tower)

        solve = disintegration_count
    end function

end module day22a
