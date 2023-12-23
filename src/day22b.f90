!> Solution for https://adventofcode.com/2023/day/22 part b
module day22b
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
        integer                           :: i, maxz, tildepos, x, y, z, x1, y1, z1, x2, y2, z2

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
        end do

        ! place bricks inside of tower
        allocate(tower(0:maxx, 0:maxx, 1:maxz), source=0)
        do i = 1, size(bricks, 2)
            x1 = bricks(x1i, i)
            y1 = bricks(y1i, i)
            z1 = bricks(z1i, i)
            x2 = bricks(x2i, i)
            y2 = bricks(y2i, i)
            z2 = bricks(z2i, i)
            ! make sure coordinates are already ordered
            if (x1 > x2) stop
            if (y1 > y2) stop
            if (z1 > z2) stop

            do x = x1, x2
                do y = y1, y2
                    do z = z1, z2
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
        integer                :: b, x, y, x1, y1, z1, x2, y2, z2, ztest
        logical                :: ztest_free, dropped_one

        dropped_one = .true.
        do while(dropped_one)
            dropped_one = .false.
            do b = 1, size(bricks, 2)
                ! get coordinates of the current brick
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
                        ! vertical brick, so x1==x2 and y1==y2
                        tower(x1, y1, z2) = 0  ! clear topmost brick
                        tower(x1, y1, ztest) = b  ! add new lowest brick
                    end if
                end if
            end do  ! b = 1, size(bricks, 2)
        end do  ! while(dropped_one)
    end subroutine

    subroutine remove_brick_and_drop_bricks(orgbricks, orgtower, removed_brick, total_chain_reaction_count)
        implicit none

        integer, intent(in)           :: orgbricks(:,:)
        integer, intent(in)           :: orgtower(0:,0:,1:)
        integer, intent(in)           :: removed_brick
        integer(int64), intent(inout) :: total_chain_reaction_count
        integer, allocatable          :: bricks(:,:)
        integer, allocatable          :: tower(:,:,:)
        integer                       :: b, x, y, z, x1, y1, z1, x2, y2, z2, ztest
        integer                       :: chain_reaction_count
        logical                       :: ztest_free, dropped_one
        logical, allocatable :: falling_bricks(:)

        ! clone current bricks, marking the removed brick
        bricks = orgbricks
        bricks(:, removed_brick) = [0, 0, 0, 0, 0, 0]

        ! clone tower and remove requested brick
        tower = orgtower
        x1 = orgbricks(x1i, removed_brick)
        y1 = orgbricks(y1i, removed_brick)
        z1 = orgbricks(z1i, removed_brick)
        x2 = orgbricks(x2i, removed_brick)
        y2 = orgbricks(y2i, removed_brick)
        z2 = orgbricks(z2i, removed_brick)
        do x = x1, x2
            do y = y1, y2
                do z = z1, z2
                    tower(x, y, z) = 0
                end do
            end do
        end do

        ! store which bricks dropped
        allocate(falling_bricks(size(bricks, 2)), source=.false.)

        dropped_one = .true.
        do while(dropped_one)
            dropped_one = .false.
            do b = 1, size(bricks, 2)
                if (b == removed_brick) cycle
                ! get coordinates of the current brick
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
                    dropped_one = .true.
                    falling_bricks(b) = .true.
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
                        ! vertical brick, so x1==x2 and y1==y2
                        tower(x1, y1, z2) = 0  ! clear topmost brick
                        tower(x1, y1, ztest) = b  ! add new lowest brick
                    end if
                end if
            end do  ! b = 1, size(bricks, 2)
        end do  ! while(dropped_one)

        chain_reaction_count = count(falling_bricks)
        total_chain_reaction_count = total_chain_reaction_count + chain_reaction_count
    end subroutine

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
        integer(int64)                :: total_chain_reaction_count
        integer                       :: removed_brick

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call create_tower(lines, bricks, tower, maxx)

        call drop_bricks(bricks, tower)

        total_chain_reaction_count = 0
        do removed_brick = 1, size(bricks, 2)
            call remove_brick_and_drop_bricks(bricks, tower, removed_brick, total_chain_reaction_count)
        end do

        solve = total_chain_reaction_count
    end function

end module day22b
