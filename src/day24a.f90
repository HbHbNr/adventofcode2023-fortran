!> Solution for https://adventofcode.com/2023/day/24 part a
module day24a
    use iso_fortran_env, only : int64, real128
    use util, only : readinputfile_asstringarray, code_0, code_9
    implicit none
    private

    integer, parameter :: maxlinelength = 68
    integer, parameter :: xi = 1
    integer, parameter :: yi = 2 
    integer, parameter :: zi = 3
    integer, parameter :: dxi = 4
    integer, parameter :: dyi = 5
    integer, parameter :: dzi = 6

    public :: solve

contains

    subroutine create_hails(lines, hails)
        implicit none

        character(len=*), intent(in)   :: lines(:)
        real(real128), allocatable, intent(out) :: hails(:,:)
        character(len=:), allocatable  :: line
        integer                        :: i, atpos

        ! for every hail, x1/y1/z1 and dx/dy/dz
        allocate(hails(6, size(lines)), source=0.0_real128)

        ! save hails
        do i = 1, size(lines, 1)
            line = lines(i)
            atpos = index(line, '@')
            read (line(:atpos - 2), *) hails(xi:zi, i)
            read (line(atpos + 2:), *) hails(dxi:dzi, i)
        end do
    end subroutine

    function count_intersections(hails, minxy, maxxy) result(intersection_count)
        implicit none

        real(real128), intent(in) :: hails(:,:)
        real(real128), intent(in) :: minxy
        real(real128), intent(in) :: maxxy
        integer(int64)   :: intersection_count
        integer          :: i, j
        real(real128)             :: x1, y1, x2, y2, x3, y3, x4, y4, dx12, dy12, dx34, dy34
        real(real128)             :: px_num, py_num, p_num_a, p_num_b, p_denom, px, py

        intersection_count = 0
        do i = 1, size(hails, 2) - 1
            do j = i + 1, size(hails, 2)
                dx12 = hails(dxi, i)
                dx34 = hails(dxi, j)
                dy12 = hails(dyi, i)
                dy34 = hails(dyi, j)
                ! original: (x1-x2)*(y3-y4)-(y1-y2)*(x3-x4)
                p_denom = dx12*dy34-dy12*dx34  ! all deltas as reversed, but that cancels each other
                if (p_denom == 0.0_real128) then
                    ! lines are parallel
                    cycle
                end if

                x1 = hails(xi, i)
                y1 = hails(yi, i)
                x2 = x1 + dx12
                y2 = y1 + dy12
                x3 = hails(xi, j)
                y3 = hails(yi, j)
                x4 = x3 + dx34
                y4 = y3 + dy34

                p_num_a = x1*y2-y1*x2
                p_num_b = x3*y4-y3*x4
                px_num = p_num_a*(-dx34)-(-dx12)*p_num_b
                py_num = p_num_a*(-dy34)-(-dy12)*p_num_b
                px = px_num / p_denom
                py = py_num / p_denom
                
                if (px >= minxy) then
                    if (px <= maxxy) then
                        if (py >= minxy) then
                            if (py <= maxxy) then
                                ! print *, i, j, px, py, 'in test area'
                                if ((px - x1) / dx12 > 0) then
                                    if ((px - x3) / dx34 > 0) then
                                        if ((py - y1) / dy12 > 0) then
                                            if ((py - y3) / dy34 > 0) then
                                                ! print *, i, j, px, py, 'in test area in the future'
                                                intersection_count = intersection_count + 1
                                            end if
                                        end if
                                    end if
                                end if
                            end if
                        end if
                    end if
                end if
            end do
        end do
    end function

    integer(int64) function solve(filename, minxy, maxxy)
        implicit none

        character(len=*), intent(in)  :: filename
        real(real128), intent(in)              :: minxy
        real(real128), intent(in)              :: maxxy
        character(len=:), allocatable :: lines(:)
        real(real128), allocatable             :: hails(:,:)
        integer(int64)                :: intersection_count

        lines = readinputfile_asstringarray(filename, maxlinelength)

        call create_hails(lines, hails)
        intersection_count = count_intersections(hails, minxy, maxxy)

        solve = intersection_count
    end function

end module day24a
