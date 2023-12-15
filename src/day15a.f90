!> Solution for https://adventofcode.com/2023/day/15 part a
module day15a
    use iso_fortran_env, only : int64
    use util, only : readinputfile_asline
    implicit none
    private

    integer, parameter :: maxlinelength = 60
    integer, parameter :: code_comma = iachar(',')

    public :: solve

contains

    ! function sumnumbers(lines) result(calibration_value)
    !     implicit none

    !     character(len=*), intent(in)  :: lines(:)
    !     integer(int64)                :: calibration_value
    !     character(len=:), allocatable :: line
    !     integer                       :: i, j, digit

    !     calibration_value = 0
    !     do i = 1, size(lines)
    !         line = lines(i)
    !         do j = 1, len_trim(line)
    !             if (iachar(line(j:j)) >= code_0) then
    !                 if (iachar(line(j:j)) <= code_9) then
    !                     digit = (iachar(line(j:j)) - code_0) * 10
    !                     exit
    !                 end if
    !             end if
    !         end do
    !         do j = len_trim(line), 1, -1
    !             if (iachar(line(j:j)) >= code_0) then
    !                 if (iachar(line(j:j)) <= code_9) then
    !                     digit = digit + (iachar(line(j:j)) - code_0)
    !                     exit
    !                 end if
    !             end if
    !         end do
    !         calibration_value = calibration_value + digit
    !     end do
    ! end function

    integer(int64) function solve(filename)
        implicit none

        character(len=*), intent(in)  :: filename
        character(len=:), allocatable :: line
        integer(int64)                :: total_sum
        integer                       :: i, hash, code

        line = readinputfile_asline(filename)

        total_sum = 0
        hash = 0
        do i = 1, len(line)
            code = iachar(line(i:i))
            if (code == code_comma) then
                total_sum = total_sum + hash
                hash = 0
                cycle
            end if
            hash = hash + code
            hash = hash * 17
            hash = modulo(hash, 256)
        end do
        if (hash > 0) then
            !also for the last character:
            total_sum = total_sum + hash
        end if

        solve = total_sum
    end function

end module day15a
