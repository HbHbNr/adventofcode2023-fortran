module util_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_gausss_area_formular
        use iso_fortran_env, only : int64
        use util, only : gausss_area_formular
        implicit none

        integer        :: array1(2, 3) = reshape([1, 1, 1, 3, 6, 3], [2, 3])
        integer(int64) :: area

        area = gausss_area_formular(array1)
        call assert_true (5 == area)
    end subroutine

    subroutine test_lcm
        use iso_fortran_env, only : int64
        use util, only : lcm
        implicit none

        integer(int64) :: array1(4) = [2, 3, 4, 5]
        integer(int64) :: lcm1
        integer(int64) :: array2(3) = [60_int64, 70_int64, 80_int64]
        integer(int64) :: lcm2

        lcm1 = lcm(array1)
        call assert_true (60 == lcm1)

        lcm2 = lcm(array2)
        call assert_true (1680 == lcm2)
    end subroutine

    subroutine test_printarray
        use iso_fortran_env, only : int64
        use util, only : printarray
        implicit none

        integer          :: array1(5) = [1, 2, 3, 4, 5]
        integer          :: array2(3) = [6, 7, 8]
        integer(int64)   :: array3(3) = [60_int64, 70_int64, 80_int64]
        character(len=3) :: array4(3) = ['600', '700', '800']

        call printarray(array1)
        call printarray(array2)
        call printarray(array1, array2)
        call printarray(array3)
        call printarray(array4)
    end subroutine

    subroutine test_printresultline_integer
        use util, only : printresultline_integer
        implicit none

        integer :: i = 42

        call printresultline_integer('01a', i)
    end subroutine

    subroutine test_printresultline_int64
        use iso_fortran_env, only : int64
        use util, only : printresultline_int64
        implicit none

        integer(int64) :: i = 42_int64

        call printresultline_int64('01a', i)
    end subroutine

    subroutine test_printresultline_stringarray
        use util, only : printresultline_stringarray
        implicit none

        character(len=5) :: s(3) = ['1    ', '2    ', '42   ']

        call printresultline_stringarray('01a', s)
    end subroutine

    subroutine test_printresultline
        use util, only : printresultline
        implicit none

        character(len=5) :: s = '42'

        call printresultline('01a', s)
    end subroutine

    ! printioerror(...) is not testable, because it will stop the program

    subroutine test_readinputfile_asline_example
        use util, only : readinputfile_asline
        implicit none

        character(len=:), allocatable :: line

        line = readinputfile_asline('../inputfiles/day01_example.txt')
        call assert_equals (5, len_trim(line))
        call assert_equals (5, len(line))
        call assert_equals ('1abc2', line)
    end subroutine

    subroutine test_readinputfile_asline_input
        use util, only : readinputfile_asline
        implicit none

        character(len=:), allocatable :: line

        line = readinputfile_asline('../inputfiles/day01_input.txt')
        call assert_equals (23, len_trim(line))
        call assert_equals (23, len(line))
        call assert_equals ('dssmtmrkonedbbhdhjbf9hq', line(:23))
    end subroutine

    ! subroutine test_readinputfile_asstringarray_example
    !     use util, only : readinputfile_asstringarray
    !     implicit none

    !     integer, parameter            :: linebufferlength = 60
    !     character(len=:), allocatable :: lines(:)

    !     lines = readinputfile_asstringarray('../inputfiles/day01_example.txt', linebufferlength)

    !     call assert_equals (11, len(lines))
    !     call assert_equals (4, size(lines))
    ! end subroutine

    ! subroutine test_readinputfile_asstringarray_input
    !     use util, only : readinputfile_asstringarray
    !     implicit none

    !     integer, parameter            :: linebufferlength = 60
    !     character(len=:), allocatable :: lines(:)

    !     lines = readinputfile_asstringarray('../inputfiles/day01_input.txt', linebufferlength)
    !     call assert_equals (1000, len(lines))
    !     call assert_equals (22278, size(lines))
    ! end subroutine

    ! subroutine test_readinputfile_asintarray_example
    !     use iso_fortran_env, only : int8
    !     use util, only : readinputfile_asintarray
    !     implicit none

    !     integer, parameter           :: linebufferlength = 5
    !     integer(int8), allocatable   :: intarray(:,:)
    !     integer                      :: testvalue, testvalues(5) = [3, 3, 5, 4, 9], i

    !     intarray = readinputfile_asintarray('../inputfiles/day08_example.txt', linebufferlength)

    !     call assert_equals (5, size(intarray, 1))
    !     call assert_equals (5, size(intarray, 2))
    !     do i = 1, size(testvalues)
    !         testvalue = intarray(4,i)
    !         call assert_equals (testvalues(i), testvalue)
    !     end do
    ! end subroutine

    ! subroutine test_readinputfile_asintarray_input
    !     use iso_fortran_env, only : int8
    !     use util, only : readinputfile_asintarray
    !     implicit none

    !     integer, parameter           :: linebufferlength = 99
    !     integer(int8), allocatable   :: intarray(:,:)
    !     integer                      :: testvalue, testvalues(5) = [6, 6, 3, 6, 4], i

    !     intarray = readinputfile_asintarray('../inputfiles/day08_input.txt', linebufferlength)

    !     call assert_equals (99, size(intarray, 1))
    !     call assert_equals (99, size(intarray, 2))
    !     do i = 1, size(testvalues)
    !         testvalue = intarray(26 + i, 16)
    !         call assert_equals (testvalues(i), testvalue)
    !     end do
    ! end subroutine

    subroutine test_string_extract_int64s
        use iso_fortran_env, only : int64
        use util, only : string_extract_int64s
        implicit none

        character(len=*), parameter :: string1 = '1716002126 3982609232 32819234'
        integer(int64), allocatable   :: seeds1(:)
        character(len=*), parameter :: string2 = ' 1187290020,247767461 40283135 64738286  2044483296 66221787   '
        integer(int64), allocatable   :: seeds2(:)

        call string_extract_int64s(string1, seeds1)

        call assert_true (seeds1(1) == 1716002126_int64)
        call assert_true (seeds1(2) == 3982609232_int64)
        call assert_true (seeds1(3) == 32819234_int64)

        call string_extract_int64s(string2, seeds2)

        call assert_true (seeds2(1) == 1187290020_int64)
        call assert_true (seeds2(2) == 247767461_int64)
        call assert_true (seeds2(3) == 40283135_int64)
        call assert_true (seeds2(4) == 64738286_int64)
        call assert_true (seeds2(5) == 2044483296_int64)
        call assert_true (seeds2(6) == 66221787_int64)
    end subroutine

    subroutine test_string_extract_integers
        use util, only : string_extract_integers
        implicit none

        character(len=*), parameter :: string1 = '79 14 55 13'
        integer, allocatable   :: seeds1(:)
        character(len=*), parameter :: string2 = '    40     82     91     66'
        integer, allocatable   :: seeds2(:)

        call string_extract_integers(string1, seeds1)

        call assert_true (seeds1(1) == 79)
        call assert_true (seeds1(2) == 14)
        call assert_true (seeds1(3) == 55)
        call assert_true (seeds1(4) == 13)

        call string_extract_integers(string2, seeds2)

        call assert_true (seeds2(1) == 40)
        call assert_true (seeds2(2) == 82)
        call assert_true (seeds2(3) == 91)
        call assert_true (seeds2(4) == 66)
    end subroutine

end module util_test
