module util_test
    use fruit
    implicit none

    ! all tests will be public

contains

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

end module util_test
