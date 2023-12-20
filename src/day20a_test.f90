module day20a_test
    use iso_fortran_env, only : int64
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_name_to_id_to_name_short
        use day20a, only : name_to_id, id_to_name
        implicit none

        character(len=2), parameter :: name_a = ' a', name_z = ' z'
        integer          :: id_a, id_z

        id_a = name_to_id(name_a)
        id_z = name_to_id(name_z)
        call assert_equals (name_a, id_to_name(id_a))
        call assert_equals (name_z, id_to_name(id_z))
    end subroutine

    subroutine test_name_to_id_to_name_long
        use day20a, only : name_to_id, id_to_name
        implicit none

        character(len=2), parameter :: name_aa = 'aa', name_az = 'az', name_zz = 'zz'
        integer          :: id_aa, id_az, id_zz

        id_aa = name_to_id(name_aa)
        id_az = name_to_id(name_az)
        id_zz = name_to_id(name_zz)
        call assert_equals (name_aa, id_to_name(id_aa))
        call assert_equals (name_az, id_to_name(id_az))
        call assert_equals (name_zz, id_to_name(id_zz))
    end subroutine

    subroutine test_solve_example1
        use day20a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day20_example1.txt')
        call assert_true (-1 == result)
    end subroutine

    subroutine test_solve_example2
        use day20a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day20_example2.txt')
        call assert_true (-1 == result)
    end subroutine

    subroutine test_solve_input
        use day20a, only : solve
        implicit none

        integer(int64) :: result

        result = solve('../inputfiles/day20_input.txt')
        call assert_true (-1 == result)
    end subroutine

end module day20a_test
