module intringbuffer_test
    use fruit
    implicit none

    ! all tests will be public

contains

    subroutine test_intringbuffer
        use class_IntRingBuffer, only : IntRingBuffer
        implicit none

        type(IntRingBuffer) :: ringbuffer

        call ringbuffer%init(8)
        ! call ringbuffer%print(.true.)

        call assert_equals(.true., ringbuffer%empty())
        call ringbuffer%addLast(1)
        call ringbuffer%addLast(2)
        ! call ringbuffer%print(.true.)

        call assert_equals(.false., ringbuffer%empty())
        call assert_equals(2, ringbuffer%length())
        call assert_equals(1, ringbuffer%get(1))
        call assert_equals(2, ringbuffer%get(2))
        call ringbuffer%addLast(3)
        call ringbuffer%addLast(4)
        call assert_equals(3, ringbuffer%get(3))
        call assert_equals(4, ringbuffer%get(4))
        ! call ringbuffer%print(.true.)

        call assert_equals(4, ringbuffer%length())
        call assert_equals(1, ringbuffer%getFirst())
        call assert_equals(1, ringbuffer%get(1))
        call assert_equals(4, ringbuffer%getLast())
        call assert_equals(4, ringbuffer%get(4))
        ! call ringbuffer%print(.true.)

        call ringbuffer%addFirst(40)
        call assert_equals(40, ringbuffer%get(1))
        call assert_equals(4, ringbuffer%get(5))
        ! call ringbuffer%print(.true.)

        call ringbuffer%addFirst(39)
        call assert_equals(39, ringbuffer%get(1))
        call assert_equals(4, ringbuffer%get(6))
        ! call ringbuffer%print(.true.)

        call assert_equals(6, ringbuffer%length())
        call ringbuffer%addLast(5)
        call assert_equals(7, ringbuffer%length())
        call assert_equals(39, ringbuffer%getFirst())
        ! call ringbuffer%print(.true.)

        call ringbuffer%addFirst(38)
        call assert_equals(8, ringbuffer%length())
        ! call ringbuffer%print(.true.)

        call assert_equals(5, ringbuffer%getLast())
        call assert_equals(38, ringbuffer%removeFirst())
        call assert_equals(7, ringbuffer%length())
        ! call ringbuffer%print(.true.)

        call assert_equals(5, ringbuffer%removeLast())
        ! call ringbuffer%print(.true.)

        call assert_equals(6, ringbuffer%length())
        call assert_equals(39, ringbuffer%removeFirst())
        call assert_equals(5, ringbuffer%length())
        ! call ringbuffer%print(.true.)
    end subroutine

end module intringbuffer_test
