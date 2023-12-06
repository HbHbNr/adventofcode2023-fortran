program tests

  ! Driver program for FRUIT unit tests in:
  ! src/day06b_test.f90

  ! Generated by FRUITPy.

  use fruit
  use day06b_test

  implicit none
  integer :: failed_count

  call init_fruit

  call run_test_case(test_solve_example,"test_solve_example")
  call run_test_case(test_solve_input,"test_solve_input")

  call get_failed_count(failed_count)
  call fruit_summary
  call fruit_finalize
  if (failed_count > 0) stop 1

end program tests