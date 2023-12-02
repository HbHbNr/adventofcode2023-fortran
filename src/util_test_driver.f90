program tests

  ! Driver program for FRUIT unit tests in:
  ! src/util_test.f90

  ! Generated by FRUITPy.

  use fruit
  use util_test

  implicit none
  integer :: failed_count

  call init_fruit

  call run_test_case(test_printarray,"test_printarray")
  call run_test_case(test_printresultline_integer,"test_printresultline_integer")
  call run_test_case(test_printresultline_int64,"test_printresultline_int64")
  call run_test_case(test_printresultline_stringarray,"test_printresultline_stringarray")
  call run_test_case(test_printresultline,"test_printresultline")
  call run_test_case(test_readinputfile_asline_example,"test_readinputfile_asline_example")
  call run_test_case(test_readinputfile_asline_input,"test_readinputfile_asline_input")

  call get_failed_count(failed_count)
  call fruit_summary
  call fruit_finalize
  if (failed_count > 0) stop 1

end program tests