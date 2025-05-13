program test_dual
    use :: dual_numbers, only: dual
    implicit none

    type(dual) :: d, b

    d = dual(2.0) ! Value-only test
    call d % print()

    b = dual(2, [1.0, 2.0, 3.0]) ! Full test
    call b % print()
end program test_dual