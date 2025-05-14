program test_dual
    use dual_numbers
    implicit none

    type(dual) :: d1, d2, d3

    d1 = dual(1.0, [0.5, 0.5])
    d2 = dual(2.0, [1.0, -1.0])

    d3 = d1 + d2
    call d3%print()
    d3 = d3 + 0.5
    d3 = 0.5 + d3
    call d3%print()

    d3 = d3 - 0.5
    d3 = 0.5 - d3
    call d3%print()
    d3 = -d3
    call d3%print()
    d3 = d3 - d1
    call d3%print()

    d3 = d3 * d2
    call d3%print()
    d3 = d3 * 2.0
    d3 = 2.0 * d3 
    call d3%print()

    d3 = d3 / d2
    call d3%print()
    d3 = d3 / 4.0
    call d3%print()
    d3 = 1.0 / d3
    call d3%print()
end program