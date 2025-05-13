module dual_numbers
    implicit none

    type :: dual
        real :: val                  ! Value component
        real, allocatable :: grad(:) ! Gradient vector
    contains
        procedure :: print => dual_print 
    end type dual

    interface dual
        module procedure dual_init_val      ! Value-only constructor
        module procedure dual_init_with_grad ! Full constructor
    end interface

contains

    function dual_init_val(val) result(d)
        real, intent(in) :: val
        type(dual) :: d
        d%val = val
    end function

    function dual_init_with_grad(val, grad) result(d)
        real, intent(in) :: val
        real, intent(in) :: grad(:)
        type(dual) :: d
        d%val = val
        allocate(d%grad(size(grad)))
        d%grad = grad
    end function

    subroutine dual_print(this)
        class(dual), intent(in) :: this
        write(*, '(A, F0.6, A)', advance='no') "dual(", this%val, ", "
        if (allocated(this%grad)) then
            write(*, '("[", *(F0.6, :", "))', advance='no') this%grad
            write(*, '("]")', advance='no')
        else
            write(*, '("no_grad")', advance='no')
        end if
        write(*, '(")")')
    end subroutine

end module dual_numbers