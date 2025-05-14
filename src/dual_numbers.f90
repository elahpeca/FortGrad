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

    interface operator(+)
        module procedure dual_add
        module procedure real_add_dual
        module procedure dual_add_real
    end interface 

    interface operator(-)
        module procedure dual_negate
        module procedure dual_subtract
        module procedure real_subtract_dual
        module procedure dual_subtract_real
    end interface

    interface operator(*)
        module procedure dual_multiply
        module procedure real_multiply_dual
        module procedure dual_multiply_real
    end interface

    interface operator(/)
        module procedure dual_divide
        module procedure real_divide_dual
        module procedure dual_divide_real
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

    function dual_add(a, b) result(res)
        type(dual), intent(in) :: a, b
        type(dual) :: res

        res%val = a%val + b%val
        if (allocated(a%grad) .and. allocated(b%grad)) then
            if ((size(a%grad)) /= size(b%grad)) then
                error stop "Error: Gradient sizes do not match."
            endif
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad + b%grad
        else if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad
        else if (allocated(b%grad)) then
            allocate(res%grad(size(b%grad)))
            res%grad = b%grad
        end if

    end function

    function real_add_dual(a, b) result(res)
        real, intent(in) :: a
        class(dual), intent(in) :: b
        type(dual) :: res

        res%val = a + b%val
        if (allocated(b%grad)) then
            allocate(res%grad(size(b%grad)))
            res%grad = b%grad
        end if

    end function

    function dual_add_real(a, b) result(res)
        class(dual), intent(in) :: a
        real, intent(in) :: b
        type(dual) :: res

        res%val = a%val + b
        if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad
        end if

    end function

    function dual_negate(a) result(res)
        type(dual), intent(in) :: a
        type(dual) :: res

        res%val = -a%val
        if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = -a%grad
        end if
        
    end function

    function dual_subtract(a, b) result(res)
        type(dual), intent(in) :: a, b
        type(dual) :: res
        res%val = a%val - b%val
        if (allocated(a%grad) .and. allocated(b%grad)) then
            if (size(a%grad) /= size(b%grad)) then
                error stop "Error: Gradient sizes do not match."
            end if
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad - b%grad
        else if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad
        else if (allocated(b%grad)) then
            allocate(res%grad(size(b%grad)))
            res%grad = -b%grad
        end if

    end function

    function real_subtract_dual(a, b) result(res)
        real, intent(in) :: a
        class(dual), intent(in) :: b
        type(dual) :: res

        res%val = a - b%val
        if (allocated(b%grad)) then
            allocate(res%grad(size(b%grad)))
            res%grad = -b%grad
        end if

    end function

    function dual_subtract_real(a, b) result(res)
        class(dual), intent(in) :: a
        real, intent(in) :: b
        type(dual) :: res

        res%val = a%val - b
        if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad
        end if

    end function

    function dual_multiply(a, b) result(res)
        class(dual), intent(in) :: a, b
        type(dual) :: res

        res%val = a%val * b%val
        if (allocated(a%grad) .and. allocated(b%grad)) then
            if (size(a%grad) /= size(b%grad)) then
                error stop "Error: Gradient sizes do not match."
            end if
            allocate(res%grad(size(a%grad)))
            res%grad = a%val * b%grad + b%val * a%grad
        else if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = b%val * a%grad
        else if (allocated(b%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = a%val * b%grad
        end if

    end function

    function real_multiply_dual(a, b) result(res)
        real, intent(in) :: a
        class(dual), intent(in) :: b
        type(dual) :: res
        
        res%val = a * b%val
        if (allocated(b%grad)) then
            allocate(res%grad(size(b%grad)))
            res%grad = a * b%grad
        end if

    end function

    function dual_multiply_real(a, b) result(res)
        class(dual), intent(in) :: a
        real, intent(in) :: b
        type(dual) :: res

        res%val = a%val * b
        if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = b * a%grad
        end if

    end function

    function dual_divide(a, b) result(res)
        class(dual), intent(in) :: a, b
        type(dual) :: res

        res%val = a%val / b%val
        if (allocated(a%grad) .and. allocated(b%grad)) then
            if (size(a%grad) /= size(b%grad)) then
                error stop "Error: Gradient sizes do not match."
            end if
            allocate(res%grad(size(a%grad)))
            res%grad = (b%val * a%grad - a%val * b%grad) / (b%val * b%val)
        else if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad / b%val
        else if (allocated(b%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = -a%val * b%grad / (b%val * b%val)
        end if

    end function

    function real_divide_dual(a, b) result(res)
        real, intent(in) :: a
        class(dual), intent(in) :: b
        type(dual) :: res
        res%val = a / b%val
        if (allocated(b%grad)) then
            allocate(res%grad(size(b%grad)))
            res%grad = -a * b%grad / (b%val * b%val)
        end if
    end function

    function dual_divide_real(a, b) result(res)
        class(dual), intent(in) :: a
        real, intent(in) :: b
        type(dual) :: res
        res%val = a%val / b
        if (allocated(a%grad)) then
            allocate(res%grad(size(a%grad)))
            res%grad = a%grad / b
        end if
    end function

end module dual_numbers