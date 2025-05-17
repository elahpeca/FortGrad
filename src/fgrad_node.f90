module fgrad_node
    implicit none
    
    type Node
        real(8) :: value = 0.0d0    ! Node value
        real(8) :: grad = 0.0d0      ! Accumulated gradient
        logical :: is_initialized = .false.  ! Initialization flag
        class(Node), pointer :: left => null()  ! Left child node
        class(Node), pointer :: right => null() ! Right child node
        integer :: operation = 0     ! Operation type (0 - input node)
    contains
        procedure :: forward => node_forward
        procedure :: backward => node_backward
        procedure :: reset => node_reset
    end type Node

    interface Node
        module procedure node_create
    end interface Node

    interface operator(+)
        module procedure add
    end interface

    interface operator(-)
        module procedure subtract
        module procedure negate
    end interface

    interface operator(*)
        module procedure multiply
    end interface

    interface operator(/)
        module procedure divide
    end interface

    interface exp
        module procedure exp_node
    end interface

    interface log
        module procedure log_node
    end interface

contains
    ! Create new node
    function node_create(val) result(new_node)
        real(8), intent(in) :: val
        type(Node), pointer :: new_node
        
        allocate(new_node)
        new_node%value = val
        new_node%grad = 0.0d0
        new_node%is_initialized = .true.
        new_node%operation = 0
    end function node_create

    ! Forward pass (compute value)
    subroutine node_forward(this)
        class(Node), intent(inout) :: this
        
        select case(this%operation)
        case(0) ! Input node - do nothing
            continue
        case(1) ! Addition
            this%value = this%left%value + this%right%value
        case(2) ! Subtraction 
            this%value = this%left%value - this%right%value
        case(3) ! Negation
            this%value = -this%left%value
        case(4) ! Multiplication
            this%value = this%left%value * this%right%value
        case(5)  ! Division
            this%value = this%left%value / this%right%value
        case(6)  ! Exponent
            this%value = exp(this%left%value)
        case(7)  ! Logarithm
            this%value = log(abs(this%left%value) + 1e-8)
        ! Other operations will be added soon
        end select
    end subroutine node_forward

    ! Backward pass (compute gradients)
    recursive subroutine node_backward(this, seed)
        class(Node), intent(inout) :: this
        logical, intent(in) :: seed
        real(8) :: eps = 1e-8
        
        if (seed) this%grad = 1.0d0  ! Initialize gradient for output node
        
        select case(this%operation)
        case(1) ! Addition
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad
            if (associated(this%right)) this%right%grad = this%right%grad + this%grad
        case(2) ! Subtraction
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad * 1.0d0
            if (associated(this%right)) this%right%grad = this%right%grad + this%grad * (-1.0d0)
        case(3) ! Negation
            if (associated(this%left)) this%left%grad = this%left%grad - this%grad
        case(4) ! Multiplication
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad * this%right%value
            if (associated(this%right)) this%right%grad = this%right%grad + this%grad * this%left%value
        case(5)  ! Division
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad / this%right%value
            if (associated(this%right)) this%right%grad = this%right%grad - this%grad * this%left%value / (this%right%value**2 + eps)
        case(6)  ! Exponent
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad * exp(this%left%value)
        case(7)  ! Logarithm
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad / (abs(this%left%value) + eps)
        end select

        ! Continue propagation
        if (associated(this%left)) call node_backward(this%left, .false.)
        if (associated(this%right)) call node_backward(this%right, .false.)
    end subroutine node_backward

    ! Reset node state
    subroutine node_reset(this)
        class(Node), intent(inout) :: this

        this%grad = 0.0d0
        this%is_initialized = .false.
    end subroutine node_reset

    ! Node operations

    function const(val) result(res)
        real(8), intent(in) :: val
        type(Node), pointer :: res
        
        allocate(res)
        res%value = val
        res%operation = 0  ! A constant is a special case of an input node
    end function const

    function add(a, b) result(res)
        class(Node), pointer, intent(in) :: a, b
        class(Node), pointer :: res

        allocate(res)
        res%value = a%value + b%value
        res%operation = 1
        res%left => a
        res%right => b
    end function add

    function subtract(a, b) result(res)
        class(Node), pointer, intent(in) :: a, b
        class(Node), pointer :: res

        allocate(res)
        res%value = a%value - b%value
        res%operation = 2 
        res%left => a
        res%right => b
        res%is_initialized = .true.
    end function subtract

    function negate(a) result(res)
        class(Node), pointer, intent(in) :: a
        class(Node), pointer :: res

        allocate(res)
        res%value = -a%value
        res%operation = 3
        res%left => a
    end function negate

    function multiply(a, b) result(res)
        class(Node), pointer, intent(in) :: a, b
        class(Node), pointer :: res

        allocate(res)
        res%value = a%value * b%value
        res%operation = 4
        res%left => a
        res%right => b
    end function multiply

    function divide(a, b) result(res)
        class(Node), pointer, intent(in) :: a, b
        class(Node), pointer :: res

        allocate(res)
        res%value = a%value / b%value
        res%operation = 5
        res%left => a
        res%right => b
    end function divide

    function exp_node(a) result(res)
        class(Node), pointer, intent(in) :: a
        class(Node), pointer :: res

        allocate(res)
        res%value = exp(a%value)
        res%operation = 6
        res%left => a
    end function exp_node

    function log_node(a) result(res)
        class(Node), pointer, intent(in) :: a
        class(Node), pointer :: res
        
        allocate(res)
        res%value = log(abs(a%value) + 1e-8)
        res%operation = 7
        res%left => a
    end function log_node

end module fgrad_node