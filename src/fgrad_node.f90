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
        case(2) ! Multiplication
            this%value = this%left%value - this%right%value
        case(3) ! Subtraction
            this%value = this%left%value * this%right%value
        ! Other operations will be added soon
        end select
    end subroutine node_forward

    ! Backward pass (compute gradients)
    recursive subroutine node_backward(this, seed)
        class(Node), intent(inout) :: this
        logical, intent(in) :: seed
        
        if (seed) this%grad = 1.0d0  ! Initialize gradient for output node
        
        select case(this%operation)
        case(1) ! Addition
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad
            if (associated(this%right)) this%right%grad = this%right%grad + this%grad
        case(2) ! Subtraction
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad * 1.0d0
            if (associated(this%right)) this%right%grad = this%right%grad + this%grad * (-1.0d0)
        case(3) ! Multiplication
            if (associated(this%left)) this%left%grad = this%left%grad + this%grad * this%right%value
            if (associated(this%right)) this%right%grad = this%right%grad + this%grad * this%left%value
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
    function add(a, b) result(res)
        class(Node), pointer :: a, b, res
        allocate(res)
        res%value = a%value + b%value
        res%operation = 1
        res%left => a
        res%right => b
    end function add

    function subtract(a, b) result(res)
        class(Node), pointer :: a, b, res
        allocate(res)
        res%value = a%value - b%value
        res%operation = 2 
        res%left => a
        res%right => b
        res%is_initialized = .true.
    end function subtract

    function multiply(a, b) result(res)
        class(Node), pointer :: a, b, res
        allocate(res)
        res%value = a%value * b%value
        res%operation = 3
        res%left => a
        res%right => b
    end function multiply
    
end module fgrad_node