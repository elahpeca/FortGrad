program linear_regression
    use fgrad_node
    implicit none
    
    ! Model parameters
    class(Node), pointer :: w, b  ! Model weights (w - weight, b - bias)
    real(8) :: learning_rate = 0.01
    integer :: i, epoch
    integer, parameter :: n_epochs = 200
    integer, parameter :: n_samples = 5
    real(8) :: total_loss 

    ! Training data (x, y)
    real(8) :: train_x(5) = [1.0, 2.0, 3.0, 4.0, 5.0]
    real(8) :: train_y(5) = [3.0, 5.0, 7.0, 9.0, 11.0]

    ! Nodes for data (create once and reuse)
    class(Node), pointer :: x, y_true, y_pred, loss
    
    ! Initialize weights
    w => Node(0.0d0)
    b => Node(0.0d0)
    
    ! Create nodes for data; initialize with some initial value
    x => Node(0.0d0) 
    y_true => Node(0.0d0) 
        
    ! Training 
    do epoch = 1, n_epochs
        total_loss = 0.0

        do i = 1, n_samples
            ! Reset gradients
            call w%reset()
            call b%reset()

            ! Update node values for the current example
            x%value = train_x(i)
            y_true%value = train_y(i)
            
            ! Forward pass: compute prediction and loss
            y_pred => w * x + b
            loss => const(0.5d0) * (y_pred - y_true) * (y_pred - y_true)
            
            ! Backward pass: compute gradients
            call loss%backward(.true.)
            
            ! Update weights (gradient descent)
            w%value = w%value - learning_rate * w%grad
            b%value = b%value - learning_rate * b%grad
            
            ! Compute loss
            total_loss = total_loss + loss%value
            
            deallocate(y_pred, loss)
        end do
        
        if (mod(epoch, 20) == 0) then
            print '(A,I5,A,F8.4,A,F8.4,A,F8.4)', &
                  'Epoch:', epoch, &
                  ' | Loss:', total_loss/n_samples, &
                  ' | w:', w%value, &
                  ' | b:', b%value
        end if
    end do
    
    print *, 'Trained model: y =', w%value, '* x +', b%value

    deallocate(w, b, x, y_true)
end program linear_regression
