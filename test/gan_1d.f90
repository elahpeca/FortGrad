program gan_1d
    use fgrad_node
    implicit none
    
    ! Parameters
    integer, parameter :: epochs = 10000, batch_size = 128, d_iters = 1 
    real(8), parameter :: g_lr = 0.0005, d_lr = 0.0002
    integer :: i, epoch, k
    
    ! Models
    type(Node), pointer :: G_w, G_b, D_w, D_b
    type(Node), pointer :: z, x, G_loss, D_loss
    type(Node), pointer :: G_output, D_real_out, D_fake_out
    type(Node), pointer :: temp1, temp2
    
    ! Weight initialization (Xavier initialization)
    G_w => Node(0.0d0); G_b => Node(0.0d0)
    D_w => Node(0.0d0); D_b => Node(0.0d0)
    
    call random_number(G_w%value); G_w%value = (G_w%value - 0.5d0) * sqrt(2.0d0)
    call random_number(G_b%value); G_b%value = (G_b%value - 0.5d0) * 0.01d0
    call random_number(D_w%value); D_w%value = (D_w%value - 0.5d0) * sqrt(2.0d0)
    call random_number(D_b%value); D_b%value = (D_b%value - 0.5d0) * 0.01d0
    
    ! Input nodes
    z => Node(0.0d0); x => Node(0.0d0)
    
    print *, "Initial weights - G_w:", G_w%value, "G_b:", G_b%value, "D_w:", D_w%value, "D_b:", D_b%value
    
    do epoch = 1, epochs
        ! Train discriminator (d_iters times)
        do k = 1, d_iters
            do i = 1, batch_size
                ! 1. Real data (normal distribution)
                call random_number(x%value)
                x%value = x%value * 2.0d0 - 1.0d0  ! [-1, 1]
                
                D_real_out => safe_sigmoid(D_w * x + D_b)
                temp1 => -log(D_real_out)
                
                ! 2. Fake data from generator
                call random_number(z%value)
                z%value = z%value * 2.0d0 - 1.0d0  
                G_output => G_w * z + G_b
                D_fake_out => safe_sigmoid(D_w * G_output + D_b)
                temp2 => -log(const(1.0d0) - D_fake_out)
                
                ! Combined discriminator loss function
                D_loss => (temp1 + temp2) / const(2.0d0)
                
                call D_loss%backward(.true.)
                
                ! Update discriminator weights
                D_w%value = D_w%value - d_lr * D_w%grad
                D_b%value = D_b%value - d_lr * D_b%grad
                
                ! Reset gradients and free memory
                call D_loss%reset()
                call D_w%reset(); call D_b%reset()
                nullify(D_real_out, D_fake_out, temp1, temp2, D_loss)
            end do
        end do
        
        ! Train generator
        do i = 1, batch_size
            call random_number(z%value)
            z%value = z%value * 2.0d0 - 1.0d0  
            G_output => G_w * z + G_b
            D_fake_out => safe_sigmoid(D_w * G_output + D_b)
            
            ! Generator loss function
            G_loss => -log(D_fake_out)
            
            call G_loss%backward(.true.)
            
            ! Update generator weights
            G_w%value = G_w%value - g_lr * G_w%grad
            G_b%value = G_b%value - g_lr * G_b%grad
            
            ! Reset gradients and free memory
            call G_loss%reset()
            call G_w%reset(); call G_b%reset()
            nullify(G_output, D_fake_out, G_loss)
        end do
        
        ! Log progress
        if (mod(epoch, 1000) == 0) then
            ! Calculate average losses
            call random_number(z%value)
            z%value = z%value * 2.0d0 - 1.0d0
            G_output => G_w * z + G_b
            D_fake_out => safe_sigmoid(D_w * G_output + D_b)
            
            call random_number(x%value)
            x%value = x%value * 2.0d0 - 1.0d0
            D_real_out => safe_sigmoid(D_w * x + D_b)
            
            temp1 => -log(D_real_out)
            temp2 => -log(const(1.0d0) - D_fake_out)
            D_loss => (temp1 + temp2) / const(2.0d0)
            G_loss => -log(D_fake_out)
            
            print '(A,I5,A,F8.4,A,F8.4,A,F8.4,A,F8.4)', &
                  'Epoch:', epoch, &
                  ' | D loss:', D_loss%value, &
                  ' | G loss:', G_loss%value, &
                  ' | D(x):', D_real_out%value, &
                  ' | D(G(z)):', D_fake_out%value
            
            ! Free memory
            call D_real_out%reset(); call D_fake_out%reset()
            call G_output%reset(); call D_loss%reset(); call G_loss%reset()
            call temp1%reset(); call temp2%reset()
            nullify(D_real_out, D_fake_out, G_output, D_loss, G_loss, temp1, temp2)
        end if
    end do
    
    ! Test generator
    print *, "Final generator samples:"
    do i = 1, 5
        call random_number(z%value)
        z%value = z%value * 2.0d0 - 1.0d0
        G_output => G_w * z + G_b
        print *, 'z:', z%value, '-> G(z):', G_output%value
        call G_output%reset()
        nullify(G_output)
    end do
    
    print *, "Final weights - G_w:", G_w%value, "G_b:", G_b%value, &
             "D_w:", D_w%value, "D_b:", D_b%value
    
    ! Free memory
    nullify(G_w, G_b, D_w, D_b, z, x)
    
contains
    function safe_sigmoid(a) result(res)
        type(Node), pointer :: a, res
        res => const(1.0d0) / (const(1.0d0) + exp(-a))
    end function
end program