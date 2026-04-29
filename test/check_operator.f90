program check_operator

    use, non_intrinsic :: binary_operator_checker

    use, non_intrinsic :: unary_operator_checker



    implicit none



    call check_unary_exp

    call check_unary_log

    call check_binary_sub

    call check_unary_neg

    call check_binary_add

    call check_unary_inv

    call check_binary_mul

    call check_unary_sqr

    call check_binary_div

    call check_unary_half

    call check_binary_avg

    call check_binary_pow

    call check_unary_sqrt

    call check_binary_hypot

    call check_unary_logistic_sigmoid

    call check_unary_cosh

    call check_unary_sinh

    call check_unary_tanh

end program
