submodule (unary_operator_checker) imp_check_unary_logistic_sigmoid
    !! [SymbolicRegressionPackage/EML_toolkit/EmL_compiler/Test_C_math_h/run_unary_suite_c.py at master · VA00/SymbolicRegressionPackage](https://github.com/VA00/SymbolicRegressionPackage/blob/master/EML_toolkit/EmL_compiler/Test_C_math_h/run_unary_suite_c.py)

    implicit none

    contains

    module procedure check_unary_logistic_sigmoid

        logical :: flag

        integer :: i, file_unit

        type(unary_operator_checker_type) :: record, trial



        open( &!
            newunit = file_unit , &!
            file    = 'test/check_unary_logistic_sigmoid.dat' , &!
            status  = 'replace' &!
        )



        flag = .false.

        call record%initialize



        do i = -64, 64

            call trial%initialize

            trial%r_x  = 0.125_real64 * i
            trial%e_x  = trial%r_x

            trial%r_op = 1.0_real64 / ( 1.0_real64 + exp( - trial%r_x ) )
            trial%e_op = logistic_sigmoid(trial%e_x)



            if ( ieee_is_nan(trial%e_op) ) then

                call trial%display('logistic_sigmoid')

                error stop ': logistic_sigmoid(x) @ eml is NaN.'

            end if



            call trial%eval_error



            write(file_unit, *) &!
            &         trial%r_x               , & !  1
            &   real( trial%e_x             ) , & !  2
            &   imag( trial%e_x             ) , & !  3
            &         trial%r_op              , & !  4
            &   real( trial%e_op            ) , & !  5
            &   imag( trial%e_op            ) , & !  6
            &   real( trial%error        () ) , & !  7
            &   imag( trial%error        () ) , & !  8
            &   real( trial%error_scaled () ) , & !  9
            &   imag( trial%error_scaled () )     ! 10



            if ( abs( record%error_scaled() ) .lt. abs( trial%error_scaled() ) ) then

                flag   = .true.
                record = trial
        
            end if

        end do



        if (flag) call record%display('logistic_sigmoid')



        close(file_unit)



        print *, 'OK: logistic_sigmoid'

    end procedure

end submodule
