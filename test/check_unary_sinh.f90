submodule (unary_operator_checker) imp_check_unary_sinh
    !! [SymbolicRegressionPackage/EML_toolkit/EmL_compiler/Test_C_math_h/run_unary_suite_c.py at master · VA00/SymbolicRegressionPackage](https://github.com/VA00/SymbolicRegressionPackage/blob/master/EML_toolkit/EmL_compiler/Test_C_math_h/run_unary_suite_c.py)

    implicit none

    contains

    module procedure check_unary_sinh

        logical :: flag

        integer :: i, file_unit

        type(unary_operator_checker_type) :: record, trial



        open( &!
            newunit = file_unit , &!
            file    = 'test/check_unary_sinh.dat' , &!
            status  = 'replace' &!
        )



        flag = .false.

        call record%initialize



        do i = -32, 32

            call trial%initialize

            trial%r_x  = 0.125_real64 * i
            trial%e_x  = trial%r_x

            trial%r_op = sinh(trial%r_x)
            trial%e_op = sinh(trial%e_x)



            if ( ieee_is_nan(trial%e_op) ) then

                call trial%display('sinh')

                error stop ': sinh(x) @ eml is NaN.'

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



        if (flag) call record%display('sinh')



        close(file_unit)



        print *, 'OK: sinh'

    end procedure

end submodule
