submodule (binary_operator_checker) imp_check_binary_div
    !! [SymbolicRegressionPackage/EML_toolkit/EmL_compiler/Test_C_math_h/run_binary_suite_c.py at master · VA00/SymbolicRegressionPackage](https://github.com/VA00/SymbolicRegressionPackage/blob/master/EML_toolkit/EmL_compiler/Test_C_math_h/run_binary_suite_c.py)

    implicit none

    contains

    module procedure check_binary_div

        logical :: flag

        logical :: flag_diving_by_zero

        integer :: i, j, file_unit

        type(binary_operator_checker_type) :: record, trial



        open( &!
            newunit = file_unit , &!
            file    = 'test/check_binary_div.dat' , &!
            status  = 'replace' &!
        )



        flag = .false.

        call record%initialize



        do i = -16, 16
        do j = -16, 16

            call trial%initialize

            trial%r_x  = 0.5_real64 * i
            trial%e_x  = trial%r_x

            trial%r_y  = 0.5_real64 * j
            trial%e_y  = trial%r_y

            trial%r_op = trial%r_x / trial%r_y
            trial%e_op = trial%e_x / trial%e_y



            flag_diving_by_zero = (j .eq. 0)

            if (flag_diving_by_zero) then

                if (i .lt. 0) then

                    associate( &!
                        r_op_flag => .not. is_ieee_negative_inf(      trial%r_op  ) , &!
                        e_op_flag => .not. is_ieee_negative_inf( real(trial%e_op) )   &!
                    )

                        if (r_op_flag .or. e_op_flag) then

                            call trial%display('div')

                            error stop ': div(x,y) is NOT -inf.'

                        end if

                    end associate

                else if (i .gt. 0) then

                    associate( &!
                        r_op_flag => .not. is_ieee_positive_inf(      trial%r_op  ) , &!
                        e_op_flag => .not. is_ieee_positive_inf( real(trial%e_op) )   &!
                    )

                        if (r_op_flag .or. e_op_flag) then

                            call trial%display('div')

                            error stop ': div(x,y) is NOT +inf.'

                        end if

                    end associate

                else

                    associate( &!
                        r_op_flag => .not. ieee_is_nan(      trial%r_op  ) , &!
                        e_op_flag => .not. ieee_is_nan( real(trial%e_op) )   &!
                    )

                        if (r_op_flag .or. e_op_flag) then

                            call trial%display('div')

                            error stop ': div(x,y) is NOT NaN.'

                        end if

                    end associate

                end if

            end if



            if ( (.not. flag_diving_by_zero) .and. ieee_is_nan(trial%e_op) ) then

                call trial%display('div')

                error stop ': div(x,y) @ eml is NaN.'

            end if



            call trial%eval_error



            write(file_unit, *) &!
            &         trial%r_x               , & !  1
            &         trial%r_y               , & !  2
            &   real( trial%e_x             ) , & !  3
            &   imag( trial%e_x             ) , & !  4
            &   real( trial%e_y             ) , & !  5
            &   imag( trial%e_y             ) , & !  6
            &         trial%r_op              , & !  7
            &   real( trial%e_op            ) , & !  8
            &   imag( trial%e_op            ) , & !  9
            &   real( trial%error        () ) , & ! 10
            &   imag( trial%error        () ) , & ! 11
            &   real( trial%error_scaled () ) , & ! 12
            &   imag( trial%error_scaled () )     ! 13



            if ( abs( record%error_scaled() ) .lt. abs( trial%error_scaled() ) ) then

                flag   = .true.
                record = trial
        
            end if

        end do
        end do



        if (flag) call record%display('div')



        close(file_unit)



        print *, 'OK: div'

    end procedure

end submodule
