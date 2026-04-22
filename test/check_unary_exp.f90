program check_unary_exp
    !! [SymbolicRegressionPackage/EML_toolkit/EmL_compiler/Test_C_math_h/run_unary_suite_c.py at master · VA00/SymbolicRegressionPackage](https://github.com/VA00/SymbolicRegressionPackage/blob/master/EML_toolkit/EmL_compiler/Test_C_math_h/run_unary_suite_c.py)



    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

    use, non_intrinsic :: eml_type_fortran

    use, non_intrinsic :: unary_operator_checker



    implicit none



    logical :: flag

    integer :: i, file_unit

    type(unary_operator_checker_type) :: record, trial



    open( &!
        newunit = file_unit , &!
        file    = 'test/check_unary_exp.dat' , &!
        status  = 'replace' &!
    )



    flag = .false.

    call record%initialize



    do i = -32, 32

        call trial%initialize

        trial%r_x  = 0.125_real64 * i
        trial%e_x  = trial%r_x

        trial%r_op = exp(trial%r_x)
        trial%e_op = exp(trial%e_x)



        if ( ieee_is_nan(trial%e_op) ) then

            call trial%display('exp')

            error stop ': exp(x) @ eml is NaN.'

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



    if (flag) call record%display('exp')



    close(file_unit)



    print *, 'OK: exp'

end program
