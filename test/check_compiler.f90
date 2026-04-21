program check_compiler

    use, intrinsic :: iso_fortran_env , only: compiler_options
    use, intrinsic :: iso_fortran_env , only: compiler_version
    use, intrinsic :: iso_fortran_env , only: output_unit



    implicit none



    character(1), parameter :: str_new_line = new_line('')



    character(:), allocatable :: str_compiler_options



    write( unit = output_unit, fmt = '(A,1X,A)' ) '[compiler version]', compiler_version()



    block

        integer :: i



        str_compiler_options = compiler_options()

        do

            i = index(str_compiler_options, ' -')

            if (i .eq. 0) exit

            str_compiler_options(i:i) = str_new_line

        end do

    end block

    write( unit = output_unit, fmt = '(A)' ) str_new_line // '[compiler options]'
    write( unit = output_unit, fmt = '(A)' ) str_compiler_options // str_new_line

end program check_compiler
