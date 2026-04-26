module operator_checker

    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: eml_type_fortran



    implicit none



    private

    public :: real64

    public :: operator_checker_class



    type, abstract :: operator_checker_class

        real(real64) :: r_x, r_op

        type(eml_real64_type) :: e_x, e_op

        real(real64), private :: hidden_spacing

        complex(real64), private :: hidden_error, hidden_error_scaled

        contains

        procedure, pass :: error
        procedure, pass :: error_scaled

        procedure, pass, private :: display_error_kernel

        procedure, pass :: display_error
        procedure, pass :: eval_error
        procedure, pass :: initialize_error

    end type



    contains



    elemental complex(real64) function error(self)

        class(operator_checker_class), intent(in) :: self

        error = self%hidden_error

    end function



    elemental complex(real64) function error_scaled(self)

        class(operator_checker_class), intent(in) :: self

        error_scaled = self%hidden_error_scaled

    end function



    subroutine display_error_kernel(self, operation, arg)

        class(operator_checker_class), intent(in) :: self

        character(*), intent(in) :: operation, arg



        print *, self%hidden_error%re        , '; error%re'
        print *, self%hidden_error%im        , '; error%im'
        print *, self%hidden_spacing         , ';            spacing( ', operation, '(', arg, ') )'
        print *, self%hidden_error_scaled%re , '; error%re / spacing( ', operation, '(', arg, ') )'
        print *, self%hidden_error_scaled%im , '; error%im / spacing( ', operation, '(', arg, ') )'

    end subroutine



    subroutine display_error(self, operation)

        class(operator_checker_class), intent(in) :: self

        character(*), intent(in) :: operation

        call self%display_error_kernel(operation = operation, arg = 'x')

    end subroutine



    elemental subroutine eval_error(self)

        class(operator_checker_class), intent(inout) :: self



        self%hidden_error   = cmplx   (self%e_op) - self%r_op
        self%hidden_spacing = spacing (self%r_op)



        if ( abs(self%r_op) .gt. 0.0_real64 ) then

            self%hidden_error_scaled = self%hidden_error / self%hidden_spacing

        else

            self%hidden_error_scaled = self%hidden_error

        end if

    end subroutine



    elemental subroutine initialize_error(self)

        class(operator_checker_class), intent(inout) :: self

        self%hidden_spacing      = 0.0_real64
        self%hidden_error        = 0.0_real64
        self%hidden_error_scaled = 0.0_real64

    end subroutine

end module
