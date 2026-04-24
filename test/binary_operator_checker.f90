module binary_operator_checker

    use, intrinsic :: iso_fortran_env, only: real64

    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan, ieee_value



    use, non_intrinsic :: eml_type_fortran




    implicit none



    private

    public :: real64

    public :: binary_operator_checker_type

    public :: check_binary_sub



    type :: binary_operator_checker_type

        real(real64) :: r_x, r_y, r_op

        real(real64), private :: hidden_spacing

        complex(real64), private :: hidden_error, hidden_error_scaled

        type(eml_real64_type) :: e_x, e_y, e_op

        contains

        procedure, pass :: error
        procedure, pass :: error_scaled

        procedure, pass :: display
        procedure, pass :: eval_error
        procedure, pass :: initialize

    end type



    interface

        module subroutine check_binary_sub
        end subroutine

    end interface



    contains



    elemental complex(real64) function error(self)

        class(binary_operator_checker_type), intent(in) :: self

        error = self%hidden_error

    end function



    elemental complex(real64) function error_scaled(self)

        class(binary_operator_checker_type), intent(in) :: self

        error_scaled = self%hidden_error_scaled

    end function



    elemental subroutine eval_error(self)

        class(binary_operator_checker_type), intent(inout) :: self



        self%hidden_error   = cmplx   (self%e_op) - self%r_op
        self%hidden_spacing = spacing (self%r_op)



        if ( abs(self%r_op) .gt. 0.0_real64 ) then

            self%hidden_error_scaled = self%hidden_error / self%hidden_spacing

        else

            self%hidden_error_scaled = self%hidden_error

        end if

    end subroutine



    subroutine display(self, operation)

        class(binary_operator_checker_type), intent(in) :: self

        character(*), intent(in) :: operation



        character(:), allocatable :: offset



        offset = repeat( ' ', len(operation) )
        
        print *
        print *,      self%r_x  , '; ', offset, '  x                @ real64'
        print *,      self%r_y  , '; ', offset, '         y         @ real64'
        print *, real(self%e_x) , '; ', offset, '  x%re             @ eml'
        print *, imag(self%e_x) , '; ', offset, '  x%im             @ eml'
        print *, real(self%e_y) , '; ', offset, '         y%re      @ eml'
        print *, imag(self%e_y) , '; ', offset, '         y%im      @ eml'

        print *,      self%r_op  , '; ', operation, '( x    , y    )    @ real64'
        print *, real(self%e_op) , '; ', operation, '( x    , y    )%re @ eml'
        print *, imag(self%e_op) , '; ', operation, '( x    , y    )%im @ eml'

        print *, self%hidden_error%re        , '; error%re'
        print *, self%hidden_error%im        , '; error%im'
        print *, self%hidden_spacing         , ';            spacing( ', operation, '(x) )'
        print *, self%hidden_error_scaled%re , '; error%re / spacing( ', operation, '(x) )'
        print *, self%hidden_error_scaled%im , '; error%im / spacing( ', operation, '(x) )'

    end subroutine



    elemental subroutine initialize(self)

        class(binary_operator_checker_type), intent(inout) :: self

        associate( nan => ieee_value(0.0_real64, ieee_quiet_nan) )

            self%r_x  = nan
            self%r_op = nan

            self%e_x  = nan
            self%e_op = nan

        end associate

        self%hidden_spacing      = 0.0_real64
        self%hidden_error        = 0.0_real64
        self%hidden_error_scaled = 0.0_real64

    end subroutine initialize

end module
