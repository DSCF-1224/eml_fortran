module unary_operator_checker

    use, intrinsic :: ieee_arithmetic, only: ieee_quiet_nan, ieee_value



    use, non_intrinsic :: eml_type_fortran

    use, non_intrinsic :: ieee_class_fortran

    use, non_intrinsic :: operator_checker




    implicit none



    private

    public :: real64

    public :: unary_operator_checker_type

    public :: check_unary_asinh
    public :: check_unary_cos
    public :: check_unary_cosh
    public :: check_unary_exp
    public :: check_unary_half
    public :: check_unary_inv
    public :: check_unary_log
    public :: check_unary_logistic_sigmoid
    public :: check_unary_neg
    public :: check_unary_sin
    public :: check_unary_sinh
    public :: check_unary_sqr
    public :: check_unary_sqrt
    public :: check_unary_tan
    public :: check_unary_tanh



    type, extends(operator_checker_class) :: unary_operator_checker_type

        contains

        procedure, pass :: display
        procedure, pass :: initialize

    end type



    interface

        module subroutine check_unary_asinh
        end subroutine

        module subroutine check_unary_cos
        end subroutine

        module subroutine check_unary_cosh
        end subroutine

        module subroutine check_unary_exp
        end subroutine

        module subroutine check_unary_half
        end subroutine

        module subroutine check_unary_inv
        end subroutine

        module subroutine check_unary_log
        end subroutine

        module subroutine check_unary_logistic_sigmoid
        end subroutine

        module subroutine check_unary_neg
        end subroutine

        module subroutine check_unary_sin
        end subroutine

        module subroutine check_unary_sinh
        end subroutine

        module subroutine check_unary_sqr
        end subroutine

        module subroutine check_unary_sqrt
        end subroutine

        module subroutine check_unary_tan
        end subroutine

        module subroutine check_unary_tanh
        end subroutine

    end interface



    contains



    subroutine display(self, operation)

        class(unary_operator_checker_type), intent(in) :: self

        character(*), intent(in) :: operation



        character(:), allocatable :: offset



        offset = repeat( ' ', len(operation) )
        
        print *
        print *,      self%r_x  , '; ', offset, '  x         @ real64'
        print *, real(self%e_x) , '; ', offset, '  x%re      @ eml'
        print *, imag(self%e_x) , '; ', offset, '  x%im      @ eml'

        print *,      self%r_op  , '; ', operation, '( x    )    @ real64'
        print *, real(self%e_op) , '; ', operation, '( x    )%re @ eml'
        print *, imag(self%e_op) , '; ', operation, '( x    )%im @ eml'

        call self%display_error_unary(operation)

    end subroutine



    elemental subroutine initialize(self)

        class(unary_operator_checker_type), intent(inout) :: self



        call set_ieee_quiet_nan( self%r_x  )
        call set_ieee_quiet_nan( self%r_op )

        associate( nan => ieee_value(0.0_real64, ieee_quiet_nan) )

            self%e_x  = nan
            self%e_op = nan

        end associate

        call self%initialize_error

    end subroutine initialize

end module
