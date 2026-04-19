module eml_type_fortran
    !! [All elementary functions from a single operator](https://arxiv.org/html/2603.21852)
    !! [SupplementaryInformation.pdf](https://arxiv.org/src/2603.21852v2/anc/SupplementaryInformation.pdf)

    use, non_intrinsic :: eml_class_fortran



    implicit none



    private

    public :: assignment(=)
    public :: operator(+)
    public :: operator(-)
    public :: operator(*)
    public :: operator(/)
    public :: operator(**)

    public :: eml_real64_type

    public :: acos
    public :: acosh
    public :: asin
    public :: asinh
    public :: atan
    public :: atanh
    public :: avg
    public :: cos
    public :: cosh
    public :: eml
    public :: exp
    public :: half
    public :: hypot
    public :: ieee_is_nan      ! from `eml_class_fortran`
    public :: imag             ! from `eml_class_fortran`
    public :: inv
    public :: log
    public :: logistic_sigmoid
    public :: real             ! from `eml_class_fortran`
    public :: sin
    public :: sinh
    public :: sqr
    public :: sqrt
    public :: tan
    public :: tanh



    type, extends(eml_real64_class) :: eml_real64_type

        contains

        procedure, pass, private, non_overridable :: set_iu_real64
        procedure, pass, private, non_overridable :: set_m1_real64
        procedure, pass, private, non_overridable :: set_p2_real64
        procedure, pass, private, non_overridable :: set_pi_real64

        generic, public :: set_iu => set_iu_real64
        generic, public :: set_m1 => set_m1_real64
        generic, public :: set_p2 => set_p2_real64
        generic, public :: set_pi => set_pi_real64

    end type



    interface operator(+)
        module procedure :: eml_add_real64
    end interface

    interface operator(-) ! unary
        module procedure :: eml_neg_real64
    end interface

    interface operator(-) ! binary
        module procedure :: eml_sub_real64
    end interface

    interface operator(*)
        module procedure :: eml_mul_real64
    end interface

    interface operator(/)
        module procedure :: eml_div_real64
    end interface

    interface operator(**)
        module procedure eml_pow_real64
    end interface

    interface acos
        module procedure :: eml_acos_real64
    end interface

    interface acosh
        module procedure :: eml_acosh_real64
    end interface

    interface asin
        module procedure eml_asin_real64
    end interface

    interface asinh
        module procedure :: eml_asinh_real64
    end interface

    interface atan
        module procedure :: eml_atan_real64
    end interface

    interface atanh
        module procedure :: eml_atanh_real64
    end interface

    interface avg
        module procedure :: eml_avg_real64
    end interface

    interface cos
        module procedure :: eml_cos_real64
    end interface

    interface cosh
        module procedure :: eml_cosh_real64
    end interface

    interface eml
        module procedure :: eml_eml_real64
    end interface

    interface exp
        module procedure :: eml_exp_real64
    end interface

    interface half
        module procedure :: eml_half_real64
    end interface

    interface hypot
        module procedure :: eml_hypot_real64
    end interface

    interface inv
        module procedure :: eml_inv_real64
    end interface

    interface log
        module procedure :: eml_natural_log_real64
    end interface

    interface logistic_sigmoid
        module procedure :: eml_logistic_sigmoid_real64
    end interface

    interface sin
        module procedure :: eml_sin_real64
    end interface

    interface sinh
        module procedure eml_sinh_real64
    end interface

    interface sqr
        module procedure :: eml_sqr_real64
    end interface

    interface sqrt
        module procedure :: eml_sqrt_real64
    end interface

    interface tan
        module procedure :: eml_tan_real64
    end interface

    interface tanh
        module procedure eml_tanh_real64
    end interface



    interface eml_acos
        module procedure :: eml_acos_real64
    end interface

    interface eml_acosh
        module procedure :: eml_acosh_real64
    end interface

    interface eml_add
        module procedure :: eml_add_real64
    end interface

    interface eml_asin
        module procedure :: eml_asin_real64
    end interface

    interface eml_asinh
        module procedure :: eml_asinh_real64
    end interface

    interface eml_atan
        module procedure :: eml_atan_real64
    end interface

    interface eml_atanh
        module procedure :: eml_atanh_real64
    end interface

    interface eml_avg
        module procedure :: eml_avg_real64
    end interface

    interface eml_cos
        module procedure :: eml_cos_real64
    end interface

    interface eml_cosh
        module procedure :: eml_cosh_real64
    end interface

    interface eml_div
        module procedure :: eml_div_real64
    end interface

    interface eml_exp
        module procedure :: eml_exp_real64
    end interface

    interface eml_half
        module procedure :: eml_half_real64
    end interface

    interface eml_hypot
        module procedure :: eml_hypot_real64
    end interface

    interface eml_inv
        module procedure :: eml_inv_real64
    end interface

    interface eml_log
        module procedure :: eml_natural_log_real64
    end interface

    interface eml_logistic_sigmoid
        module procedure :: eml_logistic_sigmoid_real64
    end interface

    interface eml_mul
        module procedure :: eml_mul_real64
    end interface

    interface eml_neg
        module procedure :: eml_neg_real64
    end interface

    interface eml_pow
        module procedure :: eml_pow_real64
    end interface

    interface eml_sin
        module procedure :: eml_sin_real64
    end interface

    interface eml_sinh
        module procedure :: eml_sinh_real64
    end interface

    interface eml_sqr
        module procedure :: eml_sqr_real64
    end interface

    interface eml_sqrt
        module procedure :: eml_sqrt_real64
    end interface

    interface eml_sub
        module procedure :: eml_sub_real64
    end interface

    interface eml_tan
        module procedure :: eml_tan_real64
    end interface

    interface eml_tanh
        module procedure :: eml_tanh_real64
    end interface



    contains



    elemental type(eml_real64_type) function eml_acos_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: iu, p1



        call iu%set_iu
        call p1%set_p1

        eml_acos_real64 = &!
            eml_mul( &!
                iu , &!
                eml_log( &!
                    eml_add( &!
                        x , &!
                        eml_mul( &!
                            eml_sqrt( eml_sub(x, p1) ) , &!
                            eml_sqrt( eml_add(x, p1) )   &!
                        ) &!
                    ) &!
                ) &!
            )

    end function



    elemental type(eml_real64_type) function eml_acosh_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p1



        ! type(eml_real64_type) :: m1

        ! call m1%set_m1

        ! eml_acosh_real64 = eml_asinh( eml_hypot( x, eml_sqrt(m1) ) )

        call p1%set_p1

        eml_acosh_real64 = &!
            eml_log( &!
                eml_add( &!
                    x , &!
                    eml_mul( &!
                        eml_sqrt( eml_sub( x , p1 ) ) , &!
                        eml_sqrt( eml_add( x , p1 ) )   &!
                    ) &!
                ) &!
            )

    end function



    elemental type(eml_real64_type) function eml_add_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_add_real64 = eml_sub( x , eml_neg(y) )

    end function



    module elemental type(eml_real64_type) function eml_asin_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: iu, p1



        ! type(eml_real64_type) :: pi

        ! call pi%set_pi

        ! eml_asin_real64 = eml_sub( eml_half(pi) , eml_acos(x) )

        call iu%set_iu
        call p1%set_p1

        eml_asin_real64 = &!
            eml_mul( &!
                iu , &!
                eml_log( &!
                    eml_sub( &!
                        eml_sqrt( eml_sub( p1 , eml_sqr(x) ) ) , &!
                        eml_mul( iu , x ) &!
                    ) &!
                ) &!
            )

    end function



    elemental type(eml_real64_type) function eml_asinh_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p1



        ! type(eml_real64_type) :: m1

        ! call m1%set_m1

        ! eml_asinh_real64 = eml_log( eml_add( x , eml_hypot(m1, x) ) )

        call p1%set_p1

        eml_asinh_real64 = &!
            eml_log( &!
                eml_add( x , eml_sqrt( eml_add( p1 , eml_sqr(x) ) ) ) &!
            )

    end function



    elemental type(eml_real64_type) function eml_atan_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) neg_iu



        ! eml_atan_real64 = eml_asin( eml_tanh( eml_asinh(x) ) )

        call neg_iu%set_iu

        neg_iu = eml_neg(neg_iu)

        eml_atan_real64 = &!
            eml_mul( &!
                eml_half(neg_iu) , &!
                eml_log( &!
                    eml_div( &!
                        x = eml_add( neg_iu , x ) , &!
                        y = eml_sub( neg_iu , x )   &!
                    ) &!
                ) &!
            )

    end function



    elemental type(eml_real64_type) function eml_atanh_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p1



        ! eml_atanh_real64 = eml_asinh( eml_inv( eml_tan( acos(x) ) ) )

        call p1%set_p1

        eml_atanh_real64 = &!
            eml_half( &!
                eml_sub( &!
                    eml_log( eml_add( p1 , x ) ) , &!
                    eml_log( eml_sub( p1 , x ) )   &!
                ) &!
            )

    end function



    elemental type(eml_real64_type) function eml_avg_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_avg_real64 = eml_half( eml_add( x , y ) )

    end function



    elemental type(eml_real64_type) function eml_cos_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: iu



        ! eml_cos_real64 = eml_cosh( eml_sqrt( eml_neg( eml_sqr(x) ) ) )

        call iu%set_iu

        eml_cos_real64 = eml_cosh( eml_neg( eml_mul( iu, x ) ) )

    end function



    elemental type(eml_real64_type) function eml_cosh_real64(x)

        type(eml_real64_type), intent(in) :: x

        eml_cosh_real64 = eml_avg( eml_exp( x ) , eml_exp( eml_neg(x) ) )

    end function



    elemental type(eml_real64_type) function eml_div_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_div_real64 = eml_mul( x , inv(y) )

    end function



    elemental type(eml_real64_type) function eml_eml_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_eml_real64 = eml_operator( x = x, y = y )

    end function



    elemental type(eml_real64_type) function eml_exp_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p1



        call p1%set_p1

        eml_exp_real64 = eml( x = x, y = p1 )

    end function



    module elemental type(eml_real64_type) function eml_half_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p2



        call p2%set_p2

        eml_half_real64 = eml_div( x = x , y = p2 )

    end function



    elemental type(eml_real64_type) function eml_hypot_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_hypot_real64 = sqrt( eml_add( eml_sqr(x) , eml_sqr(y) ) )

    end function



    elemental type(eml_real64_type) function eml_inv_real64(x)

        type(eml_real64_type), intent(in) :: x

        eml_inv_real64 = eml_exp( eml_neg( eml_log(x) ) )

    end function



    elemental type(eml_real64_type) function eml_logistic_sigmoid_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: m1



        call m1%set_m1

        eml_logistic_sigmoid_real64 = &!
            inv( eml( x = eml_neg(x) , y = eml_exp(m1) ) )

    end function



    elemental type(eml_real64_type) function eml_mul_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_mul_real64 = eml_exp( eml_add( eml_log(x) , eml_log(y) ) )

    end function



    elemental type(eml_real64_type) function eml_natural_log_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p1



        call p1%set_p1

        eml_natural_log_real64 = &!
            eml( x = p1, y = eml_exp( eml( x = p1, y = x ) ) )

    end function



    module elemental type(eml_real64_type) function eml_neg_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p1



        call p1%set_p1

        eml_neg_real64 = eml_sub( x = eml_log(p1) , y = x )

    end function



    elemental type(eml_real64_type) function eml_pow_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_pow_real64 = eml_exp( eml_mul( y , eml_log(x) ) )

    end function



    elemental type(eml_real64_type) function eml_sin_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: pi



        call pi%set_pi

        eml_sin_real64 = eml_cos( eml_sub( x = x , y = eml_half(pi) ) )

    end function



    elemental type(eml_real64_type) function eml_sinh_real64(x)

        type(eml_real64_type), intent(in) :: x

        eml_sinh_real64 = eml( x = x , y = eml_exp( eml_cosh(x) ) )

    end function



    elemental type(eml_real64_type) function eml_sqr_real64(x)

        type(eml_real64_type), intent(in) :: x

        eml_sqr_real64 = eml_mul(x, x)

    end function



    elemental type(eml_real64_type) function eml_sqrt_real64(x)

        type(eml_real64_type), intent(in) :: x



        type(eml_real64_type) :: p2



        ! eml_sqrt_real64 = eml_exp( eml_half( eml_log(x) ) )

        call p2%set_p2

        eml_sqrt_real64 = eml_pow( x = x , y = inv(p2) )

    end function



    elemental type(eml_real64_type) function eml_sub_real64(x, y)

        type(eml_real64_type), intent(in) :: x, y

        eml_sub_real64 = eml( x = eml_log(x) , y = eml_exp(y) )

    end function



    elemental type(eml_real64_type) function eml_tan_real64(x)

        type(eml_real64_type), intent(in) :: x

        eml_tan_real64 = eml_div( x = eml_sin(x) , y = eml_cos(x) )

    end function



    elemental type(eml_real64_type) function eml_tanh_real64(x)

        type(eml_real64_type), intent(in) :: x

        eml_tanh_real64 = eml_div( x = eml_sinh(x) , y = eml_cosh(x) )

    end function



    elemental subroutine set_iu_real64(self)

        class(eml_real64_type), intent(inout) :: self



        type(eml_real64_type) :: m1



        call m1%set_m1

        self = eml_neg( eml_exp( eml_half( eml_log(m1) ) ) )

    end subroutine



    elemental subroutine set_m1_real64(self)

        class(eml_real64_type), intent(inout) :: self



        type(eml_real64_type) :: p1



        call p1%set_p1

        self = eml_sub( x = eml_log(p1) , y = p1 )

    end subroutine



    module elemental subroutine set_p2_real64(self)

        class(eml_real64_type), intent(inout) :: self



        type(eml_real64_type) :: m1, p1



        call m1%set_m1
        call p1%set_p1

        self = eml_sub( x = p1 , y = m1 )

    end subroutine



    elemental subroutine set_pi_real64(self)

        class(eml_real64_type), intent(inout) :: self



        type(eml_real64_type) :: m1



        call m1%set_m1

        self = sqrt( eml_neg( eml_sqr( eml_log(m1) ) ) )

    end subroutine

end module
