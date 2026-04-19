module eml_class_fortran
    !! [All elementary functions from a single operator](https://arxiv.org/html/2603.21852)
    !! [SupplementaryInformation.pdf](https://arxiv.org/src/2603.21852v2/anc/SupplementaryInformation.pdf)

    use, intrinsic :: iso_fortran_env, only: real64

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan



    implicit none



    private

    public :: eml_real64_class
    public :: real64

    public :: assignment(=)

    public :: eml_operator
    public :: ieee_is_nan
    public :: imag
    public :: real



    type, abstract :: eml_real64_class

        complex(real64), private :: hidden

        contains

        procedure, pass, private, non_overridable :: set_p1_real64

        generic, public :: set_p1 => set_p1_real64

    end type



    interface assignment(=)
        module procedure :: assign_eml_real64
        module procedure :: assign_complex64_to_eml_real64
        module procedure :: assign_real64_to_eml_real64
    end interface

    interface eml_operator
        module procedure :: eml_operator_real64
    end interface

    interface ieee_is_nan
        module procedure :: ieee_is_nan_eml_real64
    end interface

    interface imag
        module procedure :: eml_imag_real64
    end interface

    interface real
        module procedure :: eml_real_real64
    end interface



    contains



    elemental real(real64) function eml_imag_real64(x)

        class(eml_real64_class), intent(in) :: x

        eml_imag_real64 = x%hidden%im

    end function



    elemental complex(real64) function eml_operator_real64(x, y)

        class(eml_real64_class), intent(in) :: x, y

        eml_operator_real64 = exp(x%hidden) - log(y%hidden)

    end function



    elemental real(real64) function eml_real_real64(x)

        class(eml_real64_class), intent(in) :: x

        eml_real_real64 = x%hidden%re

    end function



    elemental logical function ieee_is_nan_eml_real64(x)

        class(eml_real64_class), intent(in) :: x

        ieee_is_nan_eml_real64 = &!
            ieee_is_nan(x%hidden%re) .and. &!
            ieee_is_nan(x%hidden%im)

    end function



    elemental subroutine assign_eml_real64(dst, src)

        class(eml_real64_class), intent(inout) :: dst

        class(eml_real64_class), intent(in) :: src

        dst%hidden = src%hidden

    end subroutine



    elemental subroutine assign_complex64_to_eml_real64(dst, src)

        class(eml_real64_class), intent(inout) :: dst

        complex(real64), intent(in) :: src

        dst%hidden = src

    end subroutine



    elemental subroutine assign_real64_to_eml_real64(dst, src)

        class(eml_real64_class), intent(inout) :: dst

        real(real64), intent(in) :: src

        dst%hidden = src

    end subroutine



    elemental subroutine set_p1_real64(self)

        class(eml_real64_class), intent(inout) :: self

        self = 1.0_real64

    end subroutine

end module
