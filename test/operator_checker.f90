module operator_checker

    use, intrinsic :: iso_fortran_env, only: real64

    use, non_intrinsic :: eml_type_fortran



    implicit none



    private

    public :: real64

    public :: operator_checker_class



    type, abstract :: operator_checker_class
    end type

end module
