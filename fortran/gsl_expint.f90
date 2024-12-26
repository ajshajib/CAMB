module exponential_integral
    use, intrinsic :: iso_c_binding, only: c_double, c_int
    implicit none
    private
    public :: expint_E1

    interface
        function gsl_sf_expint_E1(x) bind(C, name="gsl_sf_expint_E1")
            use, intrinsic :: iso_c_binding, only: c_double
            implicit none
            real(c_double) :: gsl_sf_expint_E1
            real(c_double), value :: x
        end function gsl_sf_expint_E1
    end interface

contains

    double precision function expint_E1(val)
        implicit none
        double precision, intent(in) :: val
        double precision :: x, y !, sum, term, log_x
        ! integer :: n
        y = 0.0d0
        if (val > 500.) then
            x = 500.
        else
            x = val
        end if

        ! sum = 0.0d0
        ! term = 1.0d0
        ! n = 1

        ! do while (.true.)
        !     term = term * (-x / n)
        !     sum = sum + term / n
        !     if (abs((term / n) / (sum - term / n)) < 1.0d-6) then
        !         exit
        !     end if
        !     n = n + 1
        ! end do

        ! log_x = log(x)
        ! ! expint_E1 = -0.57721566490153286060d0 - log_x - sum
        y = gsl_sf_expint_E1(x)
        expint_E1 = y
    end function expint_E1

end module exponential_integral