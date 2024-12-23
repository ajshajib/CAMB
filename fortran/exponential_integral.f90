program expint_E1
    implicit none
    double precision :: x, result

    ! Example usage
    x = 1.0
    result = expint_E1(x)
    print *, "E1(", x, ") = ", result

contains

    double precision function expint_E1(x)
        implicit none
        double precision, intent(in) :: x
        double precision :: sum, term, log_x
        integer :: n

        if (x > 11.) then
            x = 11.
        end if

        sum = 0.0d0
        term = 1.0d0
        n = 1

        do while (.true.)
            term = term * (-x / n)
            sum = sum + term / n
            if (abs((term / n) / (sum - term / n)) < 1.0d-6) then
                exit
            end if
            n = n + 1
        end do

        log_x = log(x)
        expint_E1 = -0.57721566490153286060d0 - log_x - sum
    end function expint_E1

end program expint_E1