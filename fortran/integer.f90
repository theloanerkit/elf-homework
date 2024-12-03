module integer
    implicit none
    character :: str_digits(10) = (/"0","1","2","3","4","5","6","7","8","9"/)
    character :: str_hex_digits(16) = (/"0","1","2","3","4","5","6","7","8","9","a","b","c","d","e","f"/)
    contains

    function to_str(num) result (string)
        integer :: num
        !character(len=:),allocatable :: string
        character(len=32) :: string
        write(string,'(I0)') num
    end function to_str

    function is_integer(string,len) result (is_int)
        integer :: len,i
        character(len) :: string
        character :: char
        logical :: is_int
        is_int = .true.
        do i=1,len
            char = string(i:i)
            if (.not.any(str_digits.eq.char)) then
                is_int = .false.
            end if
        end do
    end function is_integer

    function is_hex(string,len) result (is_int)
        integer :: len,i
        character(len) :: string
        character :: char
        logical :: is_int
        is_int = .true.
        do i=1,len
            char = string(i:i)
            if (.not.any(str_hex_digits.eq.char)) then
                is_int = .false.
            end if
        end do
    end function is_hex

    function digit_idx(string,len) result (res)
        integer :: len, i
        character(len) :: string
        integer :: res(2), temp(1)
        res = (/-1,-1/)
        do i=1,len
            if (any(str_digits.eq.string(i:i))) then
                res(1) = i
                temp = findloc(str_digits,string(i:i))
                res(2) = temp(1)-1
                exit
            end if
        end do
    end function digit_idx

    function binary(num,bits) result (res)
        integer :: num,bits,temp,i,one
        character(bits) :: res
        res = repeat("0",bits)
        one=1
        do i=bits-1,0,-1
            temp=shifta(num,i)
            if (iand(temp,1).eq.1) then
                res(-i+32:-i+32) = "1"
            end if
        end do

    end function binary

end module integer