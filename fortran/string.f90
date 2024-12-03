module string
    implicit none
    integer :: digits(10) = (/0,1,2,3,4,5,6,7,8,9/)
    character :: str_digits(10) = (/"0","1","2","3","4","5","6","7","8","9"/)
    character :: alphabet(26) = (/"a","b","c","d","e","f","g","h","i","j","k","l","m", &
                                & "n","o","p","q","r","s","t","u","v","w","x","y","z"/)
    contains

    function find(string,string_len,char) result (idx)
        integer :: idx,string_len,i
        character(string_len) :: string
        character :: char
        idx=-1
        do i=1,string_len
            if (string(i:i).eq.char) then
                idx=i
                exit
            end if
        end do
    end function find

    function count_chars(string_len,string,chars,char_count) result (num)
        integer :: num,string_len,char_count,i
        character(string_len) :: string
        character :: chars(char_count),char
        num=0
        do i=1,string_len
            char = string(i:i)
            if (any(char.eq.chars)) then
                num = num+1
            end if
        end do
    end function count_chars

    function ints_from_str(count,string,len) result (nums)
        integer :: count, len, i, idx(1), temp_num,j
        integer :: nums(count)
        character(len) :: string
        character :: char
        temp_num = 0
        j = 1
        nums=-1
        do i=1,len
            char = string(i:i)
            idx = findloc(str_digits,char)
            if (idx(1).ne.0) then
                temp_num = temp_num*10
                temp_num = temp_num + digits(idx(1))
            else
                if(temp_num.ne.0) then
                    nums(j)=temp_num
                    j = j+1
                    temp_num = 0
                end if
            end if
            if (j.gt.count) then
                exit
            end if
        end do
        if (temp_num.ne.0) then
            nums(j)=temp_num
        end if
    end function ints_from_str

    function digits_from_str(count,string,len) result (nums)
        integer :: count, len, i, idx(1), temp_num,j
        integer :: nums(count)
        character(len) :: string
        character :: char
        temp_num = 0
        j = 1
        nums=-1
        do i=1,len
            char = string(i:i)
            idx = findloc(str_digits,char)
            if (idx(1).ne.0) then
                nums(j) = digits(idx(1))
                j=j+1
            end if
            if (j.gt.count) then
                exit
            end if
        end do
        if (j.eq.count) then
            nums(j) = temp_num
        end if
    end function digits_from_str

    function str_to_int(string,len) result (num)
        integer :: len,num,i,idx(1)
        character(len) :: string
        character :: char
        num = 0
        do i=1,len
            char = string(i:i)
            idx = findloc(str_digits,char)
            num = (num*10) + digits(idx(1))
        end do
    end function str_to_int

    !function count_chars(string, char, len) result (count)
    !    integer :: len, count, i
    !    character :: char, test
    !    character(len) :: string
    !    count = 0
    !    do i=1,len
    !        test = string(i:i)
    !        if (test.eq.char) then
    !           count = count + 1
    !        end if
    !    end do
    !end function count_chars

    function char_idx(char) result (i)
        character :: char
        integer :: idx(1),i
        idx = findloc(alphabet,char)
        i = idx(1)
    end function char_idx

    function reverse(string,len) result(gnirts)
        integer :: len
        character(len) :: string,gnirts
        integer :: i
        do i=1,len
            gnirts(len-i+1:len-i+1)=string(i:i)
        end do
    end function reverse

end module string