module list
    implicit none

    contains

    !function to_list() result (arr)
    !    character(2) :: arr
    !    print*,"hello"
    !end function to_list

    function to_list(string,line_len,separators,sep_count,list_len) result (arr)
        integer :: line_len,sep_count,list_len
        character(line_len) :: string,temp
        character :: separators(sep_count)
        character(line_len) :: arr(list_len)
        integer :: i,idx,tidx
        character :: char
        temp=""
        idx=1
        tidx=1
        do i=1,line_len
            char = string(i:i)
            if (any(char.eq.separators)) then
                arr(idx)=temp
                temp=""
                idx = idx+1
                tidx=1
            else
                temp(tidx:tidx)=char
                tidx=tidx+1
            end if
        end do
        if(idx.eq.list_len) then
            arr(list_len)=temp
        end if
    end function to_list

    function ipop(arr,arr_len,idx) result (new_arr)
        integer :: arr_len,idx,i,j
        integer :: arr(arr_len)
        integer :: new_arr(arr_len-1)
        j = 1
        do i=1,arr_len
            if (i.ne.idx) then
                new_arr(j) = arr(i)
                j = j + 1
            end if
        end do
    end function ipop
end module list