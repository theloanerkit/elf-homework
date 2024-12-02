module list
    implicit none

    contains

    !function to_list() result (arr)
    !    character(2) :: arr
    !    print*,"hello"
    !end function to_list

    function int_sorted(arr,len,strict) result (sorted)
        integer :: len,i,inc,cmpu,cmpd
        integer :: arr(len)
        logical :: sorted,strict
        cmpu = 0
        cmpd = 0
        sorted=.true.
        if (strict) then
            cmpu=1
            cmpd=-1
        end if
        if (arr(1).ne.arr(2)) then
            inc=(arr(2)-arr(1))/abs(arr(2)-arr(1))
        else 
            inc=0
        end if
        do i=1,len-1
            if (inc.eq.0.and.strict) then
                !print*,"zero gap"
                sorted=.false.
                exit
            end if
            if (inc.eq.1) then
                if (arr(i+1)-arr(i).lt.cmpu) then
                    !print*,"increasing list",arr(i),arr(i+1)
                    sorted=.false.
                    exit
                end if
            else if (inc.eq.-1) then
                !print*,"dec",arr(1),arr(2)
                if (arr(i+1)-arr(i).gt.cmpd) then
                   ! print*,"decreasing list",arr(i),arr(i+1)
                    sorted=.false.
                    exit
                end if
            end if
        end do
    end function int_sorted

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