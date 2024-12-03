program mull_it_over
    use reader
    use string
    use integer
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_03")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character(line_len) :: data(file_len)
        integer :: ans1, ans2
        ans1 = 0
        ans2 = 0
        data = string_reader(file_len,line_len)
        call part1(data,file_len,line_len,ans1)
        call part2(data,file_len,line_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function is_mul(string,string_len) result (mul)
        integer :: string_len,mul,idxc,idxb,nums(2)
        character(string_len) :: string,test
        character(32) :: one,two 
        mul=0
        if (string(1:4).eq."mul(") then
            idxc=find(string,string_len,"~")
            idxb=find(string,string_len,")")
            if (idxc.ge.6.and.idxc.le.8.and.idxb.ge.idxc+2.and.idxb.le.idxc+4)then
                nums=ints_from_str(2,string(5:idxb),idxb-5)
                one=to_str(nums(1))
                two=to_str(nums(2))
                test="mul("//trim(one)//"~"//trim(two)//")"
                if (test.eq.string(1:len(trim(test)))) then
                    mul = nums(1)*nums(2)
                end if
            end if
        end if        
    end function is_mul

    subroutine part1(string,file_len,line_len,ans1)
        integer :: ans1,file_len,line_len,i,j,t,e
        character(line_len) :: string(file_len)
        do i=1,file_len
            do j=1,line_len-8
                if (j+12.gt.line_len) then
                    e = line_len
                else
                    e = j+12
                end if
                t = is_mul(string(i)(j:e),e-j+1)
                ans1 = ans1 + t
            end do
        end do
    end subroutine part1

    subroutine part2(string,file_len,line_len,ans2)
        integer :: ans2,file_len,line_len,i,j,t,e
        character(line_len) :: string(file_len)
        logical :: check=.true.
        do i=1,file_len
            do j=1,line_len-8
                if ("don't()".eq.string(i)(j:j+6)) then
                    check=.false.
                end if
                if ("do()".eq.string(i)(j:j+3)) then
                    check=.true.
                end if
                if (j+12.gt.line_len) then
                    e = line_len
                else
                    e = j+12
                end if
                if (check) then
                    t = is_mul(string(i)(j:e),e-j+1)
                    ans2 = ans2 + t
                end if
            end do
        end do
    end subroutine part2
end program mull_it_over