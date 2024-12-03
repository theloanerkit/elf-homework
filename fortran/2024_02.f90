program red_nosed_reports
    use reader
    use list
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_02")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        integer :: data(file_len,8)
        integer :: ans1, ans2
        ans1 = 0
        ans2 = 0
        data = int_reader_columns(file_len,line_len,8)
        call part1(data,file_len,8,ans1)
        call part2(data,file_len,8,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function safe(nums,num_count) result(is_safe)
        integer :: num_count,i
        integer :: nums(num_count)
        logical :: is_safe
        character :: dir
        is_safe=.true.
        dir=""
        do i=1,num_count-1
            if (abs(nums(i)-nums(i+1)).ge.4) then
                is_safe=.false.
                exit
            end if
        end do
    end function safe

    subroutine part1(nums,file_len,cols,ans1)
        integer :: file_len,ans1,cols,i,idx(1)
        integer :: nums(file_len,cols)
        
        do i=1,file_len
            idx = findloc(nums(i,:),-1)-1
            if (idx(1).eq.-1) then
                idx=cols
            end if
            if (int_sorted(nums(i,:idx(1)),idx(1),.true.)) then
                if (safe(nums(i,:idx(1)),idx(1))) then
                    ans1=ans1+1
                end if
            end if
        end do
    end subroutine part1

    subroutine part2(nums,file_len,cols,ans2)
        integer :: file_len,ans2,cols,i,idx(1),t,j
        integer :: nums(file_len,cols),nums_pop(cols-1)
        do i=1,file_len
            idx = findloc(nums(i,:),-1)-1
            if (idx(1).eq.-1) then
                idx=cols
            end if
            t = ans2
            do j=1,idx(1)
                nums_pop = ipop(nums(i,:),cols,j)
                call part1(nums_pop,1,7,ans2)
                if (ans2.gt.t) then
                    exit
                end if
            end do
        end do
    end subroutine part2
end program red_nosed_reports