program day01
    use reader
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2019_01")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        integer :: data(file_len)
        integer :: ans1, ans2
        ans1 = 0
        ans2 = 0
        data = int_reader(file_len)
        call part1(data,file_len,ans1)
        call part2(data,file_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine part1(nums,num_count,ans1)
        integer :: num_count, ans1,i,rem,temp
        integer :: nums(num_count)
        do i=1,num_count
            rem = mod(nums(i),3)
            temp = (nums(i)-rem)/3
            ans1 = ans1 + temp -2
        end do  
    end subroutine part1

    subroutine part2(nums,num_count,ans2)
        integer :: num_count, ans2,i,temp,rem
        integer :: nums(num_count)
        do i=1,num_count
            temp=nums(i)
            do while (temp.ge.0)
                rem=mod(temp,3)
                temp = (temp-rem)/3 -2
                if (temp.ge.0) then
                    ans2 = ans2 + temp
                end if
            end do
        end do
    end subroutine part2
end program day01