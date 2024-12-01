program day01
    use reader
    use string
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_01")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        integer :: ints(file_len,2),ints2(file_len,2)
        integer :: ans1, ans2
        ans1 = 0
        ans2 = 0
        ints = int_reader_columns(file_len,line_len,2)  
        ints2 = ints     
        call part1(file_len,ints,ans1)
        call part2(file_len,ints2,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine part1(num,ints,ans1)
        integer :: num,ans1
        integer :: ints(num,2),left(num),right(num)
        integer :: i,big,idx(1)
        left=0
        right=0
        big = maxval(ints)+10
        do i=1,num
            left(i)=minval(ints(:,1))
            right(i)=minval(ints(:,2))
            idx = findloc(ints(:,1),left(i))
            ints(idx(1),1)=big
            idx = findloc(ints(:,2),right(i))
            ints(idx(1),2)=big
        end do
        do i=1,num
            ans1 = ans1 + abs(left(i)-right(i))
        end do
    end subroutine part1

    subroutine part2(num,ints,ans2)
        integer :: num,ans2
        integer :: ints(num,2)
        integer :: i,times
        do i=1,file_len
            times=count(ints(i,1).eq.ints(:,2))
            ans2 = ans2 + (ints(i,1)*times)
        end do
    end subroutine part2
end program day01