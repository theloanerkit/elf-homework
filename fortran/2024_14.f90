program restroom_redoubt
    use reader
    use string
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    type :: robot
        integer :: pos(2),vel(2)
    end type
    call cpu_time(ts)
    open(1,file="2024_14")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len,nums(4),i
        character(line_len) :: data(file_len)
        integer :: ans1, ans2
        type(robot) :: robots(file_len) ,robots2(file_len)
        ans1 = 0
        ans2 = 0
        data=string_reader(file_len,line_len)
        do i=1,file_len
            nums = ints_from_str(4,data(i),line_len)
            robots(i)%pos = nums(:2)
            robots(i)%vel = nums(3:)
            !print*,"position",robots(i)%pos,"velocity",robots(i)%vel
        end do
        robots2=robots
        call part1(robots,size(robots),ans1,101,103)
        call part2(robots2,size(robots),ans2,101,103)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine move(robots,siz,time)
        integer :: siz,time,i
        type(robot) :: robots(siz)
        do i=1,siz
            robots(i)%pos(1)=robots(i)%pos(1)+(time*robots(i)%vel(1))
            robots(i)%pos(2)=robots(i)%pos(2)+(time*robots(i)%vel(2))
        end do
    end subroutine move

    subroutine part1(robots,siz,ans1,w,h)
        integer :: siz,ans1,w,h,i,tl,tr,bl,br
        type(robot)::robots(siz)
        call move(robots,siz,100)
        tl=0
        tr=0
        bl=0
        br=0
        do i=1,siz
            robots(i)%pos(1)=mod(robots(i)%pos(1),w)
            robots(i)%pos(2)=mod(robots(i)%pos(2),h)
            if (robots(i)%pos(1).lt.0) then
                robots(i)%pos(1)=w+robots(i)%pos(1)
            end if
            if (robots(i)%pos(2).lt.0) then
                robots(i)%pos(2)=h+robots(i)%pos(2)
            end if
            !print*,robots(i)%pos
            if (robots(i)%pos(1).lt.w/2) then
                !print*,"left"
                if (robots(i)%pos(2).lt.h/2) then
                    tl=tl+1
                else if (robots(i)%pos(2).gt.h/2) then
                    bl=bl+1
                end if
            else if (robots(i)%pos(1).gt.w/2) then
                !print*,"right"
                if (robots(i)%pos(2).lt.h/2) then
                    tr=tr+1
                else if (robots(i)%pos(2).gt.h/2) then
                    br=br+1
                end if
            end if
        end do
        !print*,"top left",tl
        !print*,"top right",tr
        !print*,"bottom left",bl
        !print*,"bottom right",br
        ans1=tl*tr*bl*br
    end subroutine part1

    subroutine make_grid(grid,robots,siz,w,h)
        integer :: w,h,siz,i
        character :: grid(h,w)
        type(robot) :: robots(siz)
        grid=" "
        do i=1,siz
            grid(robots(i)%pos(1)+1,robots(i)%pos(2)+1)="X"
            !print*,robots(i)%pos
        end do
    end subroutine make_grid

    subroutine part2(robots,siz,ans2,w,h)
        integer :: siz,ans2,w,h,i,j,jump,k,r
        type(robot)::robots(siz)
        character :: grid(h,w)
        logical :: go

        go=.true.
        i=0
        jump=1
        !print*,"hi"
        do while (go)
            !print*,i
            call move(robots,siz,jump)
            do j=1,siz
                robots(j)%pos(1)=mod(robots(j)%pos(1),w)
                robots(j)%pos(2)=mod(robots(j)%pos(2),h)
                if (robots(j)%pos(1).lt.0) then
                    robots(j)%pos(1)=w+robots(j)%pos(1)
                end if
                if (robots(j)%pos(2).lt.0) then
                    robots(j)%pos(2)=h+robots(j)%pos(2)
                end if
            end do
            !print*,"moved"
            call make_grid(grid,robots,siz,w,h)
            !print*,"grid"
            i=i+jump
            do j=1,h
                do k=1,w-7
                    if (count(grid(j,k:k+7).eq."X").ge.8) then
                        go=.false.
                    end if
                end do
            end do
        end do
        ans2=i
        do j=1,h
            print*,grid(j,:)
        end do
        print*,ans2
    end subroutine part2
end program restroom_redoubt