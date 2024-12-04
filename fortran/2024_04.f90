program ceres_search
    use reader
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_04")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    function word_found(grid,file_len,line_len,i,j,dist) result (words)
        integer :: file_len,line_len,i,j,words,k,dist,t
        character :: grid(file_len,line_len)
        integer :: dirns(8,2) = reshape((/1,0,-1,-1,-1,0,1,1,&
        & 1,1,1,0,-1,-1,-1,0/),(/8,2/))
        words=0
        do k=1,8
            if (i+dist*dirns(k,1).gt.0.and.i+dist*dirns(k,1).le.file_len) then
                if (j+dist*dirns(k,2).gt.0.and.j+dist*dirns(k,2).le.line_len) then
                    if (dist.eq.3) then
                        words = words + word_check(grid,file_len,line_len,i,j,dirns(k,:),(/1,2,3/))
                    else
                        if (.not.(any(dirns(k,:).eq.0))) then
                            t = word_check(grid,file_len,line_len,i,j,dirns(k,:),(/0,1,2/))
                            if (extra(grid,file_len,line_len,i,j,dirns(k,:)).and.t.eq.1) then
                                words = words + t
                            end if
                        end if
                    end if
                end if 
            end if
        end do
    end function word_found

    function word_check(grid,file_len,line_len,i,j,dirn,mul) result (good)
        integer :: good,file_len,line_len,i,j,dirn(2),mul(3)
        character :: grid(file_len,line_len)
        character :: m,a,s
        m = grid(i+mul(1)*dirn(1),j+mul(1)*dirn(2))
        a = grid(i+mul(2)*dirn(1),j+mul(2)*dirn(2))
        s = grid(i+mul(3)*dirn(1),j+mul(3)*dirn(2))
        if (m.eq."M".and.a.eq."A".and.s.eq."S") then
            good = 1
        else 
            good = 0
        end if
    end function word_check

    function extra(grid,file_len,line_len,i,j,dirn) result (yes)
        integer :: file_len,line_len,i,j,dirn(2)
        integer :: i1,i2,j1,j2
        character :: grid(file_len,line_len)
        logical :: yes
        yes=.false.
        i1 = i+2*dirn(1)
        j1 = j
        i2 = i
        j2 = j+2*dirn(2)
        if (grid(i1,j1).eq."M".and.grid(i2,j2).eq."S") then
            yes = .true.
        else if (grid(i1,j1).eq."S".and.grid(i2,j2).eq."M") then
            yes = .true.
        end if
    end function extra

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character :: data(file_len,line_len)
        integer :: ans1, ans2,i
        ans1 = 0
        ans2 = 0
        data = grid_reader(file_len,line_len)
        call part1(data,file_len,line_len,ans1)
        call part2(data,file_len,line_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine part1(grid,file_len,line_len,ans1)
        integer :: file_len,line_len,ans1,i,j,w
        character :: grid(file_len,line_len)
        do i=1,file_len
            do j=1,line_len
                if (grid(i,j).eq."X") then
                    w = word_found(grid,file_len,line_len,i,j,3)
                    ans1 = ans1 + w
                end if
            end do
        end do
    end subroutine part1

    subroutine part2(grid,file_len,line_len,ans2)
        integer :: file_len,line_len,ans2,i,j,w
        character :: grid(file_len,line_len)
        do i=1,file_len
            do j=1,line_len
                if (grid(i,j).eq."M") then
                    w = word_found(grid,file_len,line_len,i,j,2)
                    ans2 = ans2 + w
                end if
            end do
        end do
        ans2=ans2/2
    end subroutine part2
end program ceres_search