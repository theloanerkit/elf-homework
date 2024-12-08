program resonant_collinearity
    use reader
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_08")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character :: data(file_len,line_len),grid(file_len,line_len)
        integer :: ans1, ans2,i,a_types
        ans1 = 0
        ans2 = 0
        data=grid_reader(file_len,line_len)
        a_types=count_antenna_types(data,file_len,line_len)
        grid=data
        call part1(data,file_len,line_len,a_types,ans1)
        call part2(grid,file_len,line_len,a_types,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function count_antenna_types(grid,file_len,line_len) result(num)
        integer :: num, file_len,line_len,i,j,idx
        character :: grid(file_len,line_len),seen(file_len*line_len)
        seen=""
        num=0
        idx=1
        do i=1,file_len
            do j=1,line_len
                if (.not.any(seen.eq.grid(i,j)).and.grid(i,j).ne.".") then
                    seen(idx)=grid(i,j)
                    idx=idx+1
                    num=num+1
                end if
            end do
        end do
    end function count_antenna_types

    subroutine get_antenna(grid,file_len,line_len,num,antenna)
        integer :: file_len,line_len,num,i,j,idx
        character :: grid(file_len,line_len),antenna(num)
        idx=1
        do i=1,file_len
            do j=1,line_len
                if (.not.any(antenna.eq.grid(i,j)).and.grid(i,j).ne.".") then
                    antenna(idx) = grid(i,j)
                    idx=idx+1
                end if
            end do
        end do
    end subroutine get_antenna

    subroutine get_antenna_pos(grid,file_len,line_len,label,count,positions)
        integer :: file_len,line_len,count,i,pos(2)
        character :: grid(file_len,line_len),label
        integer :: positions(count,2)
        do i=1,count
            pos=findloc(grid,label)
            positions(i,:)=pos
            grid(pos(1),pos(2))="."
        end do
    end subroutine get_antenna_pos

    subroutine find_antinode_pos(a1,a2,anodes)
        integer :: a1(2),a2(2),anodes(2,2),h(2),l(2)
        anodes=0
        if (a1(2).gt.a2(2)) then
            h = a2
            l=a1
        else
            h=a1
            l=a2
        end if
        anodes(1,2) = h(2) - abs(h(2)-l(2))
        anodes(1,1) = h(1) - (l(1)-h(1))
        anodes(2,2) = l(2) + abs(h(2)-l(2))
        anodes(2,1) = l(1) - (h(1)-l(1))
    end subroutine find_antinode_pos

    subroutine find_all_antinode_pos(a1,a2,file_len,line_len,anode_grid)
        integer :: a1(2),a2(2),file_len,line_len,h(2),l(2),m,anode(2)
        character :: anode_grid(file_len,line_len)
        logical :: hunt
        hunt=.true.
        if (a1(2).gt.a2(2)) then
            h = a2
            l=a1
        else
            h=a1
            l=a2
        end if
        m=0
        do while(hunt)
            anode(2) = h(2) - m*abs(h(2)-l(2))
            anode(1) = h(1) - m*(l(1)-h(1))
            if (anode(1).gt.0.and.anode(1).le.file_len) then
                if (anode(2).gt.0.and.anode(2).le.line_len) then
                    anode_grid(anode(1),anode(2))="X"
                    m=m+1
                else
                    hunt=.false.
                end if
            else 
                hunt=.false.
            end if
        end do
        hunt=.true.
        m=0
        do while(hunt)
            anode(2) = l(2) + m*abs(h(2)-l(2))
            anode(1) = l(1) - m*(h(1)-l(1))
            if (anode(1).gt.0.and.anode(1).le.file_len) then
                if (anode(2).gt.0.and.anode(2).le.line_len) then
                    anode_grid(anode(1),anode(2))="X"
                    m=m+1
                else
                    hunt=.false.
                end if
            else 
                hunt=.false.
            end if
        end do
    end subroutine find_all_antinode_pos

    subroutine find_antinodes(grid,file_len,line_len,label,count,anode_grid,part)
        integer :: num,file_len,line_len,count,i,j,k,idx,part
        character :: grid(file_len,line_len),label
        character :: anode_grid(file_len,line_len)
        integer :: positions(count,2),anodes(2,2)
        positions=0
        call get_antenna_pos(grid,file_len,line_len,label,count,positions)
        num=0
        idx=1
        do i=1,count
            do j=i+1,count
                if (part.eq.1) then
                    call find_antinode_pos(positions(i,:),positions(j,:),anodes)
                    do k=1,2
                        if (anodes(k,1).gt.0.and.anodes(k,1).le.file_len) then
                            if (anodes(k,2).gt.0.and.anodes(k,2).le.line_len) then
                                anode_grid(anodes(k,1),anodes(k,2))="X"
                            end if
                        end if
                    end do
                else
                    call find_all_antinode_pos(positions(i,:),positions(j,:),file_len,line_len,anode_grid)
                end if
            end do
        end do
    end subroutine find_antinodes

    subroutine part1(grid,file_len,line_len,num,ans1)
        integer :: file_len,line_len,num,ans1,i,c
        character :: grid(file_len,line_len),antenna(num)
        character :: anode_grid(file_len,line_len)
        anode_grid="."
        call get_antenna(grid,file_len,line_len,num,antenna)
        do i=1,num
            c=count(grid.eq.antenna(i))
            call find_antinodes(grid,file_len,line_len,antenna(i),c,anode_grid,1)
        end do
        ans1=count(anode_grid.eq."X")
    end subroutine part1

    subroutine part2(grid,file_len,line_len,num,ans2)
        integer :: file_len,line_len,num,ans2,i,c
        character :: grid(file_len,line_len),antenna(num)
        character :: anode_grid(file_len,line_len)
        anode_grid="."
        call get_antenna(grid,file_len,line_len,num,antenna)
        do i=1,num
            c=count(grid.eq.antenna(i))
            call find_antinodes(grid,file_len,line_len,antenna(i),c,anode_grid,2)
        end do
        ans2=count(anode_grid.eq."X")
    end subroutine part2
end program resonant_collinearity