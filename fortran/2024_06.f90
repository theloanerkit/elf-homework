program day06
    use reader
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_06")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character :: data(file_len,line_len)
        integer :: ans1, ans2,i
        ans1 = 0
        ans2 = 0
        data=grid_reader(file_len,line_len)
        do i=1,file_len
            print*,data(i,:)
        end do
        print*,"--------"
        call part1(data,file_len,line_len,ans1,.false.)
        call part2(data,file_len,line_len,ans2)
        !do i=1,file_len
        !    print*,data(i,:)
        !end do
        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function pair(x,y) result (ans)
        integer :: x,y,ans
        ans = (0.5*(x+y)*(x+y+1))+y
    end function pair

    subroutine part1(grid,file_len,line_len,ans1,l)
        integer :: ans1,file_len,line_len
        character :: grid(file_len,line_len)
        integer :: visited(file_len*line_len),vi,temp,temp2(1),loop(file_len*line_len)
        integer :: dirn(2),pos(2),nextpos(2),row,col,i,step,li,up(2),start(2)
        integer :: corners(10000),prevcorners(40),ci,cidx(1),cidx2(1)
        logical :: go,l,die
        corners = 0
        ci=1
        prevcorners=0
        visited=0
        loop=0
        pos=findloc(grid,"^")
        start=findloc(grid,"^")
        grid(pos(1),pos(2))="."
        dirn=(/-1,0/)
        go=.true.
        vi=1
        step=1
        li=1
        up=0
        die=.false.
        !print*,pos
        do while (go)
            nextpos(1) = pos(1)+dirn(1)
            nextpos(2) = pos(2)+dirn(2)
            !print*,pos,nextpos,dirn
            if (nextpos(1).eq.0.or.nextpos(1).eq.file_len+1.or.nextpos(2).eq.0.or.nextpos(2).eq.line_len+1) then
                ! off grid, done
                go=.false.
            else if (grid(nextpos(1),nextpos(2)).eq.".".or.grid(nextpos(1),nextpos(2)).eq."X") then
                !print*,"empty space"
                grid(pos(1),pos(2))="X"
                pos = nextpos
                temp = pair(pos(1),pos(2))
                temp2 = findloc(visited,temp)
                if (temp2(1).eq.0) then
                    visited(vi)=temp
                    vi=vi+1
                    !ans1=ans1+1
                end if
            else if (grid(nextpos(1),nextpos(2)).eq."#") then
                !print*,"wall"
                !if (l) then
                !    if (dirn(1).eq.-1.and.dirn(2).eq.0) then
                !        if (pos(1).eq.up(1).and.pos(2).eq.up(2)) then
                !            ans1=0
                !            go=.false.
                !        else
                !            up=pos
                !        end if
                !    end if
                !end if
                if (dirn(1).eq.1.or.dirn(1).eq.-1) then
                    col = -dirn(1)
                    dirn(1)=0
                    dirn(2)=col
                else if (dirn(2).eq.1.or.dirn(2).eq.-1) then
                    row = dirn(2)
                    dirn(2) = 0
                    dirn(1) = row
                end if
                if (l) then
                    !print*,corners
                    corners(ci)=pair(pos(1),pos(2))
                    cidx=findloc(corners,corners(ci))
                    
                   ! print*,ci,cidx
                    !if (ci.ne.cidx(1)) then
                    !    corners(cidx(1))=-1
                    !cidx2=findloc(corners,corners(ci))
                    !print*,ci,cidx,cidx2
                    if (cidx(1).ne.ci) then
                        if (mod(ci-cidx(1),4).eq.0) then!.and.mod(ci-cidx2(1),4).eq.0) then
                            go=.false.
                            ans1=0
                            die=.true.
                        end if
                    end if
                    !corners(cidx(1))=corners(ci)
                    !end if
                    ci=ci+1
                end if
                grid(pos(1),pos(2))="O"
                !print*,"----",pos
                !do i=1,file_len
                !    print*,grid(i,:)
                !end do
                !print*," "
                step=step+1
                !if (step.eq.39) then
                !    go=.false.
                !end if
                grid(pos(1),pos(2))="."
                !print*,"done"
                !print*,pos
                !pos = nextpos
                !if (dirn.eq.(/-1,0/)) then
                !    dirn=(/0,1/)
                !else if (dirn.eq.(/0,1/)) then
                !    dirn=(/1,0/)
                !else if (dirn.eq.(/1,0/)) then
                !    dirn=(/0,-1/)
                !else if (dirn.eq.(/0,-1/)) then
                !    dirn=(/-1,0/)
                !end if
            end if
            !if (vi.eq.20) then
            !    go=.false.
            !end if
        end do
        grid(pos(1),pos(2))="X"
        if (.not.l.or..not.die) then
            !do i=1,file_len
            !    print*,grid(i,:)
            !    
            !end do
            !print*,""
            ans1=count(grid.eq."X")
        end if
        grid(start(1),start(2))="^"
    end subroutine part1

    subroutine part2(grid,file_len,line_len,ans2)
        integer :: file_len,line_len,ans2,i,j,pos(2),t,k
        character :: grid(file_len,line_len)
        pos=findloc(grid,"^")
        print*,pos
        do i=1,file_len
            print*,i,"/",file_len
            do j=1,line_len
                if (i.ne.pos(1).or.j.ne.pos(2)) then
                    if (grid(i,j).ne."#") then
                        !print*,i,j
                        grid(i,j)="#"
                        !do k=1,file_len
                        !    print*,grid(k,:)
                        !end do
                        !print*,"-----"
                        call part1(grid,file_len,line_len,t,.true.)
                        !if (i.eq.7.and.j.eq.4) then
                        !    print*,t
                        !end if
                        grid(i,j)="."
                        if (t.eq.0) then
                            !print*,"obstacle",i,j
                            ans2=ans2+1
                        end if
                    end if
                end if
            end do
        end do
    end subroutine part2

    subroutine part3(grid,file_len,line_len,ans2)
        integer :: file_len,line_len,ans2,col,row
        integer :: obs,idx(2),i,a(2),b(2),c(2),d(2),t(1),j,bs(2),ds(2)
        character :: grid(file_len,line_len)
        obs=count(grid.eq."X")
        do j=1,file_len
            print*,grid(j,:)
        end do
        print*,"*"
        do i=1,obs
            idx=findloc(grid,"X")
            grid(idx(1),idx(2))="."
        end do
        do j=1,file_len
            print*,grid(j,:)
        end do
        obs=count(grid.eq."#")
        print*,obs
        do i=1,obs
            print*,i,"/",obs
            a=findloc(grid,"#")
            !if (a(1)+3.gt.file_len.or.a(2).eq.1.or.a(2)+2.gt.line_len) then
            !    grid(a(1),a(2))="O"
            !else
                b(1) = a(1)+1
                t=findloc(grid(b(1),a(2):),"#")+a(2)-1
                !print*,"grid: ",grid(b(1),a(2):),t
                if (t(1).eq.a(2)-1) then
                    t=findloc(grid(b(1),a(2):),"O")+a(2)-1
                end if
                if (t(1).eq.a(2)-1) then
                    b=(/0,0/)
                else 
                    b(2)=t(1)
                end if

                d(2) = a(2)-1
                t = findloc(grid(a(1):,d(2)),"#")+a(1)-1
                if (t(1).eq.a(1)-1) then
                    t=findloc(grid(a(1):,d(2)),"O")+a(1)-1
                end if
                if (t(1).eq.a(1)-1) then
                    d=(/0,0/)
                else 
                    d(1)=t(1)
                end if
                if (b(1).eq.0) then
                    print*,"no b, find c?"
                    if (d(1).ne.0) then
                        c(1) = d(1)+1
                        t = findloc(grid(c(1),d(2):),"#")+d(2)-1
                        if (t(1).eq.d(2)-1) then
                            t = findloc(grid(c(1),d(2):),"O")+d(2)-1
                        end if
                        !print*,t,d(2)-1
                        if (t(1).eq.d(2)-1) then
                            c=(/0,0/)
                            print*,"   no c"
                        else
                            print*,"   c found"
                            row=a(1)+1
                            col=t(1)
                            c=(/row,col/)
                            b=(/a(1)+1,c(2)+1/)
                            print*,b
                            if (grid(b(1),b(2)).eq.".") then
                                
                                if (all(grid(a(1)+1,a(2):b(2)).eq.".")) then
                                    ans2=ans2+1
                                    print*,"   can make a loop with",b
                                end if
                                !ans2=ans2+1
                            end if
                        end if
                    else
                        print*,"   no d either"
                    end if
                else if (d(1).eq.0) then
                    print*,"no d, find c?"
                    if (b(1).ne.0) then
                        c(2) = b(2)-1
                        t = findloc(grid(b(1):,c(2)),"#")+b(1)-1
                        if (t(1).eq.b(1)-1) then
                            t = findloc(grid(b(1):,c(2)),"O")+b(1)-1
                        end if
                        if (t(1).eq.b(1)-1) then
                            c=(/0,0/)
                            print*,"   no c"
                        else 
                            print*,"   c found"
                            row = t(1)
                            col = b(2)-1
                            c=(/row,col/)
                            d=(/c(1)-1,a(2)-1/)
                            print*,d
                            if (grid(d(1),d(2)).eq.".") then
                                !print*,"   can make a loop with",d
                                !ans2=ans2+1
                                if (all(grid(a(1)+1:d(1),a(2)).eq.".")) then
                                    ans2=ans2+1
                                    print*,"   can make a loop with",d
                                end if
                            end if
                        end if
                    else
                        print*,"   no b either"
                    end if
                else
                    print*,"both b and d, does c exist?"
                    col = b(2)-1
                    row = d(1)+1
                    bs=b
                    ds=d
                    if (grid(row,col).eq.".") then
                        print*,"can make a loop with",row,col
                        ans2=ans2+1
                    end if
                    ! another option
                    print*,"finding c from d, loop with b?"
                    c(1) = d(1)+1
                    t = findloc(grid(c(1),d(2):),"#")+d(2)-1
                    if (t(1).eq.d(2)-1) then
                        t = findloc(grid(c(1),d(2):),"O")+d(2)-1
                    end if
                    !print*,t,d(2)-1
                    if (t(1).eq.d(2)-1) then
                        c=(/0,0/)
                        print*,"   no c"
                    else
                        print*,"   c found"
                        row=a(1)+1
                        col=t(1)
                        c=(/row,col/)
                        b=(/a(1)+1,c(2)+1/)
                        print*,b
                        if (grid(b(1),b(2)).eq.".") then
                            
                            if (all(grid(a(1)+1,a(2):b(2)).eq.".")) then
                                ans2=ans2+1
                                print*,"   can make a loop with",b
                            end if
                        end if
                    end if
                    ! another nother option
                    b=bs
                    d=ds
                    print*,"finding c from b, loop with d?"
                    c(2) = b(2)-1
                    t = findloc(grid(b(1):,c(2)),"#")+b(1)-1
                    if (t(1).eq.b(1)-1) then
                        t = findloc(grid(b(1):,c(2)),"O")+b(1)-1
                    end if
                    if (t(1).eq.b(1)-1) then
                        c=(/0,0/)
                        print*,"   no c"
                    else 
                        print*,"   c found"
                        row = t(1)
                        col = b(2)-1
                        c=(/row,col/)
                        d=(/c(1)-1,a(2)-1/)
                        print*,a,b,c,d
                        if (grid(d(1),d(2)).eq.".") then
                            
                            if (all(grid(a(1)+1:d(1),a(2)).eq.".")) then
                                ans2=ans2+1
                                print*,"   can make a loop with",d
                            end if
                        end if
                    end if
                end if
                do j=1,file_len
                    print*,grid(j,:)
                end do
                print*,"---"
                grid(a(1),a(2))="O"
                !else
                !    b(2) = t(1)
                !    c(2) = b(2)-1
                !    t = findloc(grid(b(1):,c(2)),"#")+b(1)-1
                !    print*,"t",findloc(grid(b(1):,c(2)),"#"),b(1)-1
                !    if (t(1).eq.b(1)-1) then
                !        t=findloc(grid(b(1):,c(2)),"O")+b(1)-1
                !    end if
                !    if (t(1).eq.b(1)-1) then
                !    else
                !        c(1) = t(1)
                !        print*,a,grid(a(1),a(2))
                !        print*,b,grid(b(1),b(2))
                !        print*,c,grid(c(1),c(2))
                !        print*," "
                !    end if
                !    
                !end if
            !end if
           ! print*,idx
        end do
    end subroutine part3
end program day06