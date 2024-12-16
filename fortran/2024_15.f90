program day15
    use reader
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_15")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len,50,50)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len,row,col)
        integer :: file_len, line_len,row,col,i,j
        character :: data(row,col),bigdata(row,col*2)
        character(line_len) :: instructions(file_len-row)
        !character :: line_len
        integer :: ans1, ans2
        ans1 = 0
        ans2 = 0
        data=grid_reader(row,col)
        instructions=string_reader(file_len-row,line_len)
        do i=1,row
            do j=1,col*2,2
                !print*,j,j/2,data(i,j/2)
                if (data(i,j/2+1).eq."#") then
                    bigdata(i,j:j+1)=(/"#","#"/)
                else if (data(i,j/2+1).eq."O") then
                    bigdata(i,j:j+1)=(/"[","]"/)
                else if (data(i,j/2+1).eq.".") then
                    bigdata(i,j:j+1)=(/".","."/)
                else if (data(i,j/2+1).eq."@") then
                    bigdata(i,j:j+1)=(/"@","."/)
                end if
            end do
            !print*,bigdata(i,:)
        end do
        !print*,row,col,col*2
        !print*,instructions(2)

        call part1(data,row,col,instructions(2),line_len,ans1)
        call part2(bigdata,row,col*2,instructions(2),line_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine move(grid,row,col,dirn)
        integer :: row,col,pos(2),npos(2)
        character :: grid(row,col),dirn
        pos = findloc(grid,"@")
        if (dirn.eq."<") then
            npos=(/pos(1),pos(2)-1/)
        else if (dirn.eq."^") then
            npos=(/pos(1)-1,pos(2)/)
        else if (dirn.eq.">") then
            npos=(/pos(1),pos(2)+1/)
        else if (dirn.eq."v") then
            npos=(/pos(1)+1,pos(2)/)
        end if

        if (grid(npos(1),npos(2)).eq.".") then
            grid(npos(1),npos(2))="@"
            grid(pos(1),pos(2))="."
        else if (grid(npos(1),npos(2)).eq."O") then
            !print*,"we've hit something"
            call move_boxes(grid,row,col,dirn,npos)
            if (grid(npos(1),npos(2)).eq.".") then
                grid(npos(1),npos(2))="@"
                grid(pos(1),pos(2))="."
            end if
        end if
    end subroutine move

    subroutine move2(grid,row,col,dirn)
        integer :: row,col,pos(2),npos(2)
        character :: grid(row,col),dirn
        pos = findloc(grid,"@")
        if (dirn.eq."<") then
            npos=(/pos(1),pos(2)-1/)
        else if (dirn.eq."^") then
            npos=(/pos(1)-1,pos(2)/)
        else if (dirn.eq.">") then
            npos=(/pos(1),pos(2)+1/)
        else if (dirn.eq."v") then
            npos=(/pos(1)+1,pos(2)/)
        end if

        if (grid(npos(1),npos(2)).eq.".") then
            grid(npos(1),npos(2))="@"
            grid(pos(1),pos(2))="."
        else if (grid(npos(1),npos(2)).eq."[".or.grid(npos(1),npos(2)).eq."]") then
            !print*,"we've hit something"
            call move_boxes2(grid,row,col,dirn,npos,.true.)
            if (grid(npos(1),npos(2)).eq.".") then
                grid(npos(1),npos(2))="@"
                grid(pos(1),pos(2))="."
            end if
        end if
    end subroutine move2

    recursive subroutine move_boxes(grid,row,col,dirn,boxpos)
        integer :: row,col,boxpos(2),npos(2)
        character :: grid(row,col),dirn
        !print*,boxpos
        if (dirn.eq."<") then
            npos=(/boxpos(1),boxpos(2)-1/)
        else if (dirn.eq."^") then
            npos=(/boxpos(1)-1,boxpos(2)/)
        else if (dirn.eq.">") then
            npos=(/boxpos(1),boxpos(2)+1/)
        else if (dirn.eq."v") then
            npos=(/boxpos(1)+1,boxpos(2)/)
        end if
        if (grid(npos(1),npos(2)).eq.".") then
            grid(npos(1),npos(2))="O"
            grid(boxpos(1),boxpos(2))="."
        else if (grid(npos(1),npos(2)).eq."O") then
            call move_boxes(grid,row,col,dirn,npos)
            if (grid(npos(1),npos(2)).eq.".") then
                grid(npos(1),npos(2))="O"
                grid(boxpos(1),boxpos(2))="."
            end if
        end if
    end subroutine move_boxes

    recursive subroutine move_boxes2(grid,row,col,dirn,boxpos,both)
        integer :: row,col,boxpos(2),npos(2),lpos(2),rpos(2),bpl(2),bpr(2)
        character :: grid(row,col),dirn,oldgrid(row,col)
        logical :: both,good
        !print*,boxpos
        !both=.true.
        good=.true.
        oldgrid=grid
        !print*,dirn,boxpos
        if (dirn.eq."<") then
            npos=(/boxpos(1),boxpos(2)-1/)
        else if (dirn.eq."^") then
            npos=(/boxpos(1)-1,boxpos(2)/)
        else if (dirn.eq.">") then
            npos=(/boxpos(1),boxpos(2)+1/)
        else if (dirn.eq."v") then
            npos=(/boxpos(1)+1,boxpos(2)/)
        end if
        if (dirn.eq."<".or.dirn.eq.">") then
            if (grid(npos(1),npos(2)).eq.".") then
                grid(npos(1),npos(2))=grid(boxpos(1),boxpos(2))
                grid(boxpos(1),boxpos(2))="."
            else if (grid(npos(1),npos(2)).eq."[".or.grid(npos(1),npos(2)).eq."]") then
                call move_boxes2(grid,row,col,dirn,npos,.true.)
                if (grid(npos(1),npos(2)).eq.".") then
                    grid(npos(1),npos(2))=grid(boxpos(1),boxpos(2))
                    grid(boxpos(1),boxpos(2))="."
                end if
            end if
        else if (both) then
            !print*,"up down"
            if (grid(boxpos(1),boxpos(2)).eq."]") then
                rpos=npos
                lpos=(/npos(1),npos(2)-1/)
                bpr=boxpos
                bpl=(/boxpos(1),boxpos(2)-1/)
            else
                lpos=npos
                rpos=(/npos(1),npos(2)+1/)
                bpl=boxpos
                bpr=(/boxpos(1),boxpos(2)+1/)
            end if
            !print*,lpos,rpos
            !print*,bpl,bpr
            !print*,grid(lpos(1),lpos(2)),grid(rpos(1),rpos(2))
            !print*,""
            if (grid(lpos(1),lpos(2)).eq.".".and.grid(rpos(1),rpos(2)).eq.".") then
                grid(lpos(1),lpos(2))=grid(bpl(1),bpl(2))
                grid(rpos(1),rpos(2))=grid(bpr(1),bpr(2))
                grid(bpl(1),bpl(2))="."
                grid(bpr(1),bpr(2))="."
            else if ((grid(lpos(1),lpos(2)).eq."[".or.grid(lpos(1),lpos(2)).eq."]".or.&
                &grid(rpos(1),rpos(2)).eq."[".or.grid(rpos(1),rpos(2)).eq."]").and.&
                &grid(lpos(1),lpos(2)).ne."#".and.grid(rpos(1),rpos(2)).ne."#") then
                !print*,grid(lpos(1),lpos(2)),grid(rpos(1),rpos(2))
                if (grid(rpos(1),rpos(2)).eq.".".or.grid(rpos(1),rpos(2)).eq."]") then
                    !print*,"checking left"
                    call move_boxes2(grid,row,col,dirn,lpos,both)
                else if (grid(lpos(1),lpos(2)).eq.".".or.grid(lpos(1),lpos(2)).eq."[") then
                    !print*,"checking right"
                    call move_boxes2(grid,row,col,dirn,rpos,both)
                else 
                    !print*,"checking both"
                    call move_boxes2(grid,row,col,dirn,lpos,both)
                    call move_boxes2(grid,row,col,dirn,rpos,both)
                end if
                if (grid(lpos(1),lpos(2)).eq.".".and.grid(rpos(1),rpos(2)).eq."[") then
                    !print*,"can't move like that 1"
                    !grid(bpl(1)-1,bpl(2))=grid(lpos(1)-1,lpos(2))
                    !grid(bpl(1)-1,bpl(2)-1)=grid(lpos(1)-1,lpos(2)-1)
                    !grid(lpos(1)-1,lpos(2))="."
                    !grid(lpos(1)-1,lpos(2)-1)="."
                    grid=oldgrid
                end if
                if (grid(lpos(1),lpos(2)).eq."]".and.grid(rpos(1),rpos(2)).eq.".") then
                    !print*,"can't move like that 2"
                    !grid(bpr(1)-1,bpr(2))=grid(rpos(1)-1,rpos(2))
                    !grid(bpr(1)-1,bpr(2)+1)=grid(rpos(1)-1,rpos(2)+1)
                    !grid(rpos(1)-1,rpos(2))="."
                    !grid(rpos(1)-1,rpos(2)+1)="."
                    grid=oldgrid
                end if
                
                if (grid(lpos(1),lpos(2)).eq.".".and.grid(rpos(1),rpos(2)).eq.".") then
                    grid(lpos(1),lpos(2))=grid(bpl(1),bpl(2))
                    grid(rpos(1),rpos(2))=grid(bpr(1),bpr(2))
                    grid(bpl(1),bpl(2))="."
                    grid(bpr(1),bpr(2))="."
                end if
            else
                !both=.false.
                good=.false.
            end if
        else
        end if

    end subroutine move_boxes2

    subroutine part1(grid,row,col,ins,line_len,ans1)
        integer :: row,col,line_len,ans1,i,j
        character :: grid(row,col)
        character(line_len) :: ins

        do i=1,row
            print*,grid(i,:)
        end do
        !print*,ins
        do i=1,line_len
            call move(grid,row,col,ins(i:i))
        end do
        do i=1,row
            print*,grid(i,:)
        end do
        do i=1,row
            do j=1,col
                if (grid(i,j).eq."O") then
                    ans1=ans1+(100*(i-1))+j-1
                end if
            end do
        end do

    end subroutine part1

    subroutine part2(grid,row,col,ins,line_len,ans2)
        integer :: row,col,line_len,ans2,i,j
        character :: grid(row,col)
        character(line_len) :: ins

        do i=1,row
            print*,grid(i,:)
        end do
        !print*,ins
        print*,"hi",ins
        do i=1,line_len
            call move2(grid,row,col,ins(i:i))
            !do j=1,row
            !    print*,grid(j,:)
            !end do
            print*,i
        end do
        print*,"done"
        do i=1,row
            print*,grid(i,:)
        end do
        do i=1,row
            do j=1,col
                if (grid(i,j).eq."[") then
                    ans2=ans2+(100*(i-1))+j-1
                end if
            end do
        end do
    end subroutine part2
end program day15
! 1594237 too high
! 1575877