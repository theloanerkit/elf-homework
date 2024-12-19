program day16
    use reader
    implicit none
    integer :: file_len, line_len
    integer,parameter :: dirn(4,2)=reshape((/0,1,0,-1,1,0,-1,0/),(/4,2/))
    double precision :: ts,tf
    type :: node
        integer :: pos(3)
        integer :: score
        integer :: previous(12,3)=0
    end type node
    call cpu_time(ts)
    open(1,file="2024_16")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len,i,c,j
        character :: data(file_len,line_len)
        integer :: ans1, ans2
        integer,allocatable :: visited_idx(:)
        type(node),allocatable :: visited(:)
        !type(node),allocatable :: graph(:)
        
        ans1 = 0
        ans2 = 0
        data=grid_reader(file_len,line_len)
        do i=1,file_len
            print*,data(i,:)
        end do
        c=count(data.eq.".")
        
        call part1(data,file_len,line_len,ans1,visited,visited_idx)
        !print*,size(visited),size(visited_idx)
        call part2(visited,visited_idx,size(visited),ans2,findloc(data,"S"),findloc(data,"E"),ans1,data,file_len,line_len)
        do i=1,file_len
            do j=1,line_len
                if (data(i,j).eq."#") then
                    !data(i,j)="■"
                else if (data(i,j).eq.".") then
                    data(i,j)=" "
                end if
            end do
        end do
        ans2=count(data.eq."O")+1
        do i=1,file_len
            print*,data(i,:)
        end do
        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function pair(x,y) result (ans)
        integer :: x,y,ans
        ans = ((x+y)*(x+y+1)/2)+y
    end function pair

    subroutine add_to_queue(current,f,score,queue_idx,queue)
        integer :: pos(2),f,score,idx,i(1),k(2)
        integer,allocatable :: qt_idx(:),queue_idx(:)
        type(node),allocatable :: t(:),queue(:)
        type(node) :: current,next

        pos(1)=current%pos(1)+dirn(f,1)
        pos(2)=current%pos(2)+dirn(f,2)
        next%pos=(/pos(1),pos(2),f/)
        next%score=current%score+score
        idx=pair(pair(pos(1),pos(2)),f)
        i=findloc(queue_idx,idx)
        if (pos(1).eq.14.and.pos(2).eq.92) then
            print*,"here with score",next%score,"from",current%pos
        end if
        if (i(1).eq.0) then
            k=findloc(next%previous,0)
            next%previous(k(1),:)=current%pos
            allocate(t(size(queue)+1))
            t(:size(queue))=queue
            t(size(queue)+1)=next
            deallocate(queue)
            allocate(queue(size(t)))
            queue=t
            deallocate(t)
            allocate(qt_idx(size(queue_idx)+1))
            qt_idx(:size(queue_idx))=queue_idx
            qt_idx(size(queue_idx)+1)=idx
            deallocate(queue_idx)
            allocate(queue_idx(size(qt_idx)))
            queue_idx=qt_idx
            deallocate(qt_idx)
        else if (next%score.lt.queue(i(1))%score) then
            !k=findloc(queue(i(1))%previous,0)
            queue(i(1))%previous=0
            queue(i(1))%previous(1,:)=current%pos
            queue(i(1))%score=next%score
        else if (next%score.eq.queue(i(1))%score) then
            !print*,"two routes"
            k=findloc(queue(i(1))%previous,0)
            queue(i(1))%previous(k(1),:)=current%pos
            !print*,current%po
        end if

    end subroutine add_to_queue
    
    subroutine move(visited,visited_idx,current,row,col,grid,queue,queue_idx)
        integer :: row,col,f
        integer,allocatable :: visited_idx(:),t_idx(:),queue_idx(:)
        type(node),allocatable :: visited(:),queue(:),vis_t(:)
        type(node) :: current,next
        character :: grid(row,col)
        logical :: go,test
        go=.false.
        !print*,"at",current%pos,"from",current%previous(1,:),current%previous(2,:),current%previous(3,:)
        f = current%pos(3)
        test=.false.
        go = move_forward(current,f,grid,row,col,1,visited,visited_idx)
        if (go) then
            call add_to_queue(current,f,1,queue_idx,queue)
        end if
        f = mod(f,4)+1
        go=.false.
        go = move_forward(current,f,grid,row,col,1001,visited,visited_idx)
        if (go) then
            call add_to_queue(current,f,1001,queue_idx,queue)
        end if
        f = mod(f,4)+2
        if (f.gt.4) then
            f=mod(f,4)
        end if
        go=.false.
        go = move_forward(current,f,grid,row,col,1001,visited,visited_idx)
        if (go) then
            call add_to_queue(current,f,1001,queue_idx,queue)
        end if
        allocate(t_idx(size(visited_idx)+1))
        allocate(vis_t(size(visited)+1))
        t_idx(:size(visited_idx))=visited_idx
        vis_t(:size(visited))=visited
        t_idx(size(visited_idx)+1)=pair(pair(current%pos(1),current%pos(2)),current%pos(3))
        vis_t(size(visited)+1)=current
        deallocate(visited_idx)
        deallocate(visited)
        allocate(visited_idx(size(t_idx)))
        allocate(visited(size(vis_t)))
        visited_idx=t_idx
        visited=vis_t
        !read(*,*)
    end subroutine move

    function move_forward(current,f,grid,row,col,score,visited,visited_idx) result (go)
        integer :: pos(2),f,row,col,score,idx,loc(1),compare(4),t1(1),k(2)
        character :: grid(row,col)
        type(node) :: current
        logical :: go
        integer,allocatable :: visited_idx(:),t_idx(:)
        type(node),allocatable :: visited(:),t(:)
        go=.false.
        pos(1)=current%pos(1)+dirn(f,1)
        pos(2)=current%pos(2)+dirn(f,2)
        if (grid(pos(1),pos(2)).ne."#".and.pos(1).gt.0.and.pos(1).le.row&
        &.and.pos(2).gt.0.and.pos(2).le.col) then
            idx=pair(pair(pos(1),pos(2)),f)
            loc=findloc(visited_idx,idx)
            if (loc(1).eq.0)then!.and.current%score+score.lt.89420) then ! haven't visited already
                go=.true.
            else ! have already visited, check score
                if (visited(loc(1))%score.gt.current%score+score)then!.and.current%score+score.lt.89420)then!&
                    visited(loc(1))%score=current%score+score
                    go=.true.
                end if
            end if
        end if
        if (grid(pos(1),pos(2)).eq."E") then
            go=.false.
            print*,"reached the end with score",current%score+score
            print*,current%pos
            k=findloc(current%previous,0)
            !print*,k
            !current%previous(k(1),:)=current%pos
            !print*,current%previous(1,:)
            !print*,current%previous(2,:)
            !print*,current%previous(3,:)
            allocate(t_idx(size(visited_idx)+1))
            allocate(t(size(visited)+1))
            t_idx(:size(visited_idx))=visited_idx
            t(:size(visited)) = visited
            t_idx(size(visited_idx)+1)=idx
            t(size(visited)+1)%pos=(/pos(1),pos(2),f/)
            t(size(visited)+1)%score=current%score+score
            t(size(visited)+1)%previous(1,:)=current%pos
            !print*,t(size(visited)+1)%pos
            !print*,t(size(visited)+1)%score
            !print*,t(size(visited)+1)%previous(1,:)
            deallocate(visited_idx)
            deallocate(visited)
            allocate(visited_idx(size(t_idx)))
            allocate(visited(size(t)))
            visited_idx=t_idx
            visited=t
            deallocate(t)
            deallocate(t_idx)
            !read(*,*)
        end if
        !print*,go
    end function move_forward

    ! E = 1
    ! S = 2
    ! W = 3
    ! N = 4
    subroutine part1(grid,row,col,ans1,visited,visited_idx)
        integer :: row,col,ans1,start(3),i,fin(2),c(1)
        integer,allocatable :: visited_idx(:),queue_idx(:),qt_idx(:)
        type(node),allocatable :: visited(:),queue(:),t(:)
        type(node) :: current
        character :: grid(row,col)
        logical :: go
        go=.true.
        fin=findloc(grid,"E")
        start(:2)=findloc(grid,"S")
        start(3)=1
        current%pos=start
        current%score=0
        allocate(visited(1))
        visited(1)=current
        allocate(visited_idx(1))
        allocate(queue(0))
        allocate(queue_idx(0))
        visited_idx(1)=pair(pair(current%pos(1),current%pos(2)),current%pos(3))
        do while (go)
            call move(visited,visited_idx,current,row,col,grid,queue,queue_idx)
            if (size(queue).eq.0) then
                go=.false.
            end if
            current=queue(1)
            allocate(t(size(queue)-1))
            allocate(qt_idx(size(queue_idx)-1))
            t=queue(2:)
            qt_idx=queue_idx(2:)
            deallocate(queue)
            deallocate(queue_idx)
            allocate(queue(size(t)))
            allocate(queue_idx(size(qt_idx)))
            queue=t
            queue_idx=qt_idx
            deallocate(t)
            deallocate(qt_idx) 
        end do
        do i=1,size(visited)
            if (visited(i)%pos(1).eq.fin(1).and.visited(i)%pos(2).eq.fin(2)) then
                if (visited(i)%score.lt.ans1.or.ans1.eq.0) then
                    ans1=visited(i)%score
                end if
            end if
        end do
    end subroutine part1

    ! 58 16

    subroutine part2(visited,visited_idx,siz,ans2,s,f,ans1,grid,row,col)
        integer :: ans2,siz,s(2),f(2),ans1,i,idx(1),j,k(1),o,p,row,col
        integer :: visited_idx(siz),queue(siz),tqueue(siz),path(siz),check(siz)
        type(node) :: visited(siz),fin,current
        character :: grid(row,col)
        print*,s,f
        queue=0
        path=0
        check=0
        o=1
        print*," "
        do i=1,siz
            if (visited(i)%pos(1).eq.f(1).and.visited(i)%pos(2).eq.f(2).and.visited(i)%score.eq.ans1) then
                fin=visited(i)
            end if
            !if (visited(i)%pos(1).eq.14.and.visited(i)%pos(2).eq.93) then
            !    print*,visited(i)%previous(1,:)
            !end if
        end do
        print*,fin%pos,fin%score,fin%previous(1,:)
        current=fin
        do while (.not.(current%pos(1).eq.s(1).and.current%pos(2).eq.s(2)).or.queue(1).ne.0)
            !print*,"current",current%pos
            !print*,"previous",current%previous(1,:),current%previous(2,:)
            if (current%pos(1).eq.58.and.current%pos(2).eq.16) then
            !    print*,current%pos
            !    print*,current%previous(1,:)
            end if
            do j=1,12
                
                idx = findloc(visited_idx,pair(pair(current%previous(j,1),current%previous(j,2)),current%previous(j,3)),back=.true.)
                if (current%pos(1).eq.58.and.current%pos(2).eq.16) then
                    print*,current%previous(j,:)
                end if
                if (idx(1).ne.0) then
                    !print*,j,visited(idx(1))%pos
                    
                    k=findloc(queue,0)
                    queue(k(1))=idx(1)
                end if
                if (.not.any(path.eq.idx(1))) then
                    path(o)=idx(1)
                    o=o+1
                end if
            end do
            !print*,"prev",idx,visited(idx(1))%pos
            current=visited(queue(1))
            tqueue=0
            tqueue(1:siz-1)=queue(2:)
            queue=tqueue
            !print*,findloc(queue,0)
            !print*,current%pos,current%score
            !read(*,*)
            !ans2=ans2+1
            !print*,"start?",current%pos(:2),s(:2)
        end do
        !ans2=count(path.ne.0)
        o=1
        do i=1,size(path)
            if (path(i).ne.0) then
                !print*,visited(path(i))%pos
                p=pair(visited(path(i))%pos(1),visited(path(i))%pos(2))
                !print*,p
                grid(visited(path(i))%pos(1),visited(path(i))%pos(2))="O"
                if (.not.any(check.eq.p)) then
                    check(o)=p
                    o=o+1
                end if
            end if
        end do
        ans2=count(check.ne.0)+1
    end subroutine part2
end program day16

! 97420 too high
! 95416
! 89420 too high

! 85420 correct

! 420 + 24 + 1 = 445
! 420 + 47 + 1 = 468
! 420 + 61 + 1 = 482 low
! 420 + 64 + 1 = 485
! 420 + 67 + 1 = 488 low
! 420 + 71 + 1 = 492


! 420 + 72 + 1 = 493 high
! 420 + 77 + 1 = 498 high