program hoof_it
    use reader
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    type :: node
        integer :: id
        integer :: height
        type(node_pointer),allocatable :: n(:) ! neighbours
        integer,allocatable :: n_dist(:)
        integer :: dist=-1
        type(node_pointer),allocatable :: previous
    end type node
    type :: node_pointer
        type(node),pointer :: ptr
    end type node_pointer
    call cpu_time(ts)
    open(1,file="2024_10")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        integer :: data(file_len,line_len)
        type(node) :: nodes(file_len*line_len)
        integer :: ans1, ans2
        ans1 = 0
        ans2 = 0
        data=int_grid_reader(file_len,line_len)
        call build_nodes(nodes,file_len,line_len,data)
        call part1(file_len,line_len,nodes,ans1)
        call part2(file_len,line_len,nodes,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function pair(x,y) result (ans)
        integer :: x,y,ans
        ans = (0.5*(x+y)*(x+y+1))+y
    end function pair

    subroutine build_nodes(nodes,file_len,line_len,grid)
        integer :: file_len,line_len,i,j,n_num,idx,k
        integer :: grid(file_len,line_len)
        type(node),target :: nodes(file_len*line_len),n
        k=1
        do i=1,file_len
            do j=1,line_len
                n_num=0
                n%id=pair(i,j)
                n%height=grid(i,j)
                if (i.lt.file_len.and.grid(i+1,j)-grid(i,j).eq.1) then
                    n_num=n_num+1
                end if
                if (j.lt.line_len.and.grid(i,j+1)-grid(i,j).eq.1) then
                    n_num=n_num+1
                end if
                if (i.gt.1.and.grid(i-1,j)-grid(i,j).eq.1) then
                    !print*,3
                    n_num=n_num+1
                end if
                if (j.gt.1.and.grid(i,j-1)-grid(i,j).eq.1) then
                    !print*,4
                    n_num=n_num+1
                end if
                allocate(n%n(n_num))
                allocate(n%n_dist(n_num))
                nodes(k) = n
                deallocate(n%n)
                deallocate(n%n_dist)
                k=k+1
            end do
        end do
        k=1
        do i=1,file_len
            do j=1,line_len
                idx=1
                if (i.lt.file_len.and.grid(i+1,j)-grid(i,j).eq.1) then
                    nodes(k)%n(idx)%ptr=>nodes(k+line_len)
                    idx=idx+1
                end if
                if (j.lt.line_len.and.grid(i,j+1)-grid(i,j).eq.1) then
                    nodes(k)%n(idx)%ptr=>nodes(k+1)
                    idx=idx+1
                end if
                if (i.gt.1.and.grid(i-1,j)-grid(i,j).eq.1) then
                    nodes(k)%n(idx)%ptr=>nodes(k-line_len)
                    idx=idx+1
                end if
                if (j.gt.1.and.grid(i,j-1)-grid(i,j).eq.1) then
                    nodes(k)%n(idx)%ptr=>nodes(k-1)
                    idx=idx+1
                end if
                k=k+1
            end do
        end do
    end subroutine build_nodes

    subroutine search1(start,count)
        integer :: count,i,new,j,k,test
        type(node) :: start,current
        type(node_pointer),allocatable :: unvisited(:),temp(:)
        logical :: go,in
        allocate(unvisited(size(start%n)))
        unvisited=start%n
        go=.true.
        current=start
        count=0
        i=1
        do while(go)
            current=unvisited(1)%ptr
            allocate(temp(size(unvisited)-1))
            temp = unvisited(2:)
            deallocate(unvisited)
            new=0
            do j=1,size(current%n)
                in=.false.
                do k=1,size(temp)
                    if (temp(k)%ptr%id.eq.current%n(j)%ptr%id) then
                        in=.true.
                    end if
                end do
                if (.not.in) then
                    new=new+1
                end if
            end do
            allocate(unvisited(size(temp)+new))
            unvisited(:size(temp))=temp
            test=size(temp)+1
            do j=1,size(current%n)
                in=.false.
                do k=1,size(temp)
                    if (temp(k)%ptr%id.eq.current%n(j)%ptr%id) then
                        in=.true.
                    end if
                end do
                if (.not.in) then
                    unvisited(test)=current%n(j)
                    test=test+1
                end if
            end do
            deallocate(temp)
            if (current%height.eq.9) then
                count=count+1
            end if
            i=i+1
            if (size(unvisited).eq.0) then
                go=.false.
            end if
        end do
        deallocate(unvisited)
    end subroutine search1

    subroutine search2(start,count)
        integer :: count,new
        type(node) :: start,current
        type(node_pointer),allocatable :: unvisited(:),temp(:)
        logical :: go
        allocate(unvisited(size(start%n)))
        unvisited=start%n
        go=.true.
        current=start
        count=0
        do while(go)
            current=unvisited(1)%ptr
            allocate(temp(size(unvisited)-1))
            temp = unvisited(2:)
            deallocate(unvisited)
            new=0
            allocate(unvisited(size(temp)+size(current%n)))
            unvisited(:size(temp))=temp
            unvisited(size(temp)+1:)=current%n
            deallocate(temp)
            if (current%height.eq.9) then
                count=count+1
            end if
            if (size(unvisited).eq.0) then
                go=.false.
            end if
        end do
        deallocate(unvisited)
    end subroutine search2

    subroutine part1(file_len,line_len,nodes,ans1)
        integer :: file_len,line_len,ans1,k,count
        type(node) :: nodes(file_len*line_len)
        do k=1,file_len*line_len
            if (nodes(k)%height.eq.0) then
                call search1(nodes(k),count)
                ans1=ans1+count
            end if
        end do
    end subroutine part1

    subroutine part2(file_len,line_len,nodes,ans2)
        integer :: file_len,line_len,ans2,k,count
        type(node) :: nodes(file_len*line_len)
        do k=1,file_len*line_len
            if (nodes(k)%height.eq.0) then
                call search2(nodes(k),count)
                ans2=ans2+count
            end if
        end do
    end subroutine part2
end program hoof_it