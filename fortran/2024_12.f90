program garden_groups
    use reader
    use list
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    type :: region
        integer,allocatable :: coords(:,:)
        integer :: area
        integer :: perimeter
    end type
    call cpu_time(ts)
    open(1,file="2024_12")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character :: data(file_len,line_len)
        integer :: ans1, ans2
        type(region),allocatable :: plots(:)
        ans1 = 0
        ans2 = 0
        data=grid_reader(file_len,line_len)
        call build_plots(file_len,line_len,data,plots)
        call part1(plots,size(plots),ans1)
        call part2(plots,size(plots),ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine new_coord(coords,i,j)
        integer,allocatable :: coords(:,:),coords_t(:,:)
        integer :: i,j
        !print*,"adding",i,j
        allocate(coords_t(size(coords,1)+1,2))
        !print*,"size",size(coords,1),size(coords,1)+1
        coords_t(:size(coords,1),:)=coords
        !print*,"temporary",coords_t
        coords_t(size(coords,1)+1,:)=(/i,j/)
        !print*,"temporary",coords_t
        deallocate(coords)
        allocate(coords(size(coords_t,1),2))
        coords=coords_t
        deallocate(coords_t)
        !print*,"new",coords
    end subroutine new_coord

    recursive subroutine flood_fill(file_len,line_len,i,j,grid,char,coords)
        integer :: file_len,line_len,i,j
        integer,allocatable :: coords(:,:)
        character :: grid(file_len,line_len),char
        !print*,"flood fill ",char,i,j
        !print*,coords
        if (i.lt.file_len.and.find_2d(coords,size(coords,1),(/i+1,j/)).eq.0.and.grid(i+1,j).eq.char) then
            call new_coord(coords,i+1,j)
            grid(i+1,j)="."
            call flood_fill(file_len,line_len,i+1,j,grid,char,coords)
        end if
        if (i.gt.1.and.find_2d(coords,size(coords,1),(/i-1,j/)).eq.0.and.grid(i-1,j).eq.char) then
            call new_coord(coords,i-1,j)
            grid(i-1,j)="."
            call flood_fill(file_len,line_len,i-1,j,grid,char,coords)
        end if
        if (j.lt.line_len.and.find_2d(coords,size(coords,1),(/i,j+1/)).eq.0.and.grid(i,j+1).eq.char) then
            call new_coord(coords,i,j+1)
            grid(i,j+1)="."
            call flood_fill(file_len,line_len,i,j+1,grid,char,coords)
        end if
        if (j.gt.1.and.find_2d(coords,size(coords,1),(/i,j-1/)).eq.0.and.grid(i,j-1).eq.char) then
            call new_coord(coords,i,j-1)
            grid(i,j-1)="."        
            call flood_fill(file_len,line_len,i,j-1,grid,char,coords)
        end if
    end subroutine flood_fill

    subroutine build_plots(file_len,line_len,grid,plots)
        integer :: file_len,line_len,i,j
        integer,allocatable :: coords(:,:)
        character :: grid(file_len,line_len),char
        type(region),allocatable :: plots(:),temp(:)!,plot
        type(region) :: plot
        allocate(plots(0))
        do i=1,file_len
            do j=1,line_len
                !print*,i,j
                char=grid(i,j)
                if (char.ne.".") then
                    allocate(coords(0,2))
                    !print*,"found a plot ",char
                    call new_coord(coords,i,j)
                    grid(i,j)="."
                    !print*,"searching for plots labelled ",char
                    call flood_fill(file_len,line_len,i,j,grid,char,coords)
                    !print*,"exited flood fill"
                    allocate(plot%coords(size(coords,1),2))
                    plot%coords=coords
                    allocate(temp(size(plots)+1))
                    temp(:size(plots))=plots
                    temp(size(plots)+1)=plot
                    deallocate(plots)
                    allocate(plots(size(temp)))
                    plots=temp
                    deallocate(temp)
                    deallocate(plot%coords)
                    deallocate(coords)
                end if
            end do
        end do
        !do i=1,size(plots)
        !    print*,"a plot with coordinates"
        !    do j=1,size(plots(i)%coords,1)
        !        print*,plots(i)%coords(j,:)
        !    end do
        !    print*,""
        !end do
    end subroutine

    function perimeter(coords,count) result (p)
        integer :: p,count,i
        integer :: coords(count,2)
        p=4*count
        do i=1,count
            if (find_2d(coords,count,(/coords(i,1)+1,coords(i,2)/)).ne.0) then
                p=p-1
            end if
            if (find_2d(coords,count,(/coords(i,1)-1,coords(i,2)/)).ne.0) then
                p=p-1
            end if
            if (find_2d(coords,count,(/coords(i,1),coords(i,2)+1/)).ne.0) then
                p=p-1
            end if
            if (find_2d(coords,count,(/coords(i,1),coords(i,2)-1/)).ne.0) then
                p=p-1
            end if
        end do
    end function perimeter

    subroutine part1(plots,count,ans1)
        integer :: count,ans1,i
        type(region) :: plots(count)
        do i=1,count
            plots(i)%area = size(plots(i)%coords,1)
            plots(i)%perimeter = perimeter(plots(i)%coords,size(plots(i)%coords,1))
            ans1=ans1+(plots(I)%area*plots(i)%perimeter)
        end do
    end subroutine part1

    function sides(coords,num) result (s)
        integer :: num, s,i
        integer :: coords(num,2)
        integer :: n(8) ! tl t tr r br b bl l
        s=0
        do i=1,num
            n=0
            if (find_2d(coords,num,(/coords(i,1)+1,coords(i,2)/)).ne.0) then ! b
                n(6)=1
            end if
            if (find_2d(coords,num,(/coords(i,1)-1,coords(i,2)/)).ne.0) then ! t
                n(2)=1
            end if
            if (find_2d(coords,num,(/coords(i,1),coords(i,2)+1/)).ne.0) then ! r
                n(4)=1
            end if
            if (find_2d(coords,num,(/coords(i,1),coords(i,2)-1/)).ne.0) then ! l
                n(8)=1
            end if
            if (find_2d(coords,num,(/coords(i,1)+1,coords(i,2)-1/)).ne.0) then ! bl
                n(7)=1
            end if
            if (find_2d(coords,num,(/coords(i,1)+1,coords(i,2)+1/)).ne.0) then ! br
                n(5)=1
            end if
            if (find_2d(coords,num,(/coords(i,1)-1,coords(i,2)-1/)).ne.0) then ! tl
                n(1)=1
            end if
            if (find_2d(coords,num,(/coords(i,1)-1,coords(i,2)+1/)).ne.0) then ! tr
                n(3)=1
            end if

            if (n(8).eq.0.and.n(2).eq.0) then ! convex
                s=s+1
            else if (n(8).eq.1.and.n(2).eq.1.and.n(1).eq.0) then
                s=s+1
            end if
            if (n(2).eq.0.and.n(4).eq.0) then
                s=s+1
            else if (n(2).eq.1.and.n(4).eq.1.and.n(3).eq.0) then
                s=s+1
            end if
            if (n(4).eq.0.and.n(6).eq.0) then
                s=s+1
            else if (n(4).eq.1.and.n(6).eq.1.and.n(5).eq.0) then
                s=s+1
            end if
            if (n(6).eq.0.and.n(8).eq.0) then
                s=s+1
            else if (n(6).eq.1.and.n(8).eq.1.and.n(7).eq.0) then
                s=s+1
            end if
        end do
    end function sides

    subroutine part2(plots,count,ans2)
        integer :: count, ans2,i,temp
        type(region) :: plots(count)
        do i=1,count
            temp = sides(plots(i)%coords,size(plots(i)%coords,1))
            ans2=ans2+(plots(i)%area*temp)
        end do
    end subroutine part2
end program garden_groups