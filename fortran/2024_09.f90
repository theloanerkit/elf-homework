program day09
    use reader
    use string
    use types
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    type :: disk
        integer :: id
        integer :: length
        logical :: empty
    end type disk
    type(disk) :: nulldisk
    nulldisk%id=-1
    nulldisk%length=-1
    nulldisk%empty=.true.
    call cpu_time(ts)
    open(1,file="2024_09")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character(line_len) :: data(file_len)
        integer(kind=k12) :: ans1, ans2
        type(disk),allocatable :: diskmap(:),diskmap2(:)
        allocate(diskmap(line_len))
        allocate(diskmap2(line_len))
        ans1 = 0
        ans2 = 0
        data=string_reader(file_len,line_len)
        !print*,line_len
        call build_disk(data(1),file_len,line_len,diskmap)
        diskmap2=diskmap
        call part1(diskmap,line_len,ans1)
        call part2(diskmap2,line_len,ans2)
        print*,"fuck"
        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
        deallocate(diskmap)
        deallocate(diskmap2)
    end subroutine run

    subroutine build_disk(line,file_len,line_len,diskmap)
        integer :: file_len,line_len
        character :: line(line_len)
        type(disk) :: diskmap(line_len),tempdisk
        integer :: id,i
        logical :: empty
        diskmap=nulldisk
        !tempdisk%id=0
        !tempdisk%length=1
        !tempdisk%empty=.false.
        !diskmap(1)=tempdisk
        empty=.false.
        id=0
        do i=1,line_len
            !print*,i,"/",line_len
            tempdisk%id=id
            tempdisk%length=str_to_int(line(i),1)
            tempdisk%empty=empty
            !print*,"set up new fileblock"
            !print*,size(diskmap),i
            diskmap(i)=tempdisk
            !print*,"next file block added"
            if (empty) then
               ! print*,"empty space length",diskmap(i)%length
                id=id +1
            else
                !print*,"file id",diskmap(i)%id,"length",diskmap(i)%length
            end if
            empty = .not.empty
        end do
        !print*,"disk built"
    end subroutine build_disk

    subroutine compress_disk(diskmap,compact_disk,line_len,num)
        integer :: line_len,i,cd_idx,dm_idx,num
        type(disk) :: diskmap(line_len),compact_disk(line_len)
        logical :: go
        compact_disk=nulldisk
        go=.true.
        cd_idx=1
        dm_idx=line_len
        i=1
        num=0
        do while(go)
            !sprint*,i,cd_idx,dm_idx
            if (.not.diskmap(i)%empty.and.diskmap(i)%id.ne.-1) then
                !print*,"already a file here"
                compact_disk(cd_idx)=diskmap(i)
                diskmap(i)%id=-1
                cd_idx=cd_idx+1
            else
                if (diskmap(dm_idx)%empty) then
                    dm_idx=dm_idx-1
                end if
                if (diskmap(i)%length.eq.diskmap(dm_idx)%length.and.diskmap(dm_idx)%id.ne.-1) then
                    !print*,"correct space, moving",dm_idx,"to",cd_idx
                    compact_disk(cd_idx) = diskmap(dm_idx)
                    diskmap(dm_idx)%id=-1
                    dm_idx=dm_idx-2
                    cd_idx=cd_idx+1
                else if (diskmap(i)%length.gt.diskmap(dm_idx)%length.and.diskmap(dm_idx)%id.ne.-1) then
                    !print*,"too much space, moving",dm_idx,"to",cd_idx
                    compact_disk(cd_idx) = diskmap(dm_idx)
                    !compact_disk(cd_idx)%length = diskmap(i)%length
                    !print*,"new len",diskmap(i)%length,diskmap(dm_idx)%length
                    diskmap(i)%length=diskmap(i)%length-diskmap(dm_idx)%length
                    diskmap(dm_idx)%id=-1
                    dm_idx=dm_idx-2
                    cd_idx=cd_idx+1
                    
                    
                    i=i-1
                else if (diskmap(i)%length.lt.diskmap(dm_idx)%length.and.diskmap(dm_idx)%id.ne.-1) then
                    !print*,"not enough space, moving",diskmap(i)%length,"of",dm_idx,"to",cd_idx
                    compact_disk(cd_idx) = diskmap(dm_idx)
                    compact_disk(cd_idx)%length = diskmap(i)%length
                    diskmap(dm_idx)%length = diskmap(dm_idx)%length-diskmap(i)%length
                    !diskmap(dm_idx)%id=-1
                    !dm_idx=dm_idx-2
                    cd_idx=cd_idx+1
                end if
            end if
            i=i+1
            if (i.gt.line_len.or.dm_idx.le.0.or.cd_idx.gt.line_len) then
                go=.false.
            end if
            !print*,cd_idx,file_len
        end do
    end subroutine compress_disk


    subroutine compress_disk2(diskmap,compact_disk,line_len,num)
        integer :: line_len,i,cd_idx,dm_idx,num,j,space,k
        type(disk) :: diskmap(line_len),compact_disk(line_len),temp
        logical :: moved
        compact_disk=nulldisk
        moved=.false.
        cd_idx=1
        dm_idx=line_len
        i=1
        num=0
        !do k=1,line_len
        !    print*,diskmap(k)%id,diskmap(k)%length,diskmap(k)%empty
        !end do
        do i=line_len,1,-1
            !print*,i
            cd_idx=1
            
            moved=.false.
            !print*,"checking",diskmap(i)%id,diskmap(i)%empty
            !print*,"checking"
            if (.not.diskmap(i)%empty) then
                !print*,"moving"
                compact_disk=nulldisk
                !print*,"checking",diskmap(i)%id
                do j=1,line_len
                    !print*,diskmap(j)%id,diskmap(j)%empty
                    if (.not.diskmap(j)%empty.or.moved.or.j.gt.i) then
                        !print*,"moved already, or file is here",j
                        compact_disk(cd_idx) = diskmap(j)
                        cd_idx=cd_idx+1
                    else if (diskmap(j)%length.eq.diskmap(i)%length) then
                        !print*,"space, moving",diskmap(i)%id
                        compact_disk(cd_idx) = diskmap(i)
                        diskmap(i)%empty=.true.
                        cd_idx=cd_idx+1
                        moved=.true.
                    else if (diskmap(j)%length.gt.diskmap(i)%length) then
                        !print*,"space big, moving",diskmap(i)%id
                        compact_disk(cd_idx) = diskmap(i)
                        diskmap(i)%empty=.true.
                        diskmap(j)%length=diskmap(j)%length-diskmap(i)%length
                        compact_disk(cd_idx+1) = diskmap(j)
                        cd_idx=cd_idx+2
                        moved=.true.
                    else
                        compact_disk(cd_idx) = diskmap(j)
                        cd_idx=cd_idx+1
                    end if
                end do
                !print*,"moved"
                !do k=1,line_len
                !    print*,diskmap(k)%id,diskmap(k)%length,diskmap(k)%empty
                !end do
                !print*," "
            end if
            !print*,"checked"
            diskmap=compact_disk
            !print*,"reset"
        end do
        !print*,"hello?"
        !do while(go)
        !    !sprint*,i,cd_idx,dm_idx
        !    if (.not.diskmap(i)%empty.and.diskmap(i)%id.ne.-1) then
        !        print*,"already a file here",diskmap(i)%id,diskmap(i)%length
        !        compact_disk(cd_idx)=diskmap(i)
        !        !diskmap(i)%id=-1
        !        cd_idx=cd_idx+1
        !    else
        !        !p!rint*,"empty space",diskmap(i)%length
        !        do while (diskmap(dm_idx)%empty)
        !            dm_idx=dm_idx-1
        !        end do
        !        !print*,dm_idx,diskmap(dm_idx)%id
        !        space=diskmap(i)%length
        !        !do j=dm_idx,i,-2
        !            if (diskmap(i)%length.eq.diskmap(dm_idx)%length.and.diskmap(dm_idx)%id.ne.-1) then
        !                !print*,"   exact space"
        !                !print*,"   moving",diskmap(j)%id
        !                print*,"correct space, moving",diskmap(dm_idx)%id,"to",cd_idx
        !                compact_disk(cd_idx) = diskmap(dm_idx)
        !                diskmap(dm_idx)%id=-1
        !                dm_idx=dm_idx-1
        !                cd_idx=cd_idx+1
        !                space=0
        !                !exit
        !            else if (diskmap(i)%length.gt.diskmap(dm_idx)%length.and.diskmap(dm_idx)%id.ne.-1) then
        !                !print*,"   too much space"
        !                !print*,"   moving",diskmap(j)%id
        !                print*,"too much space, moving",diskmap(dm_idx)%id!,"to",cd_idx
        !                compact_disk(cd_idx) = diskmap(dm_idx)
        !                !compact_disk(cd_idx)%length = diskmap(i)%length
        !                !print*,"new len",diskmap(i)%length,diskmap(dm_idx)%length
        !                diskmap(i)%length=diskmap(i)%length-diskmap(dm_idx)%length
        !                diskmap(dm_idx)%id=-1
        !                dm_idx=dm_idx-1
        !                cd_idx=cd_idx+1
        !                compact_disk(cd_idx) = diskmap(i)
        !                cd_idx=cd_idx+1
        !                !space = space-diskmap(j)%length
        !            else
        !                print*,"not enough space to move",diskmap(dm_idx)%id
        !                dm_idx=dm_idx-1
        !                i=i-1
        !            end if   
        !    end if
        !    i=i+1
        !    !print*,i,dm_idx,cd_idx
        !    !print*,line_len,0,line_len
        !    if (i.gt.line_len.or.dm_idx.le.0.or.cd_idx.gt.line_len) then
        !        go=.false.
        !        print*,"stopping"
        !    end if
        !    !print*,cd_idx,file_len
        !end do
    end subroutine compress_disk2

    subroutine checksum(compact_disk,line_len,ans)
        integer :: line_len,i,j,pos
        integer(kind=k12) :: ans
        type(disk) :: compact_disk(line_len)
        pos=0
        !print*,"hello again"
        do i=1,line_len
            !print*,i,"/",line_len
            if (.not.compact_disk(i)%empty) then
                do j=1,compact_disk(i)%length
                    ans=ans+(pos*compact_disk(i)%id)
                    !print*,i,j
                    !print*,pos,"*",compact_disk(i)%id
                    pos=pos+1
                end do
                !print*,ans
                !pos=pos+1
            else
                pos=pos+compact_disk(i)%length
            end if
            !print*,"added"
        end do
    end subroutine checksum

    subroutine part1(diskmap,line_len,ans1)
        integer :: line_len,i,c_len
        integer(kind=k12) :: ans1
        type(disk) :: diskmap(line_len),compact_disk(line_len)

        call compress_disk(diskmap,compact_disk,line_len,c_len)
        !do i=1,line_len
        !    print*,compact_disk(i)%id,compact_disk(i)%length,compact_disk(i)%empty
        !end do
        call checksum(compact_disk,line_len,ans1)
    end subroutine part1

    subroutine part2(diskmap,line_len,ans2)
        integer :: line_len,i,c_len
        integer(kind=k12) :: ans2
        type(disk) :: diskmap(line_len),compact_disk(line_len)
        !do i=1,line_len
        !    print*,diskmap(i)%id,diskmap(i)%length,diskmap(i)%empty
        !end do
        !print*," "
        call compress_disk2(diskmap,compact_disk,line_len,c_len)
        !print*," "
        !do i=1,line_len
        !    print*,compact_disk(i)%id,compact_disk(i)%length,compact_disk(i)%empty
        !end do
        !print*,compact_disk(line_len)%empty,compact_disk(line_len-1)%empty,compact_disk(line_len-2)%empty
        call checksum(compact_disk,line_len,ans2)
        print*,"hello??",ans2
    end subroutine part2
end program day09
! too high 7779928253519
! too high 7776551190669