program day11
    use reader
    use string
    use integer
    use types
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    type :: stone 
        integer(kind=k12) :: number
        integer :: step
        integer(kind=k12) :: count
    end type 
    call cpu_time(ts)
    open(1,file="2024_11")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character(line_len) :: data(file_len)
        integer :: num_stones,i,idx(1)
        integer(kind=k12) :: ans1,ans2
        integer(kind=k12),allocatable :: stone_nums(:),added(:),temp_num(:)
        type(stone),allocatable :: stones(:),temp(:)
        ans1 = 0
        ans2 = 0
        data=string_reader(file_len,line_len)
        num_stones = count_chars(line_len,data(1),(/"`"/),1)+1
        allocate(stone_nums(num_stones))
        stone_nums = ints_from_str(num_stones,data(1),line_len)
        do i=1,num_stones
            if (.not.allocated(added)) then
                allocate(added(1))
                added(1)=stone_nums(i)
                allocate(stones(1))
                stones(1)%number=stone_nums(i)
                stones(1)%step=0
                stones(1)%count=1
            else if (any(added.eq.stone_nums(i))) then
                idx = findloc(added,stone_nums(i))
                stones(idx)%count = stones(idx)%count + 1
            else
                allocate(temp_num(size(added)+1))
                temp_num(:size(added)) = added
                temp_num(size(added)+1) = stone_nums(i)
                deallocate(added)
                allocate(added(size(temp_num)))
                added = temp_num
                deallocate(temp_num)
                allocate(temp(size(stones)+1))
                temp(:size(stones))=stones
                temp(size(stones)+1)%number = stone_nums(i)
                temp(size(stones)+1)%step = 0
                temp(size(stones)+1)%count = 1
                deallocate(stones)
                allocate(stones(size(temp)))
                stones=temp
                deallocate(temp)
            end if
        end do
        call part1(stones,25,ans1)
        call part1(stones,75,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function finished_blinking(stones,count,blinks) result (finish)
        integer :: count,blinks,i
        type(stone),allocatable :: stones(:)
        logical :: finish
        finish=.true.
        do i=1,count
            if (stones(i)%step.lt.blinks) then
                finish=.false.
                exit
            end if
        end do
    end function finished_blinking

    subroutine add_new_stone(stones,new_stone)
        type(stone),allocatable :: stones(:),temp(:)
        type(stone) :: new_stone
        integer :: i
        logical :: add
        add=.true.
        do i=1,size(stones)
            if (stones(i)%number.eq.new_stone%number.and.stones(i)%step.eq.new_stone%step) then
                add=.false.
                stones(i)%count = stones(i)%count+new_stone%count
            end if
        end do
        if (add) then
            allocate(temp(size(stones)+1))
            temp(:size(stones))=stones
            temp(size(stones)+1)=new_stone
            deallocate(stones)
            allocate(stones(size(temp)))
            stones=temp
            deallocate(temp)
        end if
    end subroutine

    subroutine blink(stones,count,blinks)
        integer :: count,blinks,new_count,i,k
        type(stone),allocatable :: stones(:),new_stones(:),temp(:)
        type(stone) :: current,new1,new2
        character(:),allocatable :: templ,tempr,temps
        allocate(new_stones(0))
        do i=1,count
            current = stones(i)
            k=floor(log10(real(current%number)))
            if (current%number.eq.0) then
                new1%number=1
                new1%step=current%step+1
                new1%count=current%count
                call add_new_stone(new_stones,new1)
            else if (mod(k,2).ne.0) then
                allocate(character(k+1)::temps)
                allocate(character((k+1)/2)::templ)
                allocate(character((k+1)/2)::tempr)

                temps=to_str(current%number)
                templ=temps(:(k+1)/2)
                tempr=temps(((k+1)/2+1):)

                new1%number=str_to_int(templ,(k+1)/2)
                new2%number=str_to_int(tempr,(k+1)/2)
                new1%count=current%count
                new2%count=current%count
                new1%step=current%step+1
                new2%step=current%step+1
                call add_new_stone(new_stones,new1)
                call add_new_stone(new_stones,new2)

                deallocate(temps)
                deallocate(templ)
                deallocate(tempr)
            else    
                new1%number=current%number*2024
                new1%step=current%step+1
                new1%count=current%count
                call add_new_stone(new_stones,new1)
            end if
        end do
        deallocate(stones)
        allocate(stones(size(new_stones)))
        stones=new_stones
    end subroutine blink

!
    subroutine part1(stones,blinks,ans1)
        integer(kind=k12) :: ans1
        integer :: blinks,n
        type(stone),allocatable :: stones(:)
        logical :: finish
        finish = .false.
        do while(.not.finish)
            call blink(stones,size(stones),blinks)
            finish=finished_blinking(stones,size(stones),blinks)
        end do

        do n=1,size(stones)
            ans1=ans1+stones(n)%count
        end do
    end subroutine part1

end program day11

! 1933900191 too low