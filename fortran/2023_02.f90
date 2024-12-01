program day02
    use reader
    use string
    use list
    implicit none
    type :: game
            integer :: idx
            integer,allocatable :: counts(:)
            character,allocatable :: colours(:)
        end type
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2023_02")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character(line_len) :: data(file_len)
        integer :: ans1, ans2
        type(game) :: games(file_len)
        ans1 = 0
        ans2 = 0
        data = string_reader(file_len,line_len)
        call build_games(file_len,line_len,data,games)
        call part1(games,file_len,ans1)
        call part2(games,file_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine build_games(file_len,line_len,strings,games)
        integer :: file_len,line_len
        character(line_len) :: strings(file_len),short
        type(game) :: games(file_len)
        type(game) :: newgame
        character :: separators(1)=(/"~"/)
        character(line_len),allocatable :: temp(:)
        integer :: i,items,split,j
        do i=1,file_len
            items=count_chars(line_len,strings(i),separators,1)+1
            split=index(strings(i),":")
            allocate(newgame%counts(items))
            allocate(newgame%colours(items))
            newgame%idx=str_to_int(strings(i)(6:split),split-6)
            newgame%counts=ints_from_str(items,strings(i)(split+1:line_len),line_len-split-1)
            short=""
            short=strings(i)(split+2:line_len)
            allocate(temp(items*2))
            temp=to_list(short,line_len,(/"`"/),1,items*2)
            do j=1,items*2
                if(mod(j,2).eq.0) then
                    newgame%colours(j/2)=temp(j)(1:1)
                end if
            end do
            games(i) = newgame
            deallocate(newgame%counts)
            deallocate(newgame%colours)
            deallocate(temp)
        end do
    end subroutine build_games

    subroutine part1(games,file_len,ans1)
        integer :: ans1,file_len,i,j,bad
        type(game) :: games(file_len)
        bad=0
        do i=1,file_len
            do j=1,size(games(i)%counts)
                if(games(i)%colours(j).eq."r".and.games(i)%counts(j).gt.12)then
                    bad = bad+1
                else if (games(i)%colours(j).eq."g".and.games(i)%counts(j).gt.13) then
                    bad=bad+1
                else if (games(i)%colours(j).eq."b".and.games(i)%counts(j).gt.14) then
                    bad=bad+1
                end if
            end do
            if(bad.eq.0)then
                ans1 = ans1+games(i)%idx
            end if
            bad=0
        end do

    end subroutine part1

    subroutine part2(games,file_len,ans2)
        integer :: ans2,file_len,i,j,red,green,blue
        type(game) :: games(file_len)
        do i=1,file_len
            red = 0
            green = 0
            blue = 0
            do j=1,size(games(i)%counts)
                if(games(i)%colours(j).eq."r".and.games(i)%counts(j).gt.red) then
                    red = games(i)%counts(j)
                else if (games(i)%colours(j).eq."g".and.games(i)%counts(j).gt.green) then
                    green = games(i)%counts(j)
                else if (games(i)%colours(j).eq."b".and.games(i)%counts(j).gt.blue) then
                    blue = games(i)%counts(j)
                end if
            end do
            ans2 = ans2 + (red*green*blue)
        end do
    end subroutine part2
end program day02