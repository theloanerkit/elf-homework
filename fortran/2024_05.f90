program print_queue
    use reader
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_05")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character(line_len) :: data(file_len)
        integer :: rules(file_len,2),queue(file_len,23)
        integer :: ans1, ans2
        ans1 = 0
        ans2 = 0
        data=string_reader(file_len,line_len)
        rules=get_rules(data,file_len,line_len)
        queue=get_queue(data,file_len,line_len)
        call part1(rules,queue,file_len,ans1)
        call part2(rules,queue,file_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    ! gets the rules from the input data
    function get_rules(data,file_len,line_len) result (rules)
        integer :: file_len,line_len,i
        character(line_len) :: data(file_len)
        integer :: rules(file_len,2)
        rules=-1
        do i=1,file_len
            ! if reached the empty line
            if (data(i)(1:1).eq."`") then
                exit
            end if
            ! get the rule
            rules(i,:)=ints_from_str(2,data(i),line_len)
        end do
    end function get_rules

    ! gets the queues from the input data
    function get_queue(data,file_len,line_len) result (queue)
        integer :: file_len,line_len,i,c
        character(line_len) :: data(file_len)
        integer :: queue(file_len,23)
        logical :: go
        go=.false.
        queue=-1
        do i=1,file_len
            if (go) then
                ! get number of pages in queue
                c=count_chars(line_len,data(i),(/"~"/),1)+1
                ! get the page numbers
                queue(i,:c)=ints_from_str(c,data(i),line_len)
            end if
            ! wait until the empty line
            if (data(i)(1:1).eq."`") then
                go=.true.
            end if
        end do
    end function get_queue

    ! gets rule for specific element
    function get_rule(search,file_len,page,rules) result (stuff)
        integer :: search,file_len,otherpage,stuff(4),page
        integer :: idx1(1),idx2(1),idx(2),rules(file_len,2)
        idx1=findloc(rules(search:,1),page)
        idx2=findloc(rules(search:,2),page)
        if (idx1(1).eq.0) then
            idx(1)=idx2(1)+search-1
            idx(2)=2
        else if (idx2(1).eq.0) then
            idx(1)=idx1(1)+search-1
            idx(2)=1
        else if (idx1(1).lt.idx2(1)) then
            idx(1)=idx1(1)+search-1
            idx(2)=1
        else
            idx(1)=idx2(1)+search-1
            idx(2)=2
        end if
        otherpage=rules(idx(1),mod(idx(2),2)+1)
        stuff(1)=idx(1)+1
        stuff(2)=otherpage
        stuff(3:4)=idx
    end function get_rule

    ! checks current element in the queue
    function check_queue_elem(elem,rules,file_len,queue,elem_pos) result (ok)
        logical :: ok
        integer :: elem,file_len,rulecount,k,idx(2),otherpage,check(1),elem_pos,search
        integer :: found(200,2),rules(file_len,2),queue(23),stuff(4)
        ok=.true.
        found=-1
        search=1
        ! get the number of rules involving the current page number
        rulecount=count(rules.eq.elem)
        do k=1,rulecount
            stuff=get_rule(search,file_len,elem,rules)
            search=stuff(1)
            otherpage=stuff(2)
            idx=stuff(3:4)
            check=findloc(queue,otherpage)
            if (check(1).ne.0) then
                if (check(1).gt.elem_pos.and.idx(2).eq.2) then
                    ok=.false.
                    exit
                else if (check(1).lt.elem_pos.and.idx(2).eq.1) then
                    ok=.false.
                    exit
                end if
            end if
        end do
    end function check_queue_elem

    ! checks the queue
    function check_queue(queue,rules,file_len) result (ans)
        integer :: ans,j,file_len,minus(1),loop(1)
        integer :: queue(23),rules(file_len,2)
        logical :: ok
        ok=.false.
        ans=0
        ! queue is as long as the file, only the later elements have numbers in
        if (queue(1).ne.-1) then
            ok=.true.
            loop=findloc(queue,-1)-1
            if (loop(1).eq.-1) then
                loop(1)=23
            end if
            do j=1,loop(1) ! for each element in queue ("empty" elements set to -1)
                ok = check_queue_elem(queue(j),rules,file_len,queue,j)
                if (.not.ok) then
                    exit
                end if
            end do
        else
            ans = 0
        end if
        ! if the queue passed all the rule checks
        if (ok) then
            minus = findloc(queue,-1) ! get the location of the -1 in the queue
            if (minus(1).ne.1.and.minus(1).ne.0) then ! there is a -1, and it isn't first
                ans = queue(minus(1)/2)
            else if (minus(1).eq.0) then ! there is no minus
                ans = queue(12)
            end if
        end if
    end function check_queue
        
    function fix_queue(queue,rules,file_len) result(ans)
        integer :: ans,j,rulecount,k,idx(2),file_len,otherpage,check(1),minus(1),t1,t2
        integer :: queue(23),found(200,2),rules(file_len,2),stuff(4),search
        logical :: ok
        if (queue(1).ne.-1) then
            ok=.true.
            do j=1,23
                search=1
                found=-1
                if (queue(j).eq.-1) then
                    exit
                end if
                rulecount = count(rules.eq.queue(j))
                do k=1,rulecount
                    stuff=get_rule(search,file_len,queue(j),rules)
                    search=stuff(1)
                    otherpage=stuff(2)
                    idx=stuff(3:4)
                    check=findloc(queue,otherpage)
                    if (check(1).ne.0) then
                        if (check(1).gt.j.and.idx(2).eq.2) then
                            t1 = queue(check(1))
                            t2 = queue(j)
                            queue(j) = t1
                            queue(check(1)) = t2
                            ok=.true.
                        else if (check(1).lt.j.and.idx(2).eq.1) then
                            t1 = queue(check(1))
                            t2 = queue(j)
                            queue(j) = t1
                            queue(check(1)) = t2
                            ok=.true.
                        end if
                    end if
                end do
            end do
        else
            ans = 0
        end if
        if (ok) then
            minus = findloc(queue,-1)
            if (minus(1).ne.1.and.minus(1).ne.0) then
                ans = queue(minus(1)/2)
            else if (minus(1).eq.0) then
                ans = queue(12)
            end if
        end if
    end function fix_queue

    subroutine part1(rules,queue,file_len,ans1)
        integer :: file_len,ans1,i,temp
        integer :: rules(file_len,2),queue(file_len,23)
        do i=1,file_len
            temp = check_queue(queue(i,:),rules,file_len)
            ans1=ans1+temp
        end do
    end subroutine part1

    subroutine part2(rules,queue,file_len,ans2)
        integer :: file_len,ans2,i,temp
        integer :: rules(file_len,2),queue(file_len,23)
        logical :: bad
        bad=.false.
        do i=1,file_len
            bad=.false.
            temp = check_queue(queue(i,:),rules,file_len)
            if (temp.eq.0.and.queue(i,1).ne.-1) then
                bad=.true.
            end if
            do while (temp.eq.0.and.queue(i,1).ne.-1)
                temp=fix_queue(queue(i,:),rules,file_len)
                temp=check_queue(queue(i,:),rules,file_len)
            end do
            if (bad) then
                ans2=ans2+temp
            end if
        end do
    end subroutine part2
end program print_queue