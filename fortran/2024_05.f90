program day05
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
        !print*,rules(1,:)
        !print*,queue(file_len,:)
        call part1(rules,queue,file_len,ans1)
        call part2(rules,queue,file_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    function get_rules(data,file_len,line_len) result (rules)
        integer :: file_len,line_len,i
        character(line_len) :: data(file_len)
        integer :: rules(file_len,2)
        rules=-1
        do i=1,file_len
            if (data(i)(1:1).eq."`") then
                exit
            end if
            rules(i,:)=ints_from_str(2,data(i),line_len)
        end do
    end function get_rules

    function get_queue(data,file_len,line_len) result (queue)
        integer :: file_len,line_len,i,c
        character(line_len) :: data(file_len)
        integer :: queue(file_len,23)
        logical :: go
        go=.false.
        queue=-1
        do i=1,file_len
            if (go) then
                c=count_chars(line_len,data(i),(/"~"/),1)+1
                queue(i,:c)=ints_from_str(c,data(i),line_len)
            end if
            if (data(i)(1:1).eq."`") then
                go=.true.
            end if
        end do
    end function get_queue

    function check_queue_elem(elem,rules,file_len,queue,elem_pos) result (ok)
        logical :: ok
        integer :: elem,file_len,rulecount,k,idx(2),otherpage,check(1),elem_pos
        integer :: found(200,2),rules(file_len,2),queue(23)
        ok=.true.
        found=-1
        rulecount=count(rules.eq.elem)
        !print*,rulecount
        do k=1,rulecount
            idx=findloc(rules,elem)
            rules(idx(1),idx(2))=rules(idx(1),idx(2))*10
            found(k,:)=idx
            otherpage=rules(idx(1),mod(idx(2),2)+1)
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
        do k=1,rulecount
            rules(found(k,1),found(k,2))=rules(found(k,1),found(k,2))/10
        end do
    end function check_queue_elem

    function check_queue(queue,rules,file_len) result (ans)
        integer :: ans,j,rulecount,k,idx(2),file_len,otherpage,check(1),minus(1),t
        integer :: queue(23),found(200,2),rules(file_len,2)
        logical :: ok
        if (queue(1).ne.-1) then
            ok=.true.
            do j=1,23
                found=-1
                if (queue(j).eq.-1) then
                    exit
                else 
                !    ok = check_queue_elem(queue(j),rules,file_len,queue,j)
                end if
                
                rulecount = count(rules.eq.queue(j))
                !print*,rulecount
                do k=1,rulecount
                    idx=findloc(rules,queue(j))
                    rules(idx(1),idx(2))=rules(idx(1),idx(2))*10
                    found(k,:)=idx
                    otherpage=rules(idx(1),mod(idx(2),2)+1)
                    check=findloc(queue,otherpage)
                    if (check(1).ne.0) then
                        if (check(1).gt.j.and.idx(2).eq.2) then
                            ok=.false.
                            exit
                        else if (check(1).lt.j.and.idx(2).eq.1) then
                            ok=.false.
                            exit
                        end if
                    end if
                end do
                do k=1,rulecount
                    rules(found(k,1),found(k,2))=rules(found(k,1),found(k,2))/10
                end do
                !print*," "
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
    end function check_queue
        
    function fix_queue(queue,rules,file_len) result(ans)
        integer :: ans,j,rulecount,k,idx(2),file_len,otherpage,check(1),minus(1),t1,t2
        integer :: queue(23),found(200,2),rules(file_len,2)
        logical :: ok
        !print*,queue
        if (queue(1).ne.-1) then
            ok=.true.
            do j=1,23
                found=-1
                if (queue(j).eq.-1) then
                    exit
                else 
                !    ok = check_queue_elem(queue(j),rules,file_len,queue,j)
                end if
                
                rulecount = count(rules.eq.queue(j))
                !print*,rulecount
                do k=1,rulecount
                    idx=findloc(rules,queue(j))
                    rules(idx(1),idx(2))=rules(idx(1),idx(2))*10
                    found(k,:)=idx
                    otherpage=rules(idx(1),mod(idx(2),2)+1)
                    check=findloc(queue,otherpage)
                    if (check(1).ne.0) then
                        if (check(1).gt.j.and.idx(2).eq.2) then
                            ok=.false.
                            t1 = queue(check(1))
                            t2 = queue(j)
                            queue(j) = t1
                            queue(check(1)) = t2
                            ok=.true.
                            !exit
                        else if (check(1).lt.j.and.idx(2).eq.1) then
                            ok=.false.
                            t1 = queue(check(1))
                            t2 = queue(j)
                            queue(j) = t1
                            queue(check(1)) = t2
                            ok=.true.
                            !exit
                        end if
                    end if
                end do
                do k=1,rulecount
                    rules(found(k,1),found(k,2))=rules(found(k,1),found(k,2))/10
                end do
                !print*," "
            end do
        else
            ans = 0
        end if
        !print*,queue
        !print*," "
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
        !print*,file_len
        do i=1,file_len
            bad=.false.
            !print*,queue(i,:)
            temp = check_queue(queue(i,:),rules,file_len)
            !
            print*,temp,"/",queue(i,:)

            !temp=fix_queue(queue(i,:),rules,file_len)

            !print*,temp,"/",queue(i,:)
            !print*,""
            !ans1=ans1+temp
            if (temp.eq.0.and.queue(i,1).ne.-1) then
                bad=.true.
                !print*,queue(i,:)
                !temp=fix_queue(queue(i,:),rules,file_len)
                !print*,queue(i,:)
                !temp=check_queue(queue(i,:),rules,file_len)
                !print*,temp
                
                !print*," "
                
            end if
            do while (temp.eq.0.and.queue(i,1).ne.-1)
                print*,queue(i,:)
                temp=fix_queue(queue(i,:),rules,file_len)
                print*,queue(i,:)
                temp=check_queue(queue(i,:),rules,file_len)
                print*,temp
                
                print*," "
                
            end do
            if (bad) then
                ans2=ans2+temp
            end if
        end do
    end subroutine part2
end program day05

!5568 too low
!6186 too high