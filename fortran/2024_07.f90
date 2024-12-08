program day07
    use reader
    use string
    use types
    implicit none
    integer,parameter :: k=selected_int_kind(12)
    integer :: file_len, line_len
    double precision :: ts,tf
    call cpu_time(ts)
    open(1,file="2024_07")
    read(1,*) file_len
    read(1,*) line_len

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len
        character(line_len) :: data(file_len)
        integer(kind=k12) :: ans1,ans2
        ans1 = 0
        ans2 = 0
        data=string_reader(file_len,line_len)
        call part1(data,file_len,line_len,ans1)
        print*,"-------"
        call part2(data,file_len,line_len,ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine new_answer(answers,ans)
        integer(kind=k12),allocatable :: answers(:),temp(:)
        integer(kind=k12) :: ans,s
        if (.not.allocated(answers)) then
            allocate(answers(1))
            answers(1) = ans
        else
            s=size(answers)
            allocate(temp(s))
            temp = answers
            deallocate(answers)
            allocate(answers(s+1))
            answers(s+1) = ans
            answers(:s+1)=temp
            deallocate(temp)
        end if
    end subroutine new_answer

    subroutine build_next_eqn(temp,eqn,l,op)
        integer :: op,i
        integer(kind=k12) :: l
        integer(kind=k12) :: temp(l-1),eqn(l)
        if (op.eq.1) then
            temp(1) = eqn(1)+eqn(2)
        else if (op.eq.2) then
            temp(1) = eqn(1)*eqn(2)
        else if (op.eq.3) then
            temp(1) = eqn(1)*10**(floor(log10(real(eqn(2))))+1)+eqn(2)
        end if
        do i=2,l-1
            temp(i)=eqn(i+1)
        end do
    end subroutine build_next_eqn

    recursive function operators(eqn,l,answers,goal) result (new_eqn)
        integer(kind=k12) :: l,eqn(l),new_eqn(l),temp(l-1),broke(1)
        integer(kind=k12),allocatable :: answers(:)
        integer(kind=k12) :: goal
        if (eqn(1).gt.goal.and.l.ne.1) then
            broke(1)=eqn(1)
            new_eqn=operators(broke,1_k12,answers,goal)
        else if (l.eq.1) then
            new_eqn(1) = eqn(1)
            call new_answer(answers,eqn(1))
        else
            if (allocated(answers)) then
                if (.not.any(answers.eq.goal)) then
                    call build_next_eqn(temp,eqn,l,1)
                    new_eqn=operators(temp,l-1,answers,goal)
                
                    call build_next_eqn(temp,eqn,l,2)
                    new_eqn=operators(temp,l-1,answers,goal)
                end if
            end if
            if (.not.allocated(answers)) then
                call build_next_eqn(temp,eqn,l,1)
                new_eqn=operators(temp,l-1,answers,goal)
            
                call build_next_eqn(temp,eqn,l,2)                
                new_eqn=operators(temp,l-1,answers,goal)
            end if
        end if
    end function operators

    recursive function operators2(eqn,l,answers,goal) result (new_eqn)
        integer(kind=k12) :: l,eqn(l),new_eqn(l),temp(l-1),broke(1),goal
        integer(kind=k12),allocatable :: answers(:)
        !print*,"equation",eqn
        if (eqn(1).gt.goal.and.l.ne.1) then
            !print*,"oops too far",eqn(1),goal
            broke(1)=eqn(1)
            new_eqn=operators(broke,1_k12,answers,goal)
        else if (l.eq.1) then
            !print*,"new answer",eqn(1)
            new_eqn(1) = eqn(1)
            call new_answer(answers,eqn(1))
        else
            if (allocated(answers)) then
                if (.not.any(answers.eq.goal)) then
                    !print*,"no correct answers yet"
                    call build_next_eqn(temp,eqn,l,1)
                    !print*,"added",temp
                    new_eqn=operators2(temp,l-1,answers,goal)

                    call build_next_eqn(temp,eqn,l,2)
                    !print*,"multiplied",temp
                    new_eqn=operators2(temp,l-1,answers,goal)

                    call build_next_eqn(temp,eqn,l,3)
                    !print*,"concatenated",temp
                    new_eqn=operators2(temp,l-1,answers,goal)
                end if
            end if
            if (.not.allocated(answers)) then
                !print*,"no answers yet"
                call build_next_eqn(temp,eqn,l,1)
                !print*,"added",temp
                new_eqn=operators2(temp,l-1,answers,goal)

                call build_next_eqn(temp,eqn,l,2)
                !print*,"multiplied",temp
                new_eqn=operators2(temp,l-1,answers,goal)

                call build_next_eqn(temp,eqn,l,3)
                !print*,"concatenated",temp
                new_eqn=operators2(temp,l-1,answers,goal)
            end if
        end if
    end function operators2

    function eqn_valid(line_len,eqn,nums,part) result (valid)
        integer :: line_len,nums,part
        character(line_len) :: eqn
        integer(kind=k12) :: eqn_num(nums),ans(1),nums16
        integer(kind=k12),allocatable :: answers(:)
        logical :: valid
        valid=.false.
        eqn_num=ints_from_str(nums,eqn,line_len)
        nums16=nums
        if (part.eq.1) then
            ans = operators(eqn_num(2:),nums16-1,answers,eqn_num(1))  
        else
            ans = operators2(eqn_num(2:),nums16-1,answers,eqn_num(1))  
        end if
        !print*,answers
        if (any(answers.eq.eqn_num(1))) then
            !print*,eqn_num(1)
            valid=.true.
        end if
        deallocate(answers)
    end function eqn_valid

    subroutine part1(equations,file_len,line_len,ans1)
        integer(kind=k12) :: ans1,t(1)
        integer :: file_len,line_len,i,c
        character(line_len) :: equations(file_len)
        logical :: test
        do i=1,file_len
            c=count_chars(line_len,equations(i),(/"`"/),1)+1
            test=eqn_valid(line_len,equations(i),c,1)
            if (test) then
                t = ints_from_str(1,equations(i),line_len)
                ans1 = ans1 + t(1)
            end if
        end do
    end subroutine part1

    subroutine part2(equations,file_len,line_len,ans2)
        integer(kind=k12) :: ans2,t(1),g
        integer :: file_len,line_len,i,c
        character(line_len) :: equations(file_len)
        logical :: test
        g=1
        do i=1,file_len
            print*,i,"/",file_len
            c=count_chars(line_len,equations(i),(/"`"/),1)+1
            !print*,equations(i)
            test=eqn_valid(line_len,equations(i),c,2)
            if (test) then
                t = ints_from_str(1,equations(i),line_len)
                ans2 = ans2 + t(1)
            end if
        end do
    end subroutine part2
end program day07