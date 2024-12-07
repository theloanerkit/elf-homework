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
        !print*,size(answers)!,answers
    end subroutine new_answer

    recursive function operators(eqn,l,answers) result (new_eqn)
        integer(kind=k12) :: l,eqn(l),new_eqn(l),temp(l-1),i
        integer(kind=k12),allocatable :: answers(:)
        if (l.eq.1) then
            new_eqn(1) = eqn(1)
            call new_answer(answers,eqn(1))
        else
            temp(1) = eqn(1)+eqn(2)
            do i=2,l-1
                temp(i)=eqn(i+1)
            end do
            new_eqn=operators(temp,l-1,answers)

            temp(1) = eqn(1)*eqn(2)
            do i=2,l-1
                temp(i)=eqn(i+1)
            end do
            new_eqn=operators(temp,l-1,answers)
        end if
    end function operators

    recursive function operators2(eqn,l,answers) result (new_eqn)
        integer(kind=k12) :: l,eqn(l),new_eqn(l),temp(l-1),i,f
        integer(kind=k12),allocatable :: answers(:)
        real(kind=dp) :: r,pow
        if (l.eq.1) then
            new_eqn(1) = eqn(1)
            call new_answer(answers,eqn(1))
        else
            temp(1) = eqn(1)+eqn(2)
            do i=2,l-1
                temp(i)=eqn(i+1)
            end do
            !print*,temp
            new_eqn=operators2(temp,l-1,answers)

            temp(1) = eqn(1)*eqn(2)
            do i=2,l-1
                temp(i)=eqn(i+1)
            end do
            !print*,temp
            new_eqn=operators2(temp,l-1,answers)

            r=eqn(2)
            pow=log10(r)
            f=floor(pow)+1
            temp(1) = eqn(1)*10**f+eqn(2)
            do i=2,l-1
                temp(i)=eqn(i+1)
            end do
            !print*,temp
            new_eqn=operators2(temp,l-1,answers)
            !print*,"---"
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
            ans = operators(eqn_num(2:),nums16-1,answers)  
        else
            ans = operators2(eqn_num(2:),nums16-1,answers)  
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
        integer(kind=k12) :: ans2,t(1)
        integer :: file_len,line_len,i,c
        character(line_len) :: equations(file_len)
        logical :: test
        do i=1,file_len
            print*,i,"/",file_len
            c=count_chars(line_len,equations(i),(/"`"/),1)+1
            test=eqn_valid(line_len,equations(i),c,2)
            if (test) then
                t = ints_from_str(1,equations(i),line_len)
                ans2 = ans2 + t(1)
            end if
        end do
    end subroutine part2
end program day07