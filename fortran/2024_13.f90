program claw_contraption
    use reader
    use string
    use types
    implicit none
    integer :: file_len, line_len
    double precision :: ts,tf
    real :: tol
    type :: claw
        integer(kind=k12) :: a(2),b(2),pos(2)
    end type
    call cpu_time(ts)
    open(1,file="2024_13")
    read(1,*) file_len
    read(1,*) line_len
    tol=1E-3

    call run(file_len, line_len)
    call cpu_time(tf)
    print*,"time taken: ",tf-ts,"s"
    contains

    subroutine run(file_len,line_len)
        integer :: file_len, line_len,i
        character(line_len) :: data(file_len)
        integer :: ans1
        integer(kind=k12) :: ans2
        type(claw),allocatable :: claws(:)
        !print*,file_len
        allocate(claws((file_len+1)/4))
        ans1 = 0
        ans2 = 0
        data=string_reader(file_len,line_len)
        do i=1,file_len,4
            claws((i/4)+1)%a=ints_from_str(2,data(i),line_len)
            claws((i/4)+1)%b=ints_from_str(2,data(i+1),line_len)
            claws((i/4)+1)%pos=ints_from_str(2,data(i+2),line_len)
        end do
        call part1(claws,size(claws),ans1)
        call part2(claws,size(claws),ans2)

        print*,"part 1 answer: ",ans1
        print*,"part 2 answer: ",ans2
    end subroutine run

    subroutine part1(claws,siz,ans1)
        integer :: siz,ans1,i,x,y
        real(kind=dp) :: temp,num,den,xr,diff
        type(claw) :: claws(siz),cl
        do i=1,siz
            cl=claws(i)
            temp = real(cl%b(1),dp)/real(cl%b(2),dp)
            num = real(cl%pos(1),dp)-(real(cl%pos(2),dp)*temp)
            den = real(cl%a(1),dp)-(real(cl%a(2),dp)*temp)
            xr=num/den
            diff = int(xr)-xr
            x=int(xr)
            if (abs(diff).gt.0.5) then
                diff = ceiling(xr)-xr
                x=ceiling(xr)
            end if
            if (abs(diff).le.tol) then
                y=(cl%pos(1)-(x*cl%a(1)))/cl%b(1)
                if (x.ge.0.and.y.ge.0.and.x.le.100.and.y.le.100) then
                    ans1=ans1+(3*x)+y
                end if
            end if
        end do
    end subroutine part1
    subroutine part2(claws,siz,ans2)
        integer :: siz
        integer(kind=k12) :: ans2,i,x,y,c1,c2
        real(kind=dp) :: temp,num,den,xr,diff
        type(claw) :: claws(siz),cl
        do i=1,siz
            cl=claws(i)
            cl%pos(1)=cl%pos(1)+10000000000000_k12
            cl%pos(2)=cl%pos(2)+10000000000000_k12
            temp = real(cl%b(1),dp)/real(cl%b(2),dp)
            num = real(cl%pos(1),dp)-(real(cl%pos(2),dp)*temp)
            den = real(cl%a(1),dp)-(real(cl%a(2),dp)*temp)
            xr=num/den
            diff = int(xr,k12)-xr
            x=int(xr,k12)
            if (abs(diff).gt.0.5) then
                diff = ceiling(xr,k12)-xr
                x=ceiling(xr,k12)
            end if
            if (abs(diff).le.tol) then
                y=(cl%pos(1)-(x*cl%a(1)))/cl%b(1)
                if (x.ge.0.and.y.ge.0) then
                    c1=cl%pos(1)-(x*cl%a(1))-(y*cl%b(1))
                    c2=cl%pos(2)-(x*cl%a(2))-(y*cl%b(2))
                    if (c1.eq.0.and.c2.eq.0) then
                        ans2=ans2+(3*x)+y
                    end if
                end if
            end if
        end do
    end subroutine part2
end program claw_contraption