module reader
    use string
    implicit none

    contains

    function int_reader(file_len) result (nums)
        integer :: file_len, i
        integer :: nums(file_len)
        do i=1,file_len
            read(1,*) nums(i)
        end do
    end function int_reader

    function int_reader_columns(file_len,line_len,columns) result (nums)
        integer :: file_len, line_len, columns, i
        integer :: nums(file_len,columns)
        character(line_len) :: line
        nums=-1
        do i=1,file_len
            read(1,*) line
            nums(i,:) = ints_from_str(columns,line,line_len)
        end do
    end function int_reader_columns

    function string_reader(file_len, line_len) result (strings)
        integer :: file_len, line_len, i
        character(line_len) :: strings(file_len)
        do i=1,file_len
            read(1,*) strings(i)
        end do
    end function string_reader

    function grid_reader(file_len, line_len) result (grid)
        integer :: file_len, line_len, i, j
        character :: grid(file_len,line_len)
        character(line_len) :: string
        do i=1,file_len
            read(1,*) string
            do j=1,line_len
                grid(i,j) = string(j:j)
            end do
        end do
    end function grid_reader
end module reader