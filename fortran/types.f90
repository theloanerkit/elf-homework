module types
    implicit none
    integer, parameter :: k8 = selected_int_kind(8)
    integer, parameter :: k10 = selected_int_kind(10)
    integer, parameter :: k12 = selected_int_kind(12)
    integer, parameter :: k16 = selected_int_kind(16)
    integer, parameter :: k32 = selected_int_kind(32)
    integer,parameter :: dp=selected_real_kind(15,300)
end module types