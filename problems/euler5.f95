module euler5_mod
implicit none
contains

function divis_20 (n)
	implicit none
	
	integer :: n,i
	logical :: divis_20
	do i = 20,2,-1
		if (mod(n,i)/=0) then
			divis_20 = .false.
			return
		end if		
	end do
	divis_20 = .true.

end function divis_20

function euler5 ()
	implicit none
	
	integer :: euler5
	euler5 = 420
	do while (.not. divis_20(euler5))
		euler5 = euler5 + 420
	end do

end function euler5

end module euler5_mod