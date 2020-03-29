module euler4_mod
use euler_help
implicit none
contains


function euler4 ()
	implicit none
	
	integer :: euler4,i,j	
	euler4 = 0
	do i=100,999
		do j=100,999
			if (is_palin(i*j) .and. euler4<i*j) then
				euler4 = i*j
				!print *, i, j , euler4
			end if
		end do
	end do
	
end function euler4

end module euler4_mod