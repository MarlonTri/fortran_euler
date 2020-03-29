module euler6_mod
use euler_help
implicit none
contains


function euler6 ()
	implicit none
	
	integer :: euler6,su,sq,i
	su=0
	sq=0
	do i = 1,100
		sq = sq + i
		su = su + i**2
	end do
	sq = sq **2
	euler6 = sq - su

end function euler6

end module euler6_mod