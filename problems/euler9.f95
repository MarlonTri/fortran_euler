module euler9_mod
use euler_help
implicit none
contains


function euler9 ()
	implicit none
	
	integer euler9,a,b,c
	do c = 333 , 998
		do b = 1, 1000-c-1
			if (b**2+(1000-c-b)**2==c**2) then
				euler9 = c*b*(1000-b-c)
				return
			end if
		end do
	end do

end function euler9

end module euler9_mod