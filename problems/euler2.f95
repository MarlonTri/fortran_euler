module euler2_mod
implicit none
contains


function euler2 ()
	implicit none
	
	integer :: euler2,a,b,c
	euler2 = 0
	a=1
	b=1
	do while (a+b <= 4000000)
		c = a+b
		b = a
		a = c
		if (mod(a,2)==0) then
			euler2 = euler2 + a
		end if
	end do

end function euler2

end module euler2_mod