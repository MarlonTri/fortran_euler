module euler14_mod
use euler_help
implicit none
contains

function collatz (m)
	implicit none
	
	integer :: collatz,m
	integer(kind = 8) :: n
	n = int(m,8)
	collatz = 1
	do while (n>1)
		if (2*(n/2) == n) then
			n = n/2
		else
			n = 3*n+1
		end if
		collatz = collatz + 1
	end do

end function collatz


function euler14 ()
	implicit none
	
	integer :: euler14,n,m,i
	euler14 = 1
	m = 1
	n=2
	i=2
	
	!do while (i<20)
	!!print*,i,"  ",collatz(i),NEW_LINE('A')
	!i = i + 1
	!end do
	
	do while (n<1000000)
		if (collatz(n)>m) then
			euler14 = n
			m = collatz(n)
		end if
		n = n + 1
	end do
	
	
	

end function euler14

end module euler14_mod