module euler40_mod
use euler_help
implicit none
contains

function euler40 ()
	implicit none
	
	integer :: euler40,n
	euler40 = 1
	do n = 0,6
		euler40 = euler40 * champ(10**n)
	end do
	
end function euler40


function champ (n)
	implicit none
	
	integer :: champ,n,start,dig,i
	i = 1
	do while (9*i*10**(i-1)<n)
		n = n - 9*i*10**(i-1)
		i = i + 1
	end do
	n = n - 1
	start = 10**(i-1)+(n)/i
	dig = i-mod(n,i)-1	
	champ = mod(start/(10**dig),10)
end function champ


end module euler40_mod