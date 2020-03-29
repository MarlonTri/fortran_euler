module euler3_mod
implicit none
contains


function euler3 ()
	implicit none
	
	integer(kind =8) :: euler3,i,n
	euler3 = 1
	n = 600851475143_8
	i=2
	do while (n > 1)
		do while (mod(n,i)/=0)
			i = i + 1
		end do

		euler3 = i
		n = n / i
		i = 2
	end do

end function euler3

end module euler3_mod