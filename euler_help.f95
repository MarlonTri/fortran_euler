module euler_help
implicit none
contains

logical function is_palin(n)
	implicit none
	
	integer :: n,i,j
	integer, dimension(20) :: digs
	i = 0
	j = 0
	do while (n/=0)
		i = i + 1
		digs(i) = mod(n,10)
		n = (n - digs(i))/10
	end do
	do j= 1, i/2+1
		if (digs(j)/=digs(i-j+1)) then
			is_palin = .false.
			return
		end if
	end do
	is_palin = .true.
	
end function is_palin

logical function is_prime(n)
	implicit none
	
	integer :: n,i
	do i = 2, n-1
		if (mod(n,i)==0) then
			is_prime = .false.
			return 
		end if
	end do
	is_prime = .true.
	
end function is_prime

end module  euler_help