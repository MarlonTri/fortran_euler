module euler10_mod
use euler_help
implicit none
contains


integer(kind = 8) function euler10 ()
	implicit none
	
	integer :: pamt=2,prime,i
	integer, dimension(150000) :: primes
	logical :: brk 
	prime = 3
	primes(1) = 2
	primes(2) = 3
	euler10 = 2
	do while (pamt < 150001 .and. prime<2000000)
		prime = primes(pamt)
		euler10 = euler10 + int(prime,8)
		brk = .false.
		do while ( .not. brk)
			prime = prime + 2
			do i = 1 , pamt
				if (mod(prime,primes(i))==0) then
					exit
				end if
				if (primes(i)**2>prime) then
					brk = .true.
					exit
				end if
			end do
		end do
		pamt = pamt + 1
		primes(pamt) = prime
	end do
	
end function euler10

end module euler10_mod