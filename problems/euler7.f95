module euler7_mod
use euler_help
implicit none
contains


function euler7 ()
	implicit none
	
	integer :: euler7,pamt=2,prime,i
	integer, dimension(10000) :: primes
	logical :: brk 
	primes(1) = 2
	primes(2) = 3
	do while (pamt < 10001)
		prime = primes(pamt)
		brk = .false.
		do while ( .not. brk)
			prime = prime + 2
			do i = 1 , pamt
				if (mod(prime,primes(i))==0) then
					exit
				end if
				if (i==pamt) then
					brk = .true.
				end if
			end do
		end do
		pamt = pamt + 1
		primes(pamt) = prime
	end do
	euler7 = prime

end function euler7

end module euler7_mod