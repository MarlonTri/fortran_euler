module euler34_mod
use euler_help
implicit none
contains

function sumFact (n)
	implicit none
	
	integer :: sumFact,n,mult,dig
	sumFact = 0
	do while (n/=0)
		dig = mod(n,10)
		n = n/10
		mult = 1
		do while (dig > 1)
			mult = mult * dig
			dig = dig - 1
		end do
		sumFact = sumFact + mult
	end do
	
end function sumFact

function euler34 ()
	implicit none
	
	integer :: euler34,n,hold
	euler34 = 0
	do hold = 3, 3000000
		n = hold
		if (hold == sumFact(n)) then

			euler34 = euler34 + hold
		end if
	end do

end function euler34

end module euler34_mod