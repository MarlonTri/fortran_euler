module euler12_mod
use euler_help
implicit none
contains

function triangle(n)
	implicit none
	integer :: n,triangle
	triangle = (n*n+n)/2
end function triangle

function euler12 ()
	implicit none
	
	integer :: euler12,facts,n,temp,t
	facts = 1
	n = 1
	do while (facts < 500)
		n = n + 1
		euler12 = triangle(n)
		facts = 1
		temp = 1
		do while(temp*temp<euler12)
			temp = temp + 1
			if (mod(euler12,temp)==0) then
				facts = facts + 1
			end if
		end do
		facts = facts * 2
	end do

end function euler12

end module euler12_mod