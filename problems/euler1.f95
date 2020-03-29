module euler1_mod
implicit none
contains


function euler1 ()
	implicit none
	
	integer :: euler1,n
	euler1 = 0
	do n = 0,999
		if (mod(n,3)==0 .or. mod(n,5)==0) then
			euler1 = euler1 + n
		end if
	end do
end function euler1

end module euler1_mod