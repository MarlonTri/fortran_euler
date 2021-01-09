module euler23_mod
use euler_help
implicit none
contains


logical function abundant (n)
	implicit none
	
	integer :: ab,n,i
	ab = 1
	i = 2
	do while (i<n)
		if (mod(n,i)==0) then
			ab = ab + i
		end if
		i = i + 1
	end do
	abundant = (ab > n)

end function abundant


function euler23 ()
	implicit none
	
	integer :: euler23,i,j,k,ab_ind,magic
	integer, dimension(10000) :: abunds
	logical :: summ
	
	magic = 28123
	i = 12
	ab_ind = 1
	do while (i<magic)
		if (abundant(i)) then
			abunds(ab_ind) = i
			ab_ind = ab_ind + 1
		end if
		i = i + 1
	end do
	
	euler23 = 0
	do i=1,magic
		j = 1
		k = ab_ind - 1
		summ = .true.
		do while (0 < k .and. j < ab_ind)
			if (abunds(j) + abunds(k) < i) then
				j = j + 1
			else if (abunds(j) + abunds(k) > i) then
				k = k - 1
			else if (abunds(j) + abunds(k) == i) then
				summ = .false.
				exit
			end if
		end do
		if (summ) then
			euler23 = euler23 + i
		end if
	end do

end function euler23

end module euler23_mod