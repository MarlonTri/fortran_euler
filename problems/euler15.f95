module euler15_mod
use euler_help
implicit none
contains

function lattice (n)
	implicit none
	
	integer(kind = 8) :: lattice,n,i,j
	integer(kind = 8), dimension(n) :: arr
	i = 0
	do while (i < n)
		arr(i) = i+1
		i = i + 1
	end do
	i = 2*n
	lattice = 1
	do while (i>n)
		lattice = lattice * i
		i = i - 1
		j = 0
		do while (j<n)
			if (arr(j)>1 .AND. (lattice/arr(j))*arr(j)==lattice) then
				lattice = lattice / arr(j)
				arr(j) = 1
			end if
			j = j + 1
		end do
	end do

end function lattice

function euler15 ()
	implicit none	
	
	integer(kind = 8) :: euler15
	euler15 = lattice(int(20,8))

end function euler15

end module euler15_mod