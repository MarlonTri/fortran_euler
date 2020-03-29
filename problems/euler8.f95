module euler8_mod
use euler_help
implicit none
contains


function euler8 ()
	implicit none
	
	character(1000) :: bigNum 
	integer(kind = 8) euler8,i,j,prod
	integer(kind = 8) :: adjAMT = 13
	integer(kind = 8), dimension(13) :: adj
	open (2,file = '../resources/euler8.txt', status = 'old')
	read(2,'(a)') bigNum
	close(2)
	euler8 = 0
	do i = 1, 1000-adjAMT+1
		
		do j = 0,adjAMT-1
			read(bigNum(i+j:i+j), *) adj(1+j)
		end do
		
		prod = 1
		do j = 1, adjAMT
			prod = prod * adj(j)
		end do
		if (euler8 < prod) then
			euler8 = prod
		end if
	end do

end function euler8

end module euler8_mod