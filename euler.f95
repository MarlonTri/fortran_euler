! cd C:/Users/Trifunovic/Documents/Marlon/g95/myDocs/
! g95 euler.f95 -o out.o
! out.o

program euler
	implicit none
	
	Print *, "Euler 1:", euler1()
	Print *, "Euler 2:", euler2()
	Print *, "Euler 3:", euler3()
	Print *, "Euler 4:", euler4()
	Print *, "Euler 5:", euler5()
	Print *, "Euler 6:", euler6()
	Print *, "Euler 7:", euler7()
	Print *, "Euler 8:", euler8()
	Print *, "Euler 9:", euler9()
	Print *, "Euler 10:", euler10()
	Print *, "Euler 34:", euler34()
	Print *, "Euler 40:", euler40()
	!Print *, "Euler 10:", euler10()
	!Print *, "Euler 11:", euler11()
	!Print *, "Euler 12:", euler12()
	!pause
	
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


function euler2 ()
	implicit none
	
	integer :: euler2,a,b,c
	euler2 = 0
	a=1
	b=1
	do while (a+b <= 4000000)
		c = a+b
		b = a
		a = c
		if (mod(a,2)==0) then
			euler2 = euler2 + a
		end if
	end do
	
end function euler2


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


function euler4 ()
	implicit none
	
	integer :: euler4,i,j
	euler4 = 0
	do i=100,999
		do j=100,999
			if (is_palin(i*j) .and. euler4<i*j) then
				euler4 = i*j
				!print *, i, j , euler4
			end if
		end do
	end do
	
end function euler4


function is_palin(n)
	implicit none
	
	integer :: n,i,j
	logical :: is_palin
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


function euler5 ()
	implicit none
	
	integer :: euler5
	euler5 = 420
	do while (.not. divis_20(euler5))
		euler5 = euler5 + 420
	end do
	
end function euler5


function divis_20 (n)
	implicit none
	
	integer :: n,i
	logical :: divis_20
	do i = 20,2,-1
		if (mod(n,i)/=0) then
			divis_20 = .false.
			return
		end if		
	end do
	divis_20 = .true.

end function divis_20


function euler6 ()
	implicit none
	
	integer :: euler6,su,sq,i
	su=0
	sq=0
	do i = 1,100
		sq = sq + i
		su = su + i**2
	end do
	sq = sq **2
	euler6 = sq - su
	
end function euler6


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


function is_prime(n)
	implicit none
	
	integer :: n,i
	logical :: is_prime
	do i = 2, n-1
		if (mod(n,i)==0) then
			is_prime = .false.
			return 
		end if
	end do
	is_prime = .true.
	
end function is_prime


function euler8 ()
	implicit none
	
	character(1000) :: bigNum 
	integer(kind = 8) euler8,i,j,prod
	integer(kind = 8) :: adjAMT = 13
	integer(kind = 8), dimension(13) :: adj
	open (2,file = 'euler8.txt', status = 'old')
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


function euler9 ()
	implicit none
	
	integer euler9,a,b,c
	do c = 333 , 998
		do b = 1, 1000-c-1
			if (b**2+(1000-c-b)**2==c**2) then
				euler9 = c*b*(1000-b-c)
				return
			end if
		end do
	end do
	
end function euler9


function euler10 ()
	implicit none
	
	integer :: euler10,pamt=2,prime,i
	integer, dimension(500000) :: primes
	logical :: brk 
	prime = 3
	primes(1) = 2
	primes(2) = 3
	euler10 = 2
	do while (pamt < 500001 .and. prime<2000000)
		prime = primes(pamt)
		euler10 = euler10 + prime
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
	
end function euler10


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

function euler40 ()
	implicit none
	
	integer :: euler40,n
	euler40 = 1
	do n = 0,6
		euler40 = euler40 * champ(10**n)
	end do
	
end function euler40


function champ (n)
	implicit none
	
	integer :: champ,n,start,dig,i
	i = 1
	do while (9*i*10**(i-1)<n)
		n = n - 9*i*10**(i-1)
		i = i + 1
	end do
	n = n - 1
	start = 10**(i-1)+(n)/i
	dig = i-mod(n,i)-1
	
	
	
	champ = mod(start/(10**dig),10)
end function champ

end program euler












