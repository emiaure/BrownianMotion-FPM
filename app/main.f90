program main
  use stdlib_stats_distribution_normal, only: norm => rvs_normal
  use stdlib_random, only: random_seed
real :: x, p, z, u, D, dt, tiempo, finish, start, prom, prom2, desv, dx, s1, s2, sig, tray
integer :: t, npasos, k, j, i, put, get, seed, pid, seed1
dimension z(0:1000000), u(0:1000), tray(0:9, 0:100000)
integer, dimension(8) :: values
open(1, file= 'promn.dat', status= 'replace')
open(2, file= 'trayectorias.dat', status='replace')

!posicin inicil
u(0)=1.
!Difusion
D=0.25

!Número de pruebas en cada paso de tiempo
t=10000


!tiempo final
tiempo=2.
!numero de pasos hasta llegar a t
npasos=100
write(*,*) 'Tiempo final'
read(*,*) tiempo
write(*,*) 'Numero de pasos dt'
read(*,*) npasos
write(*,*) 'Coeficiente de difusion'
read(*,*) D
write(*,*) 'Numero de pruebas para cada t'
read(*,*) t
write(*,*) 'Posición inicial'
read(*,*) u(0)

dt=tiempo/npasos
z(0)=0.
sig=sqrt(2*D*dt)
pid=getpid()
call date_and_time(values=values)
seed=pid+values(8)*values(7)*values(6)
seed1=mod(seed, 2147483647)
call random_seed(seed, get)

do k=0, 9
tray(k,0)=u(0)
do j=1, npasos
dx=norm(0.,sig)
tray(k,j)=tray(k,j-1)+dx
end do
end do 
do j=0, npasos 
write(2,*) dt*j, tray(0,j), tray(1,j), tray(2,j), tray(3,j), tray(4,j), tray(5,j), tray(6,j), tray(7,j), tray(8,j), tray(9,j)
end do 

do k=1, npasos
	do j=1, t
		do i=1, k
		dx=norm(0., sig)
		u(i)= u(i-1)+dx
		end do 
		z(j)=u(k) 
	end do

	s1=0.
	s2=0.

	do j=1, t
		s1=s1+z(j)
		s2=s2+(z(j))**2
	end do 
	prom=s1/t
	prom2=s2/t
	desv=prom2-(prom)**2
	write(1,*) k*dt, prom, desv
end do 
end program main
