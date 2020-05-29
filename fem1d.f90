!------------------------------------------------------------------------------
!------------------------------------------------------------------main program
program fem_galerkin
  use ecenergia
  use mesh
  use data
  use elemental
  use solver
  use plot
  implicit none
  real,allocatable,dimension(:,:)::nodos,elementos,longitud !discretización
  real,allocatable,dimension(:,:)::mglobal,dmglobal,kglobal,dkglobal,rhs !matriz del sistema
  integer :: alloc_stat
  integer :: nnodos,nelementos,j,i,h,ts=(tf-t0)/dt,ncb=0
  real::melem(2,2),kelem(2,2),fuente(2,1) !matriz elemental
  integer,dimension(2)::eg !nodos del elemento i
  real,allocatable,dimension(:,:)::temp,ml,mr,mlr,mrr !matriz reducida
  real,allocatable,dimension(:)::rhsr,var
  integer,allocatable,dimension(:)::reductor !operador reductor
  integer :: total_steps
  logical :: stationary_step = .false.
  real :: error0
!------------------------------------------------------------------------------
!--------------------------------------------------------------lectura de datos
  call grid()
  call getdata('data/nods.dat',nodos)
  call getdata('data/elmnts.dat',elementos)

  nnodos=size(nodos)/3
  nelementos=size(elementos)/3

  allocate (longitud(nelementos, 2), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  call long(nodos,elementos,nnodos,nelementos,longitud)

  allocate (mglobal(nnodos, nnodos), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  mglobal=0.

  allocate (dmglobal(nnodos, nnodos), stat = alloc_stat)
  if (alloc_stat /= 0) stop "** * not enough memory ***"
  dmglobal=0.

  allocate (kglobal(nnodos, nnodos), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  kglobal=0.

  allocate (dkglobal(nnodos, nnodos), stat = alloc_stat)
  if (alloc_stat /= 0) stop "** * not enough memory ***"
  dkglobal=0.

  allocate (rhs(nnodos, 1), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  rhs=0.
!------------------------------------------------------------------------------
!--------------------------------------------------------------matriz elemental
  do i=1,nelementos
    call matrizelemental(melem,kelem,fuente,i,nelementos,longitud)
!------------------------------------------------------------------------------
!-----------------------------------------------------------------matriz global
    call numglobal(elementos,nelementos,i,eg)
    do j=1,2
      do h=1,2
        dmglobal(eg(j),eg(h))=melem(j,h)
        dkglobal(eg(j),eg(h))=kelem(j,h)
        mglobal=mglobal+dmglobal
        kglobal=kglobal+dkglobal
        dmglobal=0.
        dkglobal=0.
      end do
      rhs(eg(j),1)=rhs(eg(j),1)+fuente(j,1)
    end do
  end do

  !call imprimir_matriz(mglobal,nnodos,nnodos)
  !call imprimir_matriz(kglobal,nnodos,nnodos)
!-----------------------------------------------------------------------------
!------------------------------------------------cond. de contorno e iniciales
  if (time==0) then
    ts=1
    theta=1
  end if

  allocate (temp(nnodos,ts+1), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  allocate (ml(nnodos,nnodos), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  allocate (mr(nnodos,nnodos), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  allocate (mlr(nnodos-2,nnodos-2), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  allocate (mrr(nnodos-2,nnodos-2), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  allocate (rhsr(nnodos-2), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"

  cb1%nodo=int(nodos(1,1))
  cb2%nodo=int(nodos(nnodos,1))

  temp(nnodos,1)=0.
  if (cb1%bctype=='dir') then
    temp(cb1%nodo,1)=cb1%pv
    ncb=ncb+1
  else if (cb1%bctype=='new') then
    rhs(cb1%nodo,1)=rhs(cb1%nodo,1)+cb1%sv
  else if (cb1%bctype=='rob') then
    rhs(cb1%nodo,1)=rhs(cb1%nodo,1)+cb1%gamma
    kglobal(cb1%nodo,cb1%nodo)=kglobal(cb1%nodo,cb1%nodo)-cb1%beta
  end if
  if (cb2%bctype=='dir') then
    temp(cb2%nodo,1)=cb2%pv
    ncb=ncb+1
  else if (cb2%bctype=='new') then
    rhs(cb2%nodo,1)=rhs(cb2%nodo,1)+cb2%sv
  else if (cb2%bctype=='rob') then
    rhs(cb2%nodo,1)=rhs(cb2%nodo,1)+cb2%gamma
    kglobal(cb2%nodo,cb2%nodo)=kglobal(cb2%nodo,cb2%nodo)-cb2%beta
  end if


  allocate (reductor(nnodos-ncb), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"
  j=1
  do i=1,nnodos
    if (i==cb1%nodo .and. (cb1%bctype=='new' .or. cb1%bctype=='rob')) then
      reductor(j)=i
      j=j+1
    else if (i==cb2%nodo .and. (cb2%bctype=='new' .or. cb2%bctype=='rob')) then
      reductor(j)=i
      j=j+1
    else if (i/=cb1%nodo .and. i/=cb2%nodo) then
      reductor(j)=i
      j=j+1
    end if
  end do

!-----------------------------------------------------------------------------
!--------------------------------------------------------------sistema de ecs.
  ml=mglobal/dt+theta*kglobal
  mr=-mglobal/dt+(1-theta)*kglobal
  mlr=ml(reductor,reductor)
  mrr=mr(reductor,reductor)

!call imprimir_matriz(ml,nnodos,nnodos)
!call imprimir_matriz(mr,nnodos,nnodos)

  allocate (var(nnodos-ncb), stat = alloc_stat)
  if (alloc_stat /= 0) stop "*** not enough memory ***"

  total_steps=1
  do i=2,ts+1
    rhsr=rhs(reductor,1)-&
    (ml(reductor,cb1%nodo)*cb1%pv+ml(reductor,cb2%nodo)*cb2%pv)-&
    (mr(reductor,cb1%nodo)*cb1%pv+mr(reductor,cb2%nodo)*cb2%pv)-&
    matmul(mrr,temp(reductor,i-1))
    call gauss(mlr,rhsr,var,nnodos-ncb)
    temp(cb1%nodo,i)=cb1%pv
    temp(cb2%nodo,i)=cb2%pv
    temp(reductor,i)=var
    call stationary_check(temp(:,i),temp(:,i-1),nnodos,stationary_step,error0)
    if (stationary_step .eqv. .true.) then
      total_steps=total_steps+1
      exit
    end if
    total_steps=total_steps+1
  end do
!-----------------------------------------------------------------------------
!--------------------------------------------------------------------resultado
  open(20,file='data/res.dat',status='unknown',action='write')
    do i=1,nnodos
      write(20,*)i,nodos(i,2),temp(int(nodos(i,1)),:)
      write(*,'(101f9.4)')temp(int(nodos(i,1)),total_steps)
    end do
  close(20)
  print*, total_steps
  if (total_steps<1000) then
    call gnuplot(total_steps)
  end if
!-----------------------------------------------------------------------------
!-------------------------------------------------------------fin del programa
 deallocate (nodos)
 deallocate (elementos)
 deallocate (longitud)
 deallocate (mglobal)
 deallocate (dmglobal)
 deallocate (kglobal)
 deallocate (dkglobal)
 deallocate (ml)
 deallocate (mlr)
 deallocate (mr)
 deallocate (mrr)
 deallocate (rhs)
 deallocate (rhsr)
 deallocate (var)
 deallocate (temp)
 deallocate (reductor)
!------------------------------------------------------------------------------
end program
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!------------------------------------------------------------------------------
subroutine imprimir_matriz(matriz,fil,col)
implicit none
integer i,fil,col
real matriz(fil,col)
write(*,*) "----------------------------------------------------"
do i=1,fil
  write (*,'(101f9.4)') matriz(i,:)
end do
write(*,*) "----------------------------------------------------"
end subroutine