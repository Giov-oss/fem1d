module data
  contains
    subroutine getdata(name,matriz)
      implicit none
      integer,parameter::n=101
      character(len=*):: name
      integer :: i,j,alloc_stat
      real,allocatable,dimension(:,:) :: dummy,matriz
      open(69,file=name,status='old',action='read')
      allocate (dummy(n, 3), stat = alloc_stat)
      if (alloc_stat /= 0) stop "*** not enough memory ***"
      j=0
      do i=1,n
      read(69,*,end=25)dummy(i,1),dummy(i,2),dummy(i,3)
      j=j+1
      end do
      allocate (matriz(j, 3), stat = alloc_stat)
      if (alloc_stat /= 0) stop "*** not enough memory ***"
      25 matriz=dummy(1:j,1:3)
      close(69)
    end subroutine getdata
  
    subroutine long(nodos,elementos,nnodos,nelementos,longitud)
      implicit none
      integer :: i,j,nnodos,nelementos,n1,n2
      real :: x1,x2,y1,y2
      real,dimension(nnodos,3) :: nodos
      real,dimension(nelementos,3) :: elementos
      real,dimension(nelementos,2) :: longitud
      do i=1,nelementos
        n1=int(elementos(i,2))
        n2=int(elementos(i,3))
        longitud(i,1)=elementos(i,1)
        do j=1,nnodos
          if (int(nodos(j,1))==n1) then
            x1=nodos(j,2)
            y1=nodos(j,3)
          else if (int(nodos(j,1))==n2) then
            x2=nodos(j,2)
            y2=nodos(j,3)
          end if
        end do
        longitud(i,2)=sqrt((x2-x1)**2.+(y2-y1)**2.)
      end do
    end subroutine long
  end module
  
  
