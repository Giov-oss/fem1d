module mesh
  contains
    subroutine grid()
      implicit none
      integer :: nelementos=10
      real :: largo=1.
      integer :: nnodos
      real,allocatable,dimension(:) :: coord
      real :: dx
      integer :: i
      integer :: alloc_stat
  
      nnodos=nelementos+1
      allocate (coord(nnodos), stat = alloc_stat)
      if (alloc_stat /= 0) stop "*** not enough memory ***"
  
        dx=largo/nelementos
        coord=(/(dx*(i-1),i=1,nnodos)/)
  
      open(69,file='data/elmnts.dat',status='unknown',action='write')
      open(70,file='data/nods.dat',status='unknown',action='write')
  
      do i=1,nelementos
        write(69,*)i,i,i+1
      end do
      do i=1,nnodos
        write(70,*)i,coord(i),'0.0'
      end do
  
      close(69)
      close(70)
    end subroutine grid
  end module
  
  
  