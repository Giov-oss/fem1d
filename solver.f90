module solver
contains
  subroutine GAUSS(c,b,x,n)
    implicit none
    integer i,j,n
    real,dimension(n,n)::c
    real,dimension(n)::b
    real,dimension(n)::x
    real,dimension(n,n+1)::a
  a(:,1:n)=c
  a(:,n+1)=b
  do i=1,n
    a(i,:)=a(i,:)/a(i,i)
    do j=1,n
      if (i /= j) then
        a(j,:)=a(j,:)-a(i,:)*a(j,i)
      end if
    end do
  end do
  x(:)=a(:,n+1)
  end subroutine

  subroutine stationary_check(temp_np1, temp_n, nnodos, stationary_step, error0)
    implicit none
    integer :: nnodos
    logicaL :: stationary_step
    real,dimension(nnodos) :: temp_np1, temp_n, dif_temp
    real,parameter :: criterio=10.**(-6.)
    real :: norm,error0
    dif_temp=temp_np1-temp_n
    norm=sqrt(dot_product(dif_temp,dif_temp))
    if (norm<criterio) then
      stationary_step=.true.
      error0=norm
    end if
  end subroutine

end module
