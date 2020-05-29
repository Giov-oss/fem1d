!------------------------------------------------------------------------------
!-----------------------------------------------------------ecuación de energía
module ecenergia
  implicit none
  !----------------------------------------------------------------------------
  !----------------------------------------------------------------------master
  integer,parameter :: time=1
  integer,parameter :: convec=1
  integer,parameter :: difus=1
  integer,parameter :: supg=1
  !----------------------------------------------------------------------------
  !--------------------------------------------------------------------temporal
  real,parameter :: t0=0.
  real,parameter :: tf=100.
  real,parameter :: dt=0.1
  real :: theta=0.5
  !----------------------------------------------------------------------------
  !-----------------------------------------------------------------propiedades
  real,parameter :: rho=1.
  real,parameter :: c=1.
  real,parameter :: k=1.
  real,parameter :: u=100.
  real,parameter :: f=0.
  !----------------------------------------------------------------------------
  !-----------------------------------------------------------------condiciones
  type bc
    integer :: nodo
    character(3) :: bctype
    real :: pv,sv,beta,gamma
  end type
  type(bc) :: cb1=bc(0,'dir',0,0,0,0)
  type(bc) :: cb2=bc(0,'dir',1,-1,-0.5,10)

end module ecenergia


