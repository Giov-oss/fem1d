module elemental
    contains
    subroutine matrizelemental(melem,kelem,fuente,e,nelementos,longitud)
        use ecenergia
        implicit none
        integer :: e,nelementos,i,j
        real :: longitud(nelementos,2),melem(2,2),kelem(2,2),fuente(2,1)
        real :: m=0.,kc=0.,kd=0.
        real :: he,ei,ej,pe,alpha
        he=longitud(e,2)
        pe=(abs(u)*he)/(2*k)
        if (supg==0) then
          alpha=0.
        else
          alpha=1./tanh(pe)-1./pe
          !alpha=1.
        end if
        do j=1,2
          do i=1,2
            select case (i)
              case (1)
                ei=-1
              case (2)
                ei=1
            end select
            select case (j)
              case (1)
                ej=-1
              case (2)
                ej=1
            end select
            if (time==1) then
              m=(he/12.)*(3+ei*ej)+alpha*(he*ei)/4.
            end if
            if (convec==1) then
              kc=(ej+alpha*ei*ej)*(u/2.)
            end if
            if (difus==1) then
              kd=((k/(rho*c))/he)*ei*ej
            end if
            melem(i,j)=m
            kelem(i,j)=kc+kd
            end do
            fuente(j,1)=(f/(rho*c))*(he/2.)
        end do
    end subroutine matrizelemental

    subroutine numglobal(elementos,nelementos,i,eg)
        implicit none
        integer :: i,n1,n2,nelementos
        real,dimension(nelementos,3) :: elementos
        integer,dimension(2) :: eg
          n1=int(elementos(i,2))
          n2=int(elementos(i,3))
          eg=(/n1,n2/)
    end subroutine numglobal
end module