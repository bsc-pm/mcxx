! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
!/*
!* Program written by Oliver Carrillo for the BSC
!* Copyright (c) 2008, BSC (Barcelon Supercomputing Center)
!* All rights reserved.
!*
!* Redistribution and use in source and binary forms, with or without
!* modification, are permitted provided that the following conditions are met:
!*     * Redistributions of source code must retain the above copyright
!*       notice, this list of conditions and the following disclaimer.
!*     * Redistributions in binary form must reproduce the above copyright
!*       notice, this list of conditions and the following disclaimer in the
!*       documentation and/or other materials provided with the distribution.
!*     * Neither the name of the <organization> nor the
!*       names of its contributors may be used to endorse or promote products
!*       derived from this software without specific prior written permission.
!*
!* THIS SOFTWARE IS PROVIDED BY BSC ''AS IS'' AND ANY
!* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
!* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
!* DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
!* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
!* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
!* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
!* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
!* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
!* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!*/

program argon
        implicit none
        character*50 :: auxstr,fitxer
        integer :: N,i,step,niter,iseed1,iseed,nq
        integer :: ni,nk,iatom,mm,j
        real :: fx,fy,fz,x1,y1,z1,vmig,xx,yy,zz,rr,ff
        real :: ffx,ffy,ffz,c,d,l,t,m,dt,tau,kb,pi,mkg
        real :: theta,phi,t1,ti
        double precision :: dran_u
        real lam1, lam, tins
        ! C Newtons, D Amstrongs
        parameter(N=512,l=1.e3,c=1.178e-10,d=3.4e0,m=40.e0,t=300.e0)
        parameter(dt=1.e-12,tau=1.e-11,kb=1.38e-23)
        parameter(pi=3.1415927e0,t1=300.e0,niter=10000,mkg=m*1.67e-27)
        ! La força de Lennard-Jones s de la forma 
        !             F = C * ( 2*(D/r)^13 - (D/r)^7 )
        ! C=24*e/s, D=s 
        ! e=121*kb = 1.67e-21 J, s=3.4 A !!!
        real x(N),y(N),z(N),vx(N),vy(N),vz(N)
        real xh(N),yh(N),zh(N)
        real winv(5000),v(N)
        integer iscr(5000)
        
        integer :: START_TIME, SNAP_TIME
        integer :: ii,jj 
        integer, parameter :: BSIZE = 32, BLOCKS=N/BSIZE
        interface
          !! !$CSS TASK
          subroutine velocity(BSIZE, ii, jj, xi, yi, zi, xj, yj, zj, vx, vy, vz)
             implicit none
             integer, intent(in) :: BSIZE, ii, jj
             real, intent(in), dimension(BSIZE) :: xi, yi, zi, xj, yj, zj
             real, intent(inout), dimension(BSIZE) :: vx, vy, vz
          end subroutine
          !! !$CSS TASK
          subroutine update_position(BSIZE, lam1, vx, vy, vz, x, y, z)
             implicit none
             integer, intent(in) :: BSIZE
             real, intent(in) :: lam1
             real, intent(inout), dimension(BSIZE) :: vx, vy, vz
             real, intent(inout), dimension(BSIZE) :: x, y, z
          end subroutine
          !! !$CSS TASK
          subroutine v_mod(BSIZE, v, vx, vy, vz)
             implicit none
             integer, intent(in) :: BSIZE
             real, intent(out) :: v(BSIZE)
             real, intent(in), dimension(BSIZE) :: vx, vy, vz
          end subroutine
       end interface

        !cridar subrutina dran_u per imposar posicions aleatries !!!
        iseed=1234
        call dran_ini(iseed)
        do i=1,N
          xh(i)=l*dran_u()
          yh(i)=l*dran_u()
          zh(i)=l*dran_u()
          x(i)=xh(i)
          y(i)=yh(i)
          z(i)=zh(i)
        enddo
        !       imposar velocitats aleatries !
        do nk=1,N
          winv(nk)=1.e0/mkg
        enddo
         
        iseed1=632
        call amrset(iseed1)
        call ranvel(0,1,N,v,winv,t1,iseed1,iscr)
           
        iseed=941

        call dran_ini(iseed)

        nq=0
        

        do i=1,N
          !  La subrutina dran_u() dona numeros aleatoris distribuits entre 0 i 1.
          ! les velocitats vnen en m/s
          ! A continuaci assigno els vectors velocitat inicial a cada tom d'arg.
          theta=pi*dran_u()
          phi=2.0e0*pi*dran_u()
          vx(i)=abs(v(i))*cos(theta)*cos(phi)
          vy(i)=abs(v(i))*cos(theta)*sin(phi)
          vz(i)=abs(v(i))*sin(theta)
        enddo

        !       comença bucle de temps!
        mm=0


!****************************************** START *****************************

!! !$CSS START

        do step=1,niter
          
          do ii=1, N, BSIZE
            do jj=1, N, BSIZE
              call velocity(BSIZE, ii, jj, x(ii), y(ii), z(ii), x(jj), y(jj), z(jj), vx(ii), vy(ii), vz(ii))
            enddo
          enddo
          
          do jj=1, N, BSIZE
             call v_mod(BSIZE, v(jj), vx(jj), vy(jj), vz(jj))
          enddo
          
!! !$CSS BARRIER
          tins=0.e0
          do i=1,N
            tins=mkg*v(i)**2/3.e0/kb+tins
          enddo
          tins=tins/N
          lam=sqrt(1.e0+dt/tau*(t/tins-1.e0))
          lam1=sqrt(t/tins)
         
          if(mod(step-1,10).eq.0 .and. step  /= 1)then
            mm=mm+1
            write(auxstr,111)mm
111         format(i7.7)
            fitxer='snap_argon'//auxstr//'.pdb'

            open(unit=33,file=fitxer,status='unknown')
         
            do i=1,N
              write(33,112)'ATOM',i,'CA','LEU',i,x(i),y(i),z(i)
            enddo
112         format(A4,3X,I4,2X,A2,2X,A3,2X,I4,5X,F7.3,1X,F7.3,1X,F7.3)
          endif


          do ii=1, N, BSIZE
            call update_position(BSIZE, lam1, vx(ii), vy(ii), vz(ii), x(ii), y(ii), z(ii))
          enddo
        enddo
!! !$CSS FINISH
end

!! !$CSS TASK
subroutine velocity(BSIZE, ii, jj, xi, yi, zi, xj, yj, zj, vx, vy, vz)
  implicit none
  integer, intent(in) :: BSIZE, ii, jj
  real, intent(in), dimension(BSIZE) :: xi, yi, zi, xj, yj, zj
  real, intent(inout), dimension(BSIZE) :: vx, vy, vz
  real :: xx, yy, zz, ffx, ffy, ffz, rr, ff
  real, parameter :: c=1.178e-10, d=3.4e0, m=40.e0, dt=1.e-12, mkg=m*1.67e-27
  integer :: i, j
  do i=1, BSIZE
    do j=1, BSIZE
      if ((ii+(i-1)).ne.(jj+(j-1))) then ! not the same atom
        xx = xi(i) - xj(j)
        yy = yi(i) - yj(j)
        zz = zi(i) - zj(j)
        rr = sqrt(xx**2+yy**2+zz**2) 
        if (rr.gt.3.4e0) then
          ff=c*(2.e0*(d/rr)**13-(d/rr)**7)
          
          ffx=ff*xx/rr
          ffy=ff*yy/rr
          ffz=ff*zz/rr
                                                                
          vx(i)=vx(i)+ffx/mkg*dt
          vy(i)=vy(i)+ffy/mkg*dt
          vz(i)=vz(i)+ffz/mkg*dt
        else
          ff=c*(2.e0*(d/3.4)**13-(d/3.4)**7)
     
          ffx=ff*xx/rr
          ffy=ff*yy/rr
          ffz=ff*zz/rr
           
          vx(i)=vx(i)+ffx/mkg*dt
          vy(i)=vy(i)+ffy/mkg*dt
          vz(i)=vz(i)+ffz/mkg*dt
         endif
       endif
     enddo
   enddo
end subroutine            

!! !$CSS TASK
subroutine update_position(BSIZE, lam1, vx, vy, vz, x, y, z)
   implicit none
   integer, intent(in) :: BSIZE
   real, intent(in) :: lam1
   real, intent(inout), dimension(BSIZE) :: vx, vy, vz
   real, intent(inout), dimension(BSIZE) :: x, y, z
   real, parameter :: l=1.e3, dt=1.e-12
   real :: x1, y1, z1
   integer :: i
   do i=1,BSIZE
     vx(i)=lam1*vx(i)
     vy(i)=lam1*vy(i)
     vz(i)=lam1*vz(i)
     ! multiplico per un factor 1.e10 per a passar velocitats a A/s i no en m/s com les he tingut fins ara.
     ! D'aquesta manera, les posicions se'm donaran en Amstrongs directament.
     x(i)=x(i)+vx(i)*dt*1.e10
     y(i)=y(i)+vy(i)*dt*1.e10
     z(i)=z(i)+vz(i)*dt*1.e10

     ! Aplico les condicions periòdiques de contorn mitjançant la subrutina conditions
     x1=x(i)
     y1=y(i)
     z1=z(i)

     x1=x1-l*int(x1/l)
     y1=y1-l*int(y1/l)
     z1=z1-l*int(z1/l)
  
     if(x1.lt.0.e0)then
       x1=l+x1
     endif
     if(y1.lt.0.e0)then
       y1=l+y1
     endif
     if(z1.lt.0.e0)then
       z1=l+z1
     endif

     x(i)=x1
     y(i)=y1
     z(i)=z1
   enddo
end subroutine

!! !$CSS TASK
subroutine v_mod(BSIZE, v, vx, vy, vz)
   implicit none
   integer, intent(in) :: BSIZE
   real, intent(out) :: v(BSIZE)
   real, intent(in), dimension(BSIZE) :: vx, vy, vz
   integer :: j
   do j=1,BSIZE
     v(j)=sqrt(vx(j)**2+vy(j)**2+vz(j)**2)
   enddo
end subroutine

!-----------------------------------------------------------------------------------------------

subroutine ranvel(nsel,npm,nrp,v,winv,temp0,ig,iscr)
    !   subroutine random velocities. this routine randomly selects nsel atoms and resets their 
    !   their velocities to random values with probabilities given by a maxwellian distribution 
    !   about temp0
    !
    !   nsel... number of randomly chosen atoms whose velocities will be randomly reassigned. if = 
    !   0, then all will be reassigned.
    !   npm... number of replicates. usually 1.
    !   nrp... number of atoms 
    !   v()... velocity array
    !   winv... inverse mass array
    !   ibel()... if ibelly>0, only atoms for wich ibel(i)>0 will move
    !   ibelly... >0 when belly being used.
    !   temp0... target temperature
    !   ig... seed for tandom number generator.
    !   iscr()... scratch array at least npm*nrp elements long.
    implicit none
    !   read coordinates and velocities
    integer iseed,ig,ntotat,npm,nrp,nchan,nsel,i,isel,j,jpt,ifill,m
    real winv(nrp),v(nrp)
    integer iscr(nrp)
    real boltz,y,sd,temp0
    
    iseed=ig
    !boltz=(8.31441e-3/4.184e0)*temp0
    boltz=1.38e-23*temp0
    ntotat=npm*nrp
    !   clear scratch array, after any velocity is reset, iscr(i) is set to 1. iscr(i) is set to 1 
    !   immediately (don't assign velocity) if ibel(i)=0 and belly is on.

    do 10 i=1,ntotat
      iscr(i)=0
      ! if(ibelly.gt.0 .and. ibel(i).eq.0) iscr(i)=1
10  continue
    !  do nsel velocity reassignments
    nchan=nsel
    if(nchan.gt.ntotat .or. nchan.eq.0) nchan=ntotat
    do 50 i=1,nchan
      ! generate a random number on the interval 1->ntotat. then search from that location 
      !forward, looking for the first still-untouched velocity. (loop around if we get to end of 
      !list before we find untouched velocity).
      call amrand(y)
      isel=min(int(y*float(ntotat))+1,ntotat)
      do 60 j=isel,isel+ntotat-1
        jpt=mod(j-1,ntotat)+1
        if(iscr(jpt).eq.0) goto 65
60    continue
      !   if we get here, no atoms left to change, return
      return
65    ifill=(jpt-1)
!65   ifill=3*(jpt-1)
      sd=boltz*winv(jpt)
      sd=sqrt(boltz*winv(jpt))
!     do 70 m=1,3
      call gauss(0.e0,sd,v(ifill + 1))!+m))
!70   continue
      iscr(jpt)=1
50  continue
    return
end

subroutine amrand(y)
    implicit none
    real u(97),c,cd,cm
    !.. real variables in marsaglia algorithm
    integer i97,j97
    !.. pointers into u() in marsaglia algorithm
    logical set
    !.. true if amrset has been called
    common /raset1/ u,c,cd,cm,i97,j97,set

    !    y: a random number between 0.0 and 1.0
    real y
    real uni
    if( .not. set ) then
        write(6,'(a)') 'amrand not initd'
    endif
    uni = u(i97)-u(j97)
    if (uni.lt. 0.0d0) uni=uni+1.0d0
    u(i97)=uni
    i97=i97-1
    if(i97.eq.0) i97=97
    j97=j97-1
    if (j97.eq.0) j97=97
    c=c-cd
    if(c.lt. 0.0d0) c=c+cm
    uni=uni-c
    if(uni.lt. 0.0d0)uni=uni+1.0d0
    y=uni
    y=real(uni)
    return
end

subroutine gauss(am,sd,v)
    implicit none
    ! this is a version of amrand() that adds the constraint of a gaussian distribution. it also 
    ! requires amrset to have been called first, and 'uses up' the same sequence that amrand() 
    ! does.
    real u(97),c,cd,cm, zero, six, a, uni, sd, am, v
    ! ..real variables in marsaglia algorithm
    integer i97,j97, i
    ! ..pointers into u() in marsaglia algorithm
    logical set
    ! ..true if amrset has been called
    !
    common /raset1/u,c,cd,cm,i97,j97,set
    data zero,six /0.0e0,6.0e0/
    if ( .not. set ) then
      write(6,'(a)') 'amrand not initd'
    !     call mexit(6, 1)
    endif
    a=zero
    do 50 i=1,12
         uni=u(i97)-u(j97)
         if(uni.lt.0.0e0) uni=uni+1.0e0
         u(i97)=uni
         i97=i97-1
         if(i97.eq.0)i97=97
         j97=j97-1
         if(j97.eq.0)j97=97
         c=c-cd
         if(c.lt.0.0d0)c=c+cm
         uni=uni-c
         if(uni.lt.0.0e0) uni=uni+1.0e0
         a=a+uni
50  continue
    v=(a-six)*sd+am
    return
end

subroutine amrset(iseed)
    implicit none
    integer iseed
    !  . integer seed greater than zero
    real u(97),c,cd,cm
    integer i97,j97
    logical set
    common /raset1/u,c,cd,cm,i97,j97,set
    integer is1,is2
    ! ..the two internal seeds used in marsaglia algorithm
    integer is1max
    ! ..max value of first seed (is1), 31328
    integer is2max
    ! ..max value of second seed (is2), 30081
    integer i,j,k,l,m
    ! ..used in generation of u()
    real s,t
    ! ..used in generation of u()
    integer ii,jj
    ! ..loop indices
    data is1max, is2max /31328, 30081/

    is1=max((iseed/is2max)+1,1)
    is1=min(is1,is1max)
    is2=max(1,mod(iseed,is2max)+1)
    is2=min(is2,is2max)
    i=mod(is1/177,177)+2
    j=mod(is1,177)+2
    k=mod(is2/169,178)+1
    l=mod(is2,169)
    do 200 ii=1,97
         s=0.0d0
         t=0.5d0
         do 100 jj=1,24
              m=mod(mod(i*j,179)*k,179)
              i=j
              j=k
              k=m
              l=mod(53*l+1,169)
              if(mod(l*m,64).ge.32)s=s+t
              t=0.5d0*t
100      continue
         u(ii)=s
200 continue     
    c=362436.0d0/16777216.0d0
    cd=7654321.0d0/16777216.0d0
    cm=16777213.0d0/16777216.0d0
    i97=97
    j97=33
    set=.true.
    return
end

subroutine dran_ini(iseed0)
    implicit none
    integer, parameter :: ip=1279
    integer, parameter :: np=14
    integer, parameter :: nbit=31!,pi=3.1415927e0)
    integer, parameter :: m=2**np,np1=nbit-np,nn=2**np1-1,nn1=nn+1
    integer :: ix(ip), iseed0, ic, i, j
    double precision :: c0,c1,c2,d1,d2,d3,dseed,t,x,u2th,p,pi, rand_xx
    real g(0:m)

    data c0,c1,c2/2.515517,0.802853,0.010328/
    data d1,d2,d3/1.432788,0.189269,0.001308/

    common /ixx/ ix
    common /icc/ ic
    common /gg/ g

    dseed=iseed0
    do 200 i=1,ip
      ix(i)=0
      do 200 j=0,nbit-1
        if(rand_xx(dseed).lt.0.5) ix(i)=ibset(ix(i),j)
    200 continue
    ic=0
    pi=4.0d0*datan(1.0d0)
    do 1 i=m/2,m
      p=1.0-real(i+1)/(m+2)
      t=sqrt(-2.0*log(p))
      x=t-(c0+t*(c1+c2*t))/(1.0+t*(d1+t*(d2+t*d3)))
      g(i)=x
      g(m-i)=-x
1   continue
    u2th=1.0-real(m+2)/m*sqrt(2.0/pi)*g(m)*exp(-g(m)*g(m)/2)
    u2th=nn1*sqrt(u2th)
    do 856 i=0,m
856   g(i)=g(i)/u2th
    return
end



function dran_u()
    implicit none
    integer, parameter :: ip=1279
    integer, parameter :: iq=418
    integer, parameter :: is=ip-iq
    integer, parameter :: rmax=2147483647
    integer ix(ip),i,j, ic
    double precision :: dran_u
    common /ixx/ ix
    common /icc/ ic

    ic=ic+1
    if(ic.gt.ip) ic=1
    if(ic.gt.iq) then
            i=ix(ic)
            j=ix(ic-iq)
            ix(ic)=ieor(i,j)
    else
            i=ix(ic)
            j=ix(ic+is)
            ix(ic)=ieor(i,j)
    endif
    dran_u=real(ix(ic))/rmax
    return
end

subroutine dran_gv(u,n)
    implicit none
    integer, parameter :: ip=1279
    integer, parameter :: iq=418
    integer, parameter :: np=14
    integer, parameter :: nbit=31
    integer, parameter :: m=2**np,np1=nbit-np,nn=2**np1-1,nn1=nn+1
    integer, parameter :: is=ip-iq
    integer l,s, ic, i, i2, n, k
    real g(0:m)
    real u(n)
    real ix(ip)
    common /ixx/ ix
    common /icc/ic
    common /gg/ g

    do 99 k=1,n
    ic=ic+1
    if(ic.gt.ip) ic=1
    if(ic.gt.iq) then
            l=ix(ic)
            s=ix(ic-iq)

            ix(ic)=ieor(l,s)
    else
            l=ix(ic)
            s=ix(ic+is)
            ix(ic)=ieor(l,s)
    endif
    l=ix(ic)
    s=-np1
    i=ishft(l,s)
    l=ix(ic)
    s=nn
    i2=iand(l,s)
    u(k)=i2*g(i+1)+(nn1-i2)*g(i)
99  continue
    return
end

function rand_xx(dseed)
    implicit none
    double precision a,c,xmm,rm,dseed,rand_xx
    parameter (xmm=2.d0**32,rm=1.d0/xmm,a=69069.d0,c=1.d0)
    dseed=mod(dseed*a+c,xmm)
    rand_xx=dseed*rm
    return
end

