module space3d

implicit none

type point
real(kind(0.d0)) :: x, y, z
end type point

type vector
real(kind(0.d0)) :: vx, vy, vz
end type vector

interface assignment (=)
module procedure eqpoints, eqvectors
end interface

interface operator (+)
module procedure point_add_point, point_add_vector, vector_add_vector
end interface

interface operator(-)
module procedure point_diff_point, vector_diff_vector
end interface

interface operator(*)
module procedure point_mult_no, no_mult_point, vector_mult_no, no_mult_vector, point_mult_dbl, dbl_mult_point, vector_mult_dbl, dbl_mult_vector, vector_mult_vector
end interface

interface operator(/)
module procedure point_div_no, vector_div_no, point_div_dbl, vector_div_dbl
end interface

interface operator(.x.)
module procedure vector_x_vector
end interface

contains

elemental subroutine eqpoints(p1,p2)
type(point), intent(out) :: p1
type(point), intent(in) :: p2
p1%x=p2%x
p1%y=p2%y
p1%z=p2%z
end subroutine eqpoints

elemental subroutine eqvectors(v1,v2)
type(vector), intent(out) :: v1
type(vector), intent(in) :: v2
v1%vx=v2%vx
v1%vy=v2%vy
v1%vz=v2%vz
end subroutine eqvectors

type(point) elemental function point_add_point(p1,p2) result(p3)
type(point), intent(in) :: p1, p2
p3%x=p1%x+p2%x
p3%y=p1%y+p2%y
p3%z=p1%z+p2%z
end function point_add_point

type(point) elemental function point_add_vector(p1,v2) result(p3)
type(point)  , intent(in) :: p1
type(vector), intent(in) :: v2
p3%x=p1%x+v2%vx
p3%y=p1%y+v2%vy
p3%z=p1%z+v2%vz
end function point_add_vector

type(vector) elemental function vector_add_vector(v1,v2) result(v3)
type(vector), intent(in) :: v1
type(vector), intent(in) :: v2
v3%vx=v1%vx+v2%vx
v3%vy=v1%vy+v2%vy
v3%vz=v1%vz+v2%vz
end function vector_add_vector

type(vector) elemental function point_diff_point(p1,p2) result(v3)
type(point), intent(in) :: p1, p2
v3%vx=p1%x-p2%x
v3%vy=p1%y-p2%y
v3%vz=p1%z-p2%z
end function point_diff_point

type(vector) elemental function vector_diff_vector(v1,v2) result(v3)
type(vector), intent(in) :: v1, v2
v3%vx=v1%vx-v2%vx
v3%vy=v1%vy-v2%vy
v3%vz=v1%vz-v2%vz
end function vector_diff_vector

type(point) elemental function point_mult_no(p1,no) result(p2)
type(point)       , intent(in) :: p1
real, intent(in) :: no
p2%x=p1%x*no
p2%y=p1%y*no
p2%z=p1%z*no
end function point_mult_no

type(point) elemental function point_mult_dbl(p1,no) result(p2)
type(point)       , intent(in) :: p1
real(kind(0.d0)), intent(in) :: no
p2%x=p1%x*no
p2%y=p1%y*no
p2%z=p1%z*no
end function point_mult_dbl

type(point) elemental function no_mult_point(no,p1) result(p2)
real, intent(in) :: no
type(point)       , intent(in) :: p1
p2%x=no*p1%x
p2%y=no*p1%y
p2%z=no*p1%z
end function no_mult_point

type(point) elemental function dbl_mult_point(no,p1) result(p2)
real(kind(0.d0)), intent(in) :: no
type(point)       , intent(in) :: p1
p2%x=no*p1%x
p2%y=no*p1%y
p2%z=no*p1%z
end function dbl_mult_point

type(vector) elemental function vector_mult_no(v1,no) result(v2)
type(vector)     , intent(in) :: v1
real, intent(in) :: no
v2%vx=v1%vx*no
v2%vy=v1%vy*no
v2%vz=v1%vz*no
end function vector_mult_no

type(vector) elemental function vector_mult_dbl(v1,no) result(v2)
type(vector)     , intent(in) :: v1
real(kind(0.d0)), intent(in) :: no
v2%vx=v1%vx*no
v2%vy=v1%vy*no
v2%vz=v1%vz*no
end function vector_mult_dbl

type(vector) elemental function no_mult_vector(no,v1) result(v2)
real, intent(in) :: no
type(vector)     , intent(in) :: v1
v2%vx=no*v1%vx
v2%vy=no*v1%vy
v2%vz=no*v1%vz
end function no_mult_vector

type(vector) elemental function dbl_mult_vector(no,v1) result(v2)
real(kind(0.d0)), intent(in) :: no
type(vector)     , intent(in) :: v1
v2%vx=no*v1%vx
v2%vy=no*v1%vy
v2%vz=no*v1%vz
end function dbl_mult_vector

type(point) elemental function point_div_no(p1,no) result(p2)
type(point)       , intent(in) :: p1
real, intent(in) :: no
p2%x=p1%x/no
p2%y=p1%y/no
p2%z=p1%z/no
end function point_div_no

type(point) elemental function point_div_dbl(p1,no) result(p2)
type(point)       , intent(in) :: p1
real(kind(0.d0)), intent(in) :: no
p2%x=p1%x/no
p2%y=p1%y/no
p2%z=p1%z/no
end function point_div_dbl

type(vector) elemental function vector_div_no(v1,no) result(v2)
type(vector)     , intent(in) :: v1
real, intent(in) :: no
v2%vx=v1%vx/no
v2%vy=v1%vy/no
v2%vz=v1%vz/no
end function vector_div_no

type(vector) elemental function vector_div_dbl(v1,no) result(v2)
type(vector)     , intent(in) :: v1
real(kind(0.d0)), intent(in) :: no
v2%vx=v1%vx/no
v2%vy=v1%vy/no
v2%vz=v1%vz/no
end function vector_div_dbl

real(kind(0.d0)) elemental function vector_mult_vector(v1,v2) result(inpro)
type(vector), intent(in) :: v1, v2
inpro=v1%vx*v2%vx+v1%vy*v2%vy+v1%vz*v2%vz
end function vector_mult_vector

type(vector) elemental function vector_x_vector(v1,v2) result(v3)
type(vector), intent(in) :: v1, v2
v3%vx=v1%vy*v2%vz-v1%vz*v2%vy
v3%vy=v1%vz*v2%vx-v1%vx*v2%vz
v3%vz=v1%vx*v2%vy-v1%vy*v2%vx
end function vector_x_vector

real(kind(0.d0)) elemental function norm(v1) result(v_l)
type(vector), intent(in) :: v1
v_l=sqrt(v1%vx**2+v1%vy**2+v1%vz**2)
end function norm

real(kind(0.d0)) elemental function norm2(v1) result(v_l)
type(vector), intent(in) :: v1
v_l=v1%vx**2+v1%vy**2+v1%vz**2
end function norm2

type(vector) elemental function unit(v1) result(uv1)
type(vector), intent(in) :: v1
uv1%vx=v1%vx/norm(v1)
uv1%vy=v1%vy/norm(v1)
uv1%vz=v1%vz/norm(v1)
end function unit

end module space3d

module functions

contains

real(kind(0.d0)) function fact(n) result(ff)
integer, intent(in) :: n
integer :: i
ff=1
if ( n /= 0 ) then
   do i=1, n
      ff=i*ff
   end do
end if
end function fact


end module functions


module implied_defs

use space3d

implicit none

real(kind(0.d0)), parameter :: pi=acos(-0.1d1)
type(point), parameter :: O=point(0d0,0d0,0d0)
type(vector), parameter :: ii=vector(1d0,0d0,0d0), jj=vector(0d0,1d0,0d0), kk=vector(0d0,0d0,1d0), Vinf=vector(0d0,0d0,0d0), zero_v=vector(0d0,0d0,0d0)
integer :: test=0
logical :: signal

end module implied_defs

module periodic_parameters

use space3d
use implied_defs

implicit none

real(kind(0.d0)) , dimension(10) :: Bernoulli = (/ 1d0/6d0 , -1d0/3d1 , 1d0/4.2d1 , -1d0/3d1 , 5d0/6.6d1, -6.91d2/2.730d3 , 7d0/6d0, -3.617d3/5.10d2 , 4.3867d4/7.98d2 , -1.74611d5/3.30d2 /)  
real(kind(0.d0)), dimension(:,:) , allocatable :: M_coefs, N_coefs
integer, parameter:: ns=2 ,n=5, m=4
type(vector), parameter :: le=jj
real(kind(0.d0)), parameter :: ll=0.60d2
type(point), parameter :: Rp=point(0d0,0d0,0d0)

contains

subroutine find_M_N
integer :: i, q

if (m /= 0) then
   allocate(M_coefs(0:m+1,2*m+2))
   M_coefs(0,1)=-0.15d1
   do i=1,m+1
      M_coefs(0,2*i)=M_coefs(0,2*i-1)*(-0.5d0-2*i)
      if (i /= m+1) M_coefs(0,2*i+1)=M_coefs(0,2*i)*(-0.15d1-2*i)
      if (i /= 1) then 
         do q=1,i-1
            M_coefs(q,2*i)=M_coefs(q-1,2*i-1)*(2*i-2*q+0.1d1)+M_coefs(q,2*i-1)*(-0.5d0-2*i+q)
            if (i /= m+1) M_coefs(q,2*i+1)=M_coefs(q-1,2*i)*(2*i-2*q+0.2d1)+M_coefs(q,2*i)*(-0.15d1-2*i+q)
         end do
      end if
      M_coefs(i,2*i)=M_coefs(i-1,2*i-1)
      if (i /= m+1) then 
         M_coefs(i,2*i+1)=M_coefs(i,2*i)*(-0.15d1-i)+0.2d1*M_coefs(i-1,2*i)
      end if
   end do
end if

if (m /= 0) then
   allocate(N_coefs(0:m+1,2*m+2))
   N_coefs(0,1)=-0.25d1
   do i=1,m+1
      N_coefs(0,2*i)=N_coefs(0,2*i-1)*(-0.15d1-2*i)
      if (i /= m+1) N_coefs(0,2*i+1)=N_coefs(0,2*i)*(-0.25d1-2*i)
      if (i /= 1) then 
         do q=1,i-1
            N_coefs(q,2*i)=N_coefs(q-1,2*i-1)*(2*i-2*q+0.1d1)+N_coefs(q,2*i-1)*(-0.15d1-2*i+q)
            if (i /= m+1) N_coefs(q,2*i+1)=N_coefs(q-1,2*i)*(2*i-2*q+0.2d1)+N_coefs(q,2*i)*(-0.25d1-2*i+q)
         end do
      end if
      N_coefs(i,2*i)=N_coefs(i-1,2*i-1)
      if (i /= m+1) then 
         N_coefs(i,2*i+1)=N_coefs(i,2*i)*(-0.25d1-i)+0.2d1*N_coefs(i-1,2*i)
      end if
   end do
end if

end subroutine find_M_N

end module periodic_parameters

module sing_elem

use space3d
use functions
use implied_defs

implicit none

type mpoint
type(point) :: p
type(vector) :: v
end type mpoint

type pvortex
type(mpoint) :: mp
type(vector) :: G
type(vector) :: dG
real(kind(0.d0)) :: u
real(kind(0.d0)) :: pmove
real(kind(0.d0)) :: e_mol
real(kind(0.d0)) :: d_core
end type pvortex

type psource
type(mpoint) :: mp
real(kind(0.d0)) :: S
end type psource


interface assignment(=)
module procedure eqsources, eqvortices
end interface

interface operator(.dist.)
module procedure distps, distpv, distsp, distss, distsv, distvp, distvs, distvv
end interface

interface indvel
module procedure ind_vel_sp, ind_vel_vp, ind_vel_ss, ind_vel_vv, ind_vel_sv, ind_vel_vs
end interface

interface periodic_indvel
module procedure periodic_ind_vel_vv, periodic_ind_vel_vp
end interface

contains

elemental subroutine eqmpoints(mp1,mp2)
type(mpoint), intent(out) :: mp1
type(mpoint), intent(in)   :: mp2
mp1%p=mp2%p
mp1%v=mp2%v
end subroutine eqmpoints

elemental subroutine eqvortices(vo1,vo2)
type(pvortex), intent(out) :: vo1
type(pvortex), intent(in)   :: vo2
vo1%mp%p=vo2%mp%p
vo1%mp%v=vo2%mp%v
vo1%G=vo2%G
vo1%dG=vo2%dG
vo1%e_mol=vo2%e_mol
vo1%u=vo2%u
vo1%pmove=vo2%pmove
vo1%d_core=vo2%d_core
end subroutine eqvortices

elemental subroutine eqsources(so1,so2)
type(psource), intent(out) :: so1
type(psource), intent(in)   :: so2
so1%mp=so2%mp
so1%S=so2%S
end subroutine eqsources

real(kind(0.d0)) elemental function distsp(so1,mp2) result(dis)
type(psource), intent(in) :: so1
type(mpoint) , intent(in) :: mp2
dis=norm(mp2%p-so1%mp%p)
end function distsp

real(kind(0.d0)) elemental function distvp(vo1,mp2) result(dis)
type(pvortex), intent(in) :: vo1
type(mpoint) , intent(in) :: mp2
dis=norm(mp2%p-vo1%mp%p)
end function distvp

real(kind(0.d0)) elemental function distps(mp1,so2) result(dis)
type(mpoint) , intent(in) :: mp1
type(psource), intent(in) :: so2
dis=norm(mp1%p-so2%mp%p)
end function distps

real(kind(0.d0)) elemental function distpv(mp1,vo2) result(dis)
type(mpoint) , intent(in) :: mp1
type(pvortex), intent(in) :: vo2
dis=norm(mp1%p-vo2%mp%p)
end function distpv

real(kind(0.d0)) elemental function distss(so1,so2) result(dis)
type(psource), intent(in) :: so1, so2
dis=norm(so2%mp%p-so1%mp%p)
end function distss

real(kind(0.d0)) elemental function distsv(so,vo) result(dis)
type(psource), intent(in) :: so
type(pvortex), intent(in) :: vo
dis=norm(so%mp%p-vo%mp%p)
end function distsv

real(kind(0.d0)) elemental function distvs(vo,so) result(dis)
type(pvortex), intent(in) :: vo
type(psource), intent(in) :: so
dis=norm(vo%mp%p-so%mp%p)
end function distvs

real(kind(0.d0)) elemental function distvv(vo1,vo2) result(dis)
type(pvortex), intent(in) :: vo1, vo2
dis=norm(vo1%mp%p-vo2%mp%p)
end function distvv

subroutine ind_vel_vp(vo1,mp1)
use ieee_exceptions
implicit none
type(pvortex), intent(in)      :: vo1
type(mpoint) , intent(inout) :: mp1
type(vector)                          :: dv
real(kind(0.d0))                     :: dd, dde
logical :: i_signal
call ieee_get_flag(ieee_invalid,i_signal)
if (i_signal) then 
  signal=.true.
  print *, 'Input at induced velocity already a NaN'
  call ieee_set_flag(ieee_invalid,.false.)
  return
end if
dv=mp1%p-vo1%mp%p
dd=norm(dv)
!dde=0.1d1-exp(-(dd/e_mol)**3)
dde=0.1d1-exp(-(dd/vo1%e_mol)**3)
dd=norm2(dv)
dv=unit(dv)
call ieee_get_flag(ieee_invalid,i_signal)
if (i_signal) then
  signal=.true.
    print *, 'Problem at induced velocity pre-calculations'
    print *, 'I got:'
    print *, 'vec r1',mp1%p
    print *, 'vec r2', vo1%mp%p
    print *, 'r1-r2=',mp1%p-vo1%mp%p
    print *, 'unit(r1-r2)=', dv
    print *,'1-exp(-(abs(r1-r2)/emol)**3)', dde
    call ieee_set_flag(ieee_invalid,.false.)
    return
else   
    !dv=(vo1%G.x.dv)*dde/(0.4d1*pi*dd)
    dv=dde/dd/0.4d1/pi*norm(vo1%G)*(unit(vo1%G).x.dv)
    call ieee_get_flag(ieee_invalid,i_signal)
    if (i_signal) then
       signal=.true.
       print *, 'Problem at induced velocity calculations'
       print *, 'I get:'
       print *, 'vo1%G',vo1%G
       print *, 'vo1%G.x.unit(dv)=',vo1%G.x.dv
       print *, '(0.1d1-exp(-dde))=',dde
       print *,'0.1d1/(0.4d1*pi*norm2(dv))=',0.1d1/(0.4d1*pi*dd)
       print *,'(vo1%G.x.dv)*dde/(0.4d1*pi*dd)=',(vo1%G.x.dv)*dde/(0.4d1*pi*dd)
       call ieee_set_flag(ieee_invalid,.false.)
       return
    else
      mp1%v=mp1%v+dv
    end if
end if
end subroutine ind_vel_vp

subroutine ind_vel_sp(so1,mp1)
type(psource), intent(in)      :: so1
type(mpoint) , intent(inout) :: mp1
mp1%v=so1%S*(mp1%p-so1%mp%p)/(4*pi*norm(mp1%p-so1%mp%p)**3)+mp1%v
mp1%p=mp1%p
end subroutine ind_vel_sp

!subroutine ind_vel_vv(vo1,vo2)
!type(pvortex), intent(in) :: vo1
!type(pvortex), intent(inout) :: vo2
!if (norm(vo2%mp%p-vo1%mp%p) /= 0.) then
!  vo2%mp%v=vo1%G*(vo2%mp%p-vo1%mp%p)/(4*pi*(norm(vo2%mp%p-vo1%mp%p))**3)+vo2%mp%v
!end if
!vo2%mp%p=vo2%mp%p
!vo2%G=vo2%G
!end subroutine ind_vel_vv

subroutine ind_vel_vv(vo1,vo2)
type(pvortex), intent(in)      :: vo1
type(pvortex), intent(inout) :: vo2
call ind_vel_vp(vo1,vo2%mp)
vo2%G=vo2%G
vo2%dG=vo2%dG
end subroutine ind_vel_vv

subroutine ind_vel_sv(so1,vo2)
type(psource), intent(in)      :: so1
type(pvortex), intent(inout) :: vo2
call ind_vel_sp(so1,vo2%mp)
vo2%G=vo2%G
vo2%dG=vo2%dG
end subroutine ind_vel_sv

subroutine ind_vel_vs(vo1,so2)
type(pvortex), intent(in)      :: vo1
type(psource), intent(inout) :: so2
call ind_vel_vp(vo1,so2%mp)
so2%S=so2%S
end subroutine ind_vel_vs

subroutine ind_vel_ss(so1,so2)
type(psource), intent(in)      :: so1
type(psource), intent(inout) :: so2
call ind_vel_sp(so1,so2%mp)
so2%S=so2%S
end subroutine ind_vel_ss

subroutine ind_G_vv(vo1,vo2)
use ieee_exceptions
type(pvortex), intent(in)      :: vo1
type(pvortex), intent(inout) :: vo2
type(vector)                          :: dv
real(kind(0.d0))                    :: dd, dde
!vo2%dG=((vo1%G.x.vo2%G)*(1.-exp(-1*dde))/dd**3-3.*(vo2%G*dv)*(vo1%G.x.dv)*(1.-(1+dde)*exp(-1*dde))/dd**5)/(4*pi)+vo2%dg
logical :: i_signal
call ieee_get_flag(ieee_invalid,i_signal)
if (i_signal) then 
   signal=.true.
   print *, 'Input at vorticity eq already a NaN'
   call ieee_set_flag(ieee_invalid,.false.)
   return
end if
dv=vo2%mp%p-vo1%mp%p
dd=norm(dv)
!dde=exp(-(dd/e_mol)**3)
dde=exp(-(dd/vo1%e_mol)**3)
dd=dd**3
dv=unit(dv)
call ieee_get_flag(ieee_invalid,i_signal)
if (i_signal) then
   signal=.true.
    print *, 'Problem at induced vorticity pre-calculations'
    print *, 'I get:'
    print *, 'unit(dv)=', dv
    print *,'dde=', dde
    print *, 'dd**3=',dd
    call ieee_set_flag(ieee_invalid,.false.)
    return
else   
    !dv=((vo1%G.x.vo2%G)*(0.1d1-dde)-(0.3d1*((vo2%G*dv)*(vo1%G.x.dv))*(0.1d1-(0.1d1+dd/vo1%e_mol**3)*dde)))/(0.4d1*pi*dd)
    !dv=((unit(vo1%G).x.unit(vo2%G))-(0.3d1*((unit(vo2%G)*dv)*(unit(vo1%G).x.dv))))*(0.1d1-dde)/dd*norm(vo1%G)*norm(vo2%G)/(0.4d1*pi)    
    dv=((unit(vo1%G).x.unit(vo2%G))*(0.1d1-dde)/dd-(0.3d1*((unit(vo2%G)*dv)*(unit(vo1%G).x.dv))*(0.1d1-(0.1d1+dd/vo1%e_mol**3)*dde)/dd))*norm(vo1%G)*norm(vo2%G)/(0.4d1*pi)
    call ieee_get_flag(ieee_invalid,i_signal)
    if (i_signal) then
        signal=.true.
        print *, 'Problem at vorticity calculations'
        print *, 'I get:'
        print *, 'e_mol=',vo1%e_mol
        print *, 'vo1%G.x.vo2%G=', vo1%G.x.vo2%G
        print *, 'norm(dv)=',norm(vo2%mp%p-vo1%mp%p)
        print *, 'dde=',dde 
        PRINT *,'(0.1d1-(0.1d1-log(dde))*dde)=',(0.1d1-(0.1d1+dd/vo1%e_mol**3)*dde)
        print *, '(0.1d1-exp(-dde))=',0.1d1-dde
        print *,'(vo2%G*dv)=',(vo2%G*unit(vo2%mp%p-vo1%mp%p))
        print *,'(vo1%G.x.dv)=',(vo1%G.x.unit(vo2%mp%p-vo1%mp%p))
        print *,'(vo2%G*dv)*(vo1%G.x.dv)=',(vo2%G*unit(vo2%mp%p-vo1%mp%p))*(vo1%G.x.unit(vo2%mp%p-vo1%mp%p))
        print *,'1/(0.4d1*pi*dd)=',0.1d1/(0.4d1*pi*dd)
        call ieee_set_flag(ieee_invalid,.false.)
       return
    else
       vo2%dG = vo2%dG + dv
    end if
end if
end subroutine ind_G_vv

subroutine make_vps_help(voo1,voo2)
use periodic_parameters
type(pvortex), intent(in), dimension(:) :: voo1
type(pvortex), intent(out), dimension((2*ns-1)*size(voo1)) :: voo2
!real(kind(0.d0)), dimension(size(voo1),size(voo1)) :: helpp
integer :: i
voo2(1:size(voo1))=voo1
where((voo1%mp%p-Rp)*le >= ll/0.2d1)   
voo2(1:size(voo1))%mp%p=voo1%mp%p+(-ll)*le
voo2(1:size(voo1))%pmove=0.1d1+voo2(1:size(voo1))%pmove
end where
where((voo1%mp%p-Rp)*le < -ll/0.2d1 )    
voo2(1:size(voo1))%mp%p=voo1%mp%p+ll*le
voo2(1:size(voo1))%pmove=-0.1d1+voo2(1:size(voo1))%pmove
end where
!forall (i=1:size(voo1), j=1:size(voo1) helpp(i,j)=(voo1(i)%mp%p-voo1(j)%mp%p)*le
!ll=maxval(helpp)
do i=1,2*(ns-1)
   voo2(i*size(voo1)+1:(i+1)*size(voo1))=voo2(1:size(voo1))
   voo2(i*size(voo1)+1:(i+1)*size(voo1))%mp%p=voo2(1:size(voo1))%mp%p+(le*(ll*(-0.1d1)**i*((i+1)/2)))
end do
end subroutine make_vps_help

subroutine periodic_ind_vel_vv(vo1,vo2)
use ieee_exceptions
use periodic_parameters
implicit none
type(pvortex), intent(in)      :: vo1
type(pvortex), intent(inout) :: vo2
type(vector)                          :: dvv
integer                                   :: i, j
real(kind(0.d0))                     :: k, k1, c, ss, ss1, der, der1, hl, hr
logical :: isignal

 dvv=vo2%mp%p-vo1%mp%p
 c=norm(dvv)/ll
 k=dvv*le/ll

call ieee_get_flag(ieee_invalid,isignal)

if (isignal) then
 signal=.true. 
  print *, 'Input error at periodic induced velocity'
   call ieee_set_flag(ieee_invalid,.false.)
   return

else

ss=0d0
ss1=0d0

k1=k
if (k<0d0) k1=-k
 
   if (c<0.3d-4) then
       ss=0.2d1*0.2020569031572758d0
       ss1=0d0
   else if (abs(c-k1)<=0.1d-7) then
      do i=ns,n
         ss=ss+(i+k)**-2-(i-k)**-2
      end do
      if (m/=1) then
         do i=1,m
            ss=ss-Bernoulli(i)*((n+k)**-(2*i+1)-(n-k)**-(2*i+1))
         end do
      end if
      ss=ss+0.2d1*n*k/(n**2-k**2)**2-0.2d1*k/(n**2-k**2)
   call ieee_get_flag(ieee_invalid,isignal)
   if (isignal) then
      signal=.true.
       print *, 'parallel case periodic induced velocity problem'
       print *, 'I get:'
       print *, 'ss=',ss
       print *, 'ss1=',ss1
       print *, 'and i found them using'
       print *,'c=',c
       print *,'k=',k
       call ieee_set_flag(ieee_invalid,.false.)
       return
   end if
   else
      hl=sqrt(c**2+0.2d1*k*n+n**2)
      hr=sqrt(c**2-0.2d1*k*n+n**2)
      do i=ns,n
         ss=ss+sqrt(c**2+0.2d1*k*i+i**2)**-3+sqrt(c**2-0.2d1*k*i+i**2)**-3
         ss1=ss1+i/sqrt(c**2+0.2d1*k*i+i**2)**3-i/sqrt(c**2-0.2d1*k*i+i**2)**3
      end do
      if (m/=1) then
       do i=1,m-1
         der=0d0
         der1=0d0
         do j=0,i-1
            der=der+M_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(1+4*i-2*j)+(n-k)**(2*i-2*j-1)/hr**(1+4*i-2*j))*0.2d1**(2*i-j-1)
            der1=der1+M_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(1+4*i-2*j)-(n-k)**(2*i-2*j-1)/hr**(1+4*i-2*j))*0.2d1**(2*i-j-1) ! first part of j/(c^2+2*k*j+j^2)^(3/2)-j/(c^2-2*k*j+j^2)^(3/2) derivative
         end do
         if (i==1) then !second part calcuation
            der1=hl**-3-hr**-3+n*der1
         else
            der1=der1*n
            do j=0,i-1
               der1=der1+(0.2d0*i-0.1d1)*M_coefs(j,2*i-2)*((k+n)**(2*i-2*j-2)/hl**(4*i-2*j-1)-(n-k)**(2*i-2*j-2)/hr**(4*i-2*j-1))*0.2d1**(2*i-j-2)
            end do
         end if
         ss=ss-Bernoulli(i)*der/fact(2*i)
         ss1=ss1-Bernoulli(i)*der1/fact(2*i)
       end do
     end if
      ss=ss-0.5d0/hl**3-0.5d0/hr**3+(0.2d1-(n+k)/hl-(n-k)/hr)/(c**2-k**2)
      ss1=ss1+((k*n+c**2)/hl+(k*n-c**2)/hr-0.2d1*k)/(c**2-k**2)-n*0.5d0/hl**3+n*0.5d0/hr**3
   call ieee_get_flag(ieee_invalid,isignal)
   if (isignal) then
       signal=.true.
       print *, 'default case periodic induced velocity problem'
       print *, 'I get:'
       print *, 'ss=',ss
       print *, 'ss1=',ss1
       print *, 'and i found them using'
       print *,'c=',c
       print *,'k=',k
       call ieee_set_flag(ieee_invalid,.false.)
       return
   end if
   end if

  
   if ((c<0.3d-4) .or. (abs(c-k1)<=0.1d-7))  then
      dvv=((vo1%G.x.le)/(0.4d1*pi*ll**2)*ss)
   else
      dvv=(((vo1%G.x.unit(dvv))*c*ss)+((vo1%G.x.le)*ss1))/(0.4d1*pi*ll**2)
  end if

  call ieee_get_flag(ieee_invalid,isignal)
  if (isignal) then
      signal=.true.
      print *, 'Problem at periodic induced velocity calculations'
      print *, 'I get'
      print *, 'vo1%G=',vo1%G
      print *, 'ss=',ss
      print *, 'ss1=',ss1
      print *, 'and i found them using'
      print *, 'c=',c
      print *, 'k=',k
      call ieee_set_flag(ieee_invalid,.false.)
      return
  else
      vo2%mp%v=dvv+vo2%mp%v
  end if
      
end if
end subroutine periodic_ind_vel_vv

subroutine periodic_ind_vel_vp(vo1,mp2)
use ieee_exceptions
use periodic_parameters
implicit none
type(pvortex), intent(in)      :: vo1
type(mpoint), intent(inout) :: mp2
type(vector)                          :: dvv
integer                                   :: i, j
real(kind(0.d0))                     :: k, k1, c, ss, ss1, der, der1, hl, hr
logical :: isignal

 dvv=mp2%p-vo1%mp%p
 c=norm(dvv)/ll
 k=dvv*le/ll

call ieee_get_flag(ieee_invalid,isignal)

if (isignal) then
 signal=.true. 
  print *, 'Input error at periodic induced velocity'
   call ieee_set_flag(ieee_invalid,.false.)
   return

else

ss=0d0
ss1=0d0

k1=k
if (k<0d0) k1=-k
 
   if (c<0.3d-4) then
       ss=0.2d1*0.2020569031572758d0
       ss1=0d0
   else if (abs(c-k1)<=0.1d-7) then
      do i=ns,n
         ss=ss+(i+k)**-2-(i-k)**-2
      end do
      if (m/=1) then
         do i=1,m
            ss=ss-Bernoulli(i)*((n+k)**-(2*i+1)-(n-k)**-(2*i+1))
         end do
      end if
      ss=ss+0.2d1*n*k/(n**2-k**2)**2-0.2d1*k/(n**2-k**2)
   call ieee_get_flag(ieee_invalid,isignal)
   if (isignal) then
      signal=.true.
       print *, 'parallel case periodic induced velocity problem'
       print *, 'I get:'
       print *, 'ss=',ss
       print *, 'ss1=',ss1
       print *, 'and i found them using'
       print *,'c=',c
       print *,'k=',k
       call ieee_set_flag(ieee_invalid,.false.)
       return
   end if
   else
      hl=sqrt(c**2+0.2d1*k*n+n**2)
      hr=sqrt(c**2-0.2d1*k*n+n**2)
      do i=ns,n
         ss=ss+sqrt(c**2+0.2d1*k*i+i**2)**-3+sqrt(c**2-0.2d1*k*i+i**2)**-3
         ss1=ss1+i/sqrt(c**2+0.2d1*k*i+i**2)**3-i/sqrt(c**2-0.2d1*k*i+i**2)**3
      end do
      if (m/=1) then
       do i=1,m-1
         der=0d0
         der1=0d0
         do j=0,i-1
            der=der+M_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(1+4*i-2*j)+(n-k)**(2*i-2*j-1)/hr**(1+4*i-2*j))*0.2d1**(2*i-j-1)
            der1=der1+M_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(1+4*i-2*j)-(n-k)**(2*i-2*j-1)/hr**(1+4*i-2*j))*0.2d1**(2*i-j-1) ! first part of j/(c^2+2*k*j+j^2)^(3/2)-j/(c^2-2*k*j+j^2)^(3/2) derivative
         end do
         if (i==1) then !second part calcuation
            der1=hl**-3-hr**-3+n*der1
         else
            der1=der1*n
            do j=0,i-1
               der1=der1+(0.2d0*i-0.1d1)*M_coefs(j,2*i-2)*((k+n)**(2*i-2*j-2)/hl**(4*i-2*j-1)-(n-k)**(2*i-2*j-2)/hr**(4*i-2*j-1))*0.2d1**(2*i-j-2)
            end do
         end if
         ss=ss-Bernoulli(i)*der/fact(2*i)
         ss1=ss1-Bernoulli(i)*der1/fact(2*i)
       end do
     end if
      ss=ss-0.5d0/hl**3-0.5d0/hr**3+(0.2d1-(n+k)/hl-(n-k)/hr)/(c**2-k**2)
      ss1=ss1+((k*n+c**2)/hl+(k*n-c**2)/hr-0.2d1*k)/(c**2-k**2)-n*0.5d0/hl**3+n*0.5d0/hr**3
   call ieee_get_flag(ieee_invalid,isignal)
   if (isignal) then
       signal=.true.
       print *, 'default case periodic induced velocity problem'
       print *, 'I get:'
       print *, 'ss=',ss
       print *, 'ss1=',ss1
       print *, 'and i found them using'
       print *,'c=',c
       print *,'k=',k
       call ieee_set_flag(ieee_invalid,.false.)
       return
   end if
   end if

  
   if ((c<0.3d-4) .or. (abs(c-k1)<=0.1d-7))  then
      dvv=((vo1%G.x.le)/(0.4d1*pi*ll**2)*ss)
   else
      dvv=(((vo1%G.x.unit(dvv))*c*ss)+((vo1%G.x.le)*ss1))/(0.4d1*pi*ll**2)
  end if

  call ieee_get_flag(ieee_invalid,isignal)
  if (isignal) then
      signal=.true.
      print *, 'Problem at periodic induced velocity calculations'
      print *, 'I get'
      print *, 'vo1%G=',vo1%G
      print *, 'ss=',ss
      print *, 'ss1=',ss1
      print *, 'and i found them using'
      print *, 'c=',c
      print *, 'k=',k
      call ieee_set_flag(ieee_invalid,.false.)
      return
  else
      mp2%v=dvv+mp2%v
  end if
      
end if
end subroutine periodic_ind_vel_vp

subroutine periodic_ind_G_vv(vo1,vo2)
use ieee_exceptions
use periodic_parameters
implicit none
type(pvortex), intent(in)      :: vo1
type(pvortex), intent(inout) :: vo2
type(vector)                          :: dvv
real(kind(0.d0))                     :: k, k1, c , ss, ss1, ss2, ss3, hl, hr, der, der1, der2, der3, derh, epsilon1, epsilon2
integer                                   :: i, j
logical :: isignal
dvv=vo2%mp%p-vo1%mp%p
 c=norm(dvv)/ll
k=dvv*le/ll

call ieee_get_flag(ieee_invalid,isignal)

if (isignal) then
   signal=.true.   
   print *, 'Input error at periodic induced vorticity'
   call ieee_set_flag(ieee_invalid,.false.)
   return

else

 if ((c>4d-3) .and. (c<=3d-1)) then 
      epsilon1=1d-5 
      epsilon2=1d-6 
   else if ((c>3d-1) .and. (c<=75d-2)) then
      epsilon1=5d-5
      epsilon2=1d-6
   else if ((c>75d-2) .and. (c<=1d0)) then
      epsilon1=2d-6
      epsilon2=5d-7
   else
      epsilon1=0.
      epsilon2=0.
   end if

ss=0d0
ss1=0d0
ss2=0d0
ss3=0d0

k1=k
if (k<0.) k1=-k
      
   if (((epsilon1 == 0.).and.(abs(c-k1)>1e-8)) .or. (abs(c-k1)>epsilon1)) then
     if (c<4d-5) then ! c is almost zero calculations
       ss=0.2d1*0.2020569031572758d0
       ss1=0.2d1*0.3692775514336792d-1
       ss2=0d0
       ss3=0.2d1*0.2020569031572758d0
    else ! c is not zero
     do i=ns,n
         ss=ss+sqrt(c**2+2d0*k*i+i**2)**-3+sqrt(c**2-2d0*k*i+i**2)**-3
         ss1=ss1+sqrt(c**2+2d0*k*i+i**2)**-5+sqrt(c**2-2d0*k*i+i**2)**-5
         ss2=ss2+i/sqrt(c**2+2d0*k*i+i**2)**5-i/sqrt(c**2-2d0*k*i+i**2)**5
         ss3=ss3+i**2/sqrt(c**2+2d0*k*i+i**2)**5+i**2/sqrt(c**2-2d0*k*i+i**2)**5
     end do
     if (m/=1) then
        hl=sqrt(c**2+2d0*k*n+n**2)
        hr=sqrt(c**2-2d0*k*n+n**2)
        do i=1,m-1
           der=0d0
           der1=0d0
           der2=0d0
           der3=0d0
           do j=0,i-1
              der=der+M_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(1+4*i-2*j)+(n-k)**(2*i-2*j-1)/hr**(1+4*i-2*j))*2d0**(2*i-j-1)
              der1=der1+N_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(3+4*i-2*j)+(n-k)**(2*i-2*j-1)/hr**(3+4*i-2*j))*2d0**(2*i-j-1)
              der2=der2+N_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(3+4*i-2*j)- (n-k)**(2*i-2*j-1)/hr**(3+4*i-2*j))*2d0**(2*i-j-1)
           end do
           if (i==1) then !second parts calcuation
              der2=(hl**-5)-(hr**-5)+n*der2
              der3=2d0*n*((hl**-5)+(hr**-5))+n**2*der1
           else
              der2=der2*n
              der3=der1*n**2+derh*(2*i-1)*(2*i-2)
              do j=0,i-1
                 der2=der2+(2*i-1)*N_coefs(j,2*i-2)*((k+n)**(2*i-2*j-2)/hl**(4*i-2*j+1)-(n-k)**(2*i-2*j-2)/hr**(4*i-2*j+1))*2d0**(2*i-j-2)
                 der3=der3+2d0*n*(2*i-1)*N_coefs(j,2*i-2)*((k+n)**(2*i-2*j-2)/hl**(4*i-2*j+1)+(n-k)**(2*i-2*j-2)/hr**(4*i-2*j+1))*2d0**(2*i-j-2)
              end do
           end if
           ss=ss-Bernoulli(i)*der/fact(2*i)
           ss1=ss1-Bernoulli(i)*der1/fact(2*i)
           ss2=ss2-Bernoulli(i)*der2/fact(2*i)
           ss3=ss3-Bernoulli(i)*der3/fact(2*i)
           derh=der1
        end do
    end if

    ss=  ss  -5d-1*(hl**-3)-5d-1*(hr**-3)+(2d0-(n+k)/hl-(n-k)/hr)/(c**2-k**2)
    ss1=ss1-5d-1*(hl**-5)-5d-1*(hr**-5)+(4d0-(k+n)*(3d0*c**2-k**2+4d0*k*n+2d0*n**2)/hl**3-(n-k)*(3d0*c**2-k**2-4d0*k*n+2d0*n**2)/hr**3)/(3d0*(c**2-k**2)**2)
    ss2=ss2+n*5d-1/hr**5-n*5d-1/hl**5+((2d0*k*(n+k)*hl**2-k**3*n+c**4-c**2*k**2+c**2*k*n)/hl**3+(2d0*k*(n-k)*hr**2-k**3*n-c**4+c**2*k**2+c**2*k*n)/hr**3-4d0*k)/(3d0*(c**2-k**2)**2)
    ss3=ss3-n**2*5d-1/hl**5-n**2*5d-1/hr**5+(2d0*(c**2+k**2)-(2d0*c**4*k+6d0*c**2*k**2*n+3d0*c**2*k*n**2+c**2*n**3+3d0*k**3*n**2+k**2*n**3)/hl**3    &
                                                                                                   -(-2d0*c**4*k+6d0*c**2*k**2*n  -3d0*c**2*k*n**2+c**2*n**3 -3d0*k**3*n**2+k**2*n**3)/hr**3)/(3d0*(c**2-k**2)**2)
    end if
    if ((c>=4d-5) .and. (c<4d-4)) then ! c is small enough for specific sums to attain certain values
       ss1=0.2d1*3.692775514336792d-1
       ss2=0d0
       ss3=0.2d1*0.2020569031572758d0
    else if ((c>=4d-4 .and. (c<3d-3))) then
       ss1=0.2d1*0.3692775514336792d-1
       ss2=0d0  
    else if ((c>=3d-3) .and. (c<4d-3)) then
      ss1=0.2d1*0.3692775514336792d-1
    end if

   call ieee_get_flag(ieee_invalid,isignal)
   if (isignal) then
      signal=.true.
      print *, 'Problem at vorticity evolution eq default case'
      print *, 'I got'
      print *, 'ss=', ss
      print *, 'ss1=',ss1
      print *, 'ss2=',ss2
      print *, 'ss3=',ss3
      print *, 'with'
      print *, 'c=',c
      print *, 'k=',k
   call ieee_set_flag(ieee_invalid,.false.)
   return
   end if   

   else if ((abs(c-k1)<=epsilon1) .and. (abs(c-k1)>epsilon2)) then
  !   print *, 'Almost parallel particles case 1'

     do i=ns,n
        ss=ss+sqrt(c**2+0.2d1*k*i+i**2)**-3+sqrt(c**2-0.2d1*k*i+i**2)**-3
        ss1=ss1+(i+k)**-5+(i-k)**-5
        ss2=ss2+i/(i+k)**5-i/(i-k)**5
        ss3=ss3+i**2/sqrt(c**2+0.2d1*k*i+i**2)**5+i**2/sqrt(c**2-0.2d1*k*i+i**2)**5
     end do
     if (m/=1) then
        hl=sqrt(c**2+2d0*k*n+n**2)
        hr=sqrt(c**2-2d0*k*n+n**2)
        do i=1,m-1
           der=0d0
           der1=0d0
           der2=0d0
           der3=0d0
           do j=0,i-1
              der=der+M_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(1+4*i-2*j)+(n-k)**(2*i-2*j-1)/hr**(1+4*i-2*j))*0.2d1**(2*i-j-1)
              der1=der1+N_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(3+4*i-2*j)+(n-k)**(2*i-2*j-1)/hr**(3+4*i-2*j))*0.2d1**(2*i-j-1)
           end do
           if (i==1) then !second parts calcuation
              der3=0.2d1*n*((hl**-5)+(hr**-5))+n**2*der1
           else
              der3=der1*n**2+derh*(2*i-1)*(2*i-2)
              do j=0,i-1
                 der3=der3+0.2d1*n*(0.2d1*i-0.1d1)*N_coefs(j,2*i-2)*((k+n)**(2*i-2*j-2)/hl**(4*i-2*j+1)+(n-k)**(2*i-2*j-2)/hr**(4*i-2*j+1))*0.2d1**(2*i-j-2)
              end do
           end if
           ss=ss-Bernoulli(i)*der/fact(2*i)
           ss1=ss1+Bernoulli(i)/0.24d2*(0.2d1*i+0.1d1)*(0.2d1*i+0.2d1)*(0.2d1*i+0.3d1)*((n+k)**-(2*i+4)+(n-k)**-(2*i+4))
           ss2=ss2-Bernoulli(i)/0.24d2*(0.2d1*i+0.1d1)*(0.2d1*i+0.2d1)*((0.2d1*i-0.1d1)*((n+k)**-(2*i+3)-(n-k)**-(2*i+3))-n*(0.2d1*i+0.3d1)*((n+k)**-(2*i+4)-(n-k)**-(2*i+4)))
           ss3=ss3-Bernoulli(i)*der3/fact(2*i)
           derh=der1
        end do
     end if
  
     ss=  ss  -0.5d0*hl**-3-0.5d0*hr**-3+(0.2d1-(n+k)/hl-(n-k)/hr)/(c**2-k**2)
     ss1=ss1-0.5d0/(n+k)**5-0.5d0/(n-k)**5+0.25d0/(k+n)**4+0.25d0/(n-k)**4
     ss2=ss2-n*0.5d0/(n+k)**5+n*0.5d0/(n-k)**5+(k+4*n)/0.12d2/(k+n)**4-(4*n-k)/0.12d2/(n-k)**4
     ss3=ss3-n**2*0.5d0/hl**5-n**2*0.5d0/hr**5+(0.2d1*(c**2+k**2)-(0.2d1*c**4*k+0.6d1*c**2*k**2*n+0.3d1*c**2*k*n**2+c**2*n**3+0.3d1*k**3*n**2+k**2*n**3)/hl**3    &
                                                                                                     -(-0.2d1*c**4*k+0.6d1*c**2*k**2*n-0.3d1*c**2*k*n**2+c**2*n**3 -0.3d1*k**3*n**2+k**2*n**3)/hr**3)/(0.3d1*(c**2-k**2)**2)
  call ieee_get_flag(ieee_invalid,isignal)
  if (isignal) then
     signal=.true.
     print *, 'Problem at vorticity evolution eq almost parallel case 1'
     print *, 'I got'
     print *, 'ss=', ss
     print *, 'ss1=',ss1
     print *, 'ss2=',ss2
     print *, 'ss3=',ss3
     print *, 'with'
     print *, 'c=',c
     print *, 'k=',k
   call ieee_set_flag(ieee_invalid,.false.)
   return
   end if   


   else if ((abs(c-k1)<=epsilon2) .and. (abs(c-k1)>1d-8)) then
  !   print *, 'Almost parallel particles case 2'
    do i=ns,n
        ss=ss+sqrt(c**2+0.2d1*k*i+i**2)**-3+sqrt(c**2-0.2d1*k*i+i**2)**-3
        ss1=ss1+(i+k)**-5+(i-k)**-5
        ss2=ss2+i/(i+k)**5-i/(i-k)**5
        ss3=ss3+i**2/(i+k)**5+i**2/(i-k)**5
     end do
     if (m/=1) then
        hl=sqrt(c**2+0.2d1*k*n+n**2)
        hr=sqrt(c**2-0.2d1*k*n+n**2)
        do i=1,m-1
           der=0d0
           do j=0,i-1
              der=der+M_coefs(j,2*i-1)*((k+n)**(2*i-2*j-1)/hl**(1+4*i-2*j)+(n-k)**(2*i-2*j-1)/hr**(1+4*i-2*j))*0.2d1**(2*i-j-1)
           end do
           ss=ss-Bernoulli(i)*der/fact(2*i)
           ss1=ss1+Bernoulli(i)/0.24d2*(0.2d1*i+0.1d1)*(0.2d1*i+0.2d1)*(0.2d1*i+0.3d1)*((n+k)**-(2*i+4)+(n-k)**-(2*i+4))
           ss2=ss2-Bernoulli(i)/0.24d2*(0.2d1*i+0.1d1)*(0.2d1*i+0.2d1)*((0.2d1*i-0.1d1)*((n+k)**-(2*i+3)-(n-k)**-(2*i+3))-n*(0.2d1*i+0.3d1)*((n+k)**-(2*i+4)-(n-k)**-(2*i+4)))
           ss3=ss3+Bernoulli(i)/0.24d2*(0.2d1*i+0.1d1)*  &
          ((0.2d1*i-0.1d1)*(0.2d1*i-0.2d1)*((n+k)**-(2*i+2)+(n-k)**-(2*i+2))-0.2d1*(0.2d1*i-0.1d1)*(0.2d1*i+0.2d1)*n*((n+k)**-(2*i+3)+  &
             (n-k)**-(2*i+3))+n**2*(0.2d1*i+0.2d1)*(0.2d1*i+0.3d1)*((n+k)**-(2*i+4)+(n-k)**-(2*i+4)))
        end do
     end if
  
     ss=  ss  -0.5d0*(hl**-3)-0.5d0*(hr**-3)+(0.2d1-(n+k)/hl-(n-k)/hr)/(c**2-k**2)
     ss1=ss1-0.5d0/(n+k)**5-0.5d0/(n-k)**5+0.25d0/(k+n)**4+0.25d0/(n-k)**4
     ss2=ss2-n*0.5d0/(n+k)**5+n*0.5d0/(n-k)**5+(k+4*n)/0.12d2/(k+n)**4-(4*n-k)/0.12d2/(n-k)**4
     ss3=ss3-n**2*0.5d0/(n+k)**5-n**2*0.5d0/(n-k)**5+0.1d1/k**2/0.6d1-n**3*(0.4d1*k+n)/0.12d2/k**2/(n+k)**4-n**3*(n-0.4d1*k)/0.12d2/k**2/(n-k)**4

  call ieee_get_flag(ieee_invalid,isignal)
   if (isignal) then
      signal=.true.
      print *, 'Problem at vorticity evolution eq almost paraller case 2'
      print *, 'I got'
      print *, 'ss=', ss
      print *, 'ss1=',ss1
      print *, 'ss2=',ss2
      print *, 'ss3=',ss3
      print *, 'with'
      print *, 'c=',c
      print *, 'k=',k
   call ieee_set_flag(ieee_invalid,.false.)
   return
   end if

   else if (abs(c-k1)<=1d-8) then

     ss=0d0
     do i=ns,n 
         ss=ss+0.2d1*i*(0.3d1*k**2+i**2)/(i**2-k**2)**3
      end do
      if (m/=1) then
         do i=1,m-1
            ss=ss+(0.2d1*i+0.1d1)*Bernoulli(i)/0.2d1*((k+n)**-(2*i+2)+(n-k)**-(2*i+2))
         end do
      end if

      ss=ss+(k**2+n**2)/(n**2-k**2)**2-n*(0.3d1*k**2+n**2)/(n**2-k**2)**3
 
  call ieee_get_flag(ieee_invalid,isignal)
  if (isignal) then
    signal=.true.
    print *, 'Problem at vorticity evolution eq parallel case'
    print *, 'I got'
    print *, 'ss=', ss
    print *, 'with'
    print *, 'c=',c
    print *, 'k=',k
    call ieee_set_flag(ieee_invalid,.false.)
    return
  end if 
    
end if

if (abs(c-k1)<=1d-8) then
   dvv=(((vo1%G.x.vo2%G)-(0.3d1*(vo1%G.x.le)*(vo2%G*le)))/(0.4d1*pi*ll**3)*ss)
else
   dvv=(((vo1%G.x.vo2%G)*ss)   &
         -(0.3d1*(((vo1%G.x.unit(dvv))*(unit(dvv)*vo2%G)*c**2*ss1)+(((vo1%G.x.unit(dvv))*(le*vo2%G)+(vo1%G.x.le)*(unit(dvv)*vo2%G))*c*ss2)+((vo1%G.x.le)*(le*vo2%G)*ss3))))/(0.4d1*pi*ll**3)
end if

call ieee_get_flag(ieee_invalid,isignal)
if (isignal) then
   signal=.true.
   print *, 'Problem at vorticity evolution eq final calculations'
   print *, 'I got'
   print *, 'ss=', ss
   print *, 'ss1=',ss1
   print *, 'ss2=',ss2
   print *, 'ss3=',ss3
   print *, 'with'
   print *, 'c=',c
   print *, 'k=',k
   print *, 'unit(dvv)=',unit(dvv)
   call ieee_set_flag(ieee_invalid,.false.)
   return
else
   vo2%dG=dvv+vo2%dG
end if

end if

vo2%mp=vo2%mp
vo2%G=vo2%G
end subroutine periodic_ind_G_vv

end module sing_elem

module linspaces

implicit none

contains

function midlinspace(a,b,n) result(ls)
real(kind(0.d0)), intent(in) :: a, b
integer , intent(in) :: n
real(kind(0.d0)), dimension(n) :: ls
integer :: i
do i=1,n
 ls(i)=a+(b-a)/(0.2d1*n)*(0.2d1*i-0.1d1)
end do
end function midlinspace

function linspace0(a,b,n) result(ls)
real(kind(0.d0))      , intent(in) :: a, b
integer , intent(in) :: n
real(kind(0.d0)), dimension(n) :: ls
integer :: i
do i=1,n
    ls(i)=a+(i-1)*(b-a)/n
end do
end function linspace0

function linspace(a,b,n) result(ls)
real(kind(0.d0))      , intent(in) :: a, b
integer , intent(in) :: n
real(kind(0.d0)), dimension(n) :: ls
integer :: i
do i=1,n
    ls(i)=a+(i-1)*(b-a)/(n-1)
end do
end function linspace

end module linspaces

module sin_length_n_stuff

use implied_defs
use linspaces

implicit none
real(kind(0.d0)) :: A_amp, wl_ll
integer, parameter :: m_quad=100
real(kind(0.d0)), parameter :: Ntn_acc=0.1d-2

contains

real(kind(0.d0)) function norm_sin_tang(x) result(g)
real(kind(0.d0)), intent(in) :: x
g=sqrt(0.1d1+A_amp**2*0.4d1*pi**2/wl_ll**2*cos(0.2d1*pi*x/wl_ll)**2)
end function norm_sin_tang

real(kind(0.d0)) function sin_length(x1,x2) result(lngth)
real(kind(0.d0)), intent(in) :: x1, x2
real(kind(0.d0)), dimension(m_quad+1):: x
integer :: i
x=linspace(x1,x2,m_quad+1)
lngth=norm_sin_tang(x1)+norm_sin_tang(x2)
do i=2,m_quad-1,2
   lngth=lngth+0.4d1*norm_sin_tang(x(i))+0.2d1*norm_sin_tang(x(i+1))
end do
lngth=(x2-x1)*(lngth+0.4d1*norm_sin_tang(x(m_quad)))/0.3d1/m_quad
end function sin_length

recursive subroutine sin_lngth_findx(x1,l,x2)
real(kind(0.d0)), intent(in) :: x1, l
real(kind(0.d0)) :: l0
real(kind(0.d0)) :: x2
l0=sin_length(x1,x2)
if (abs(l-l0)<Ntn_acc*norm_sin_tang(x2)) return
x2=x2+(l-l0)/norm_sin_tang(x2)
call sin_lngth_findx(x1,l,x2)
end subroutine sin_lngth_findx

end module sin_length_n_stuff

module Hermitian_n_stuff

use space3d
use linspaces

implicit none
type(point), dimension(:),allocatable :: point_set
type(vector), dimension(:),allocatable :: vector_set
real(kind(0.d0)), dimension(:),allocatable :: u_set
integer, parameter :: m_par=200
real(kind(0.d0)), parameter :: acc_Ntn=0.1d-4
logical :: first=.true.
real(kind(0.d0)) :: um,up, unext

contains

type(point) elemental function Hermitian(u) result(Hp)
real(kind(0.d0)), intent(in) :: u
integer :: j
real(kind(0.d0)) :: du, bdu
do j=1,size(point_set)-1
   if  (u>u_set(j) .and. u<u_set(j+1)) then
      du=u_set(j+1)-u_set(j)
      bdu=u-u_set(j)
      Hp=(bdu**3*0.2d1/du**3-bdu**2*0.3d1/du**2+0.1d1)*point_set(j)+(-bdu**3*0.2d1/du**3+bdu**2*0.3d1/du**2)*point_set(j+1) &
       +(bdu**3/du**2-bdu**2*0.2d1/du+bdu)*vector_set(j)+(bdu**3/du**2-bdu**2/du)*vector_set(j) 
      exit
   else if  (u == u_set(j)) then 
      Hp=point_set(j)
      exit
   else if (u == u_set(j+1)) then
      Hp=point_set(j+1)
      exit
   end if
end do
end function Hermitian

real(kind(0.d0)) elemental function norm_Herm_velo(u) result(nHv)
real(kind(0.d0)), intent(in) :: u
integer :: j,i
real :: du, bdu, fdu
do j=1,size(point_set)-1
   if  (u>=u_set(j) .and. u<u_set(j+1)) then
      du=u_set(j+1)-u_set(j)
      bdu=u-u_set(j)
      fdu=u_set(j+1)-u
      i=j
      exit
   end if
end do
nHv=sqrt(0.36d2*bdu**2*fdu**2/du**2*norm2(point_set(i+1)-point_set(i))  &
    +0.12d2*bdu*fdu/du*((point_set(i+1)-point_set(i))*(fdu*(du-0.3d1*bdu)*vector_set(i)-bdu*(0.2d1*du-0.3d1*bdu)*vector_set(i+1))) &
    +fdu**2*(du-0.3d1*bdu)**2*norm2(vector_set(i))-0.2d1*bdu*fdu*(du-0.3d1*bdu)*(0.2d1*du-0.3d1*bdu)*(vector_set(i)*vector_set(i+1))+bdu**2*(0.2d1*du-0.3d1*bdu)**2*norm2(vector_set(i+1)))/du**2
end function norm_Herm_velo

type(vector) elemental function tanHermitian(u) result(dHp)
real(kind(0.d0)), intent(in) :: u
integer :: j
real :: du, bdu
do j=1,size(point_set)-1
   if  (u>u_set(j) .and. u<u_set(j+1)) then
      du=u_set(j+1)-u_set(j)
      bdu=u-u_set(j)
      dHp=(((bdu**2*0.6d1/du**3-bdu*0.6d1/du**2)*point_set(j)-(bdu**2*0.6d1/du**3-bdu*0.6d1/du**2)*point_set(j+1)) &
       +(bdu**2*0.3d1/du**2-bdu*0.4d1/du+0.1d1)*vector_set(j)+(bdu**2*0.3d1/du**2-bdu*0.2d1/du)*vector_set(j+1)) 
      !dHp=dHp/norm(dhp)
      exit
   else if  (u == u_set(j)) then 
      dhp=vector_set(j)!/norm(vector_set(j))
      exit
   else if (u == u_set(j+1)) then
      dhp=vector_set(j+1)!/norm(vector_set(j+1))
      exit
   end if
end do

end function tanHermitian

real(kind(0.d0)) function Hermitian_length(u1,u2,m) result(lngth)
real(kind(0.d0)), intent(in) :: u1, u2
integer,intent(in) :: m
real(kind(0.d0)), dimension(m+1) :: uu
integer :: i
uu=linspace(u1,u2,m+1)
lngth=norm(tanHermitian(u1))+norm(tanHermitian(u2))
do i=2,m-1,2
 !  lngth=lngth+0.4d1*norm_Herm_velo(uu(i))+0.2d1*norm_Herm_velo(uu(i+1))
  lngth=lngth+0.4d1*norm(tanHermitian(uu(i)))+0.2d1*norm(tanHermitian(uu(i+1)))
end do
!lngth=(u2-u1)*(lngth+0.4d1*norm_Herm_velo(uu(m_par)))/0.3d1/m_par
lngth=(u2-u1)*(lngth+0.4d1*norm(tanHermitian(uu(m))))/0.3d1/m
end function Hermitian_length

real(kind(0.d0)) recursive function adpt_Hermitian_length(u1,u2,m) result(adptlngth)
real(kind(0.d0)), intent(in) :: u1, u2
integer, intent(in) :: m
real(kind(0.d0)) :: adptlngth1
adptlngth1=Hermitian_length(u1,u2,m)
adptlngth=Hermitian_length(u1,u2,2*m)
if (abs(adptlngth-adptlngth1) < acc_Ntn) then
return
else 
adptlngth=adpt_Hermitian_length(u1,u2,2*m)
end if
end function adpt_Hermitian_length

recursive subroutine Herm_find_u(u1,l,u2)
real(kind(0.d0)), intent(in) :: u1,l
real(kind(0.d0)), intent(out) :: u2
real(kind(0.d0)) :: l0
integer :: i
if (first) then
   do i=2,size(u_set)
       if (u_set(i)>u1) then
             l0=adpt_Hermitian_length(u1,u_set(i),m_par)
             if (l<l0) then 
                 up=u_set(i)
                 exit
             else if (abs(l0-l)<1e-06) then
                 u2=u_set(i)
                 return 
             end if
       end if
   end do
um=u1
first=.false.
end if
unext=(um+up)/2d0
l0=adpt_Hermitian_length(u1,unext,m_par)     
if (abs(l-l0)<1d-6) then
   u2=unext
   first=.true.
   um=0d0
   up=0d0
else if ((l-l0)>0d0) then
  um=unext
  call Herm_find_u(u1,l,u2)
else if ((l-l0)<0d0) then
  up=unext
  call Herm_find_u(u1,l,u2)
end if
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!if (abs(l-l0)<acc_Ntn*norm_Herm_velo(u2)) return
!u2=u2+(l-l0)/norm_Herm_velo(u2)
!if (norm(tanHermitian(u2))<1d-6) print *,'care:: not a good line'
!if (abs(l-l0)<acc_Ntn) then
!u2=u2
!else
!u2=u2+(l-l0)/norm(tanHermitian(u2))
!if  ( u2>=u_set(size(u_set))) then
!print *, 'opa!!!!!!!!!!!!!!!!!!!!!1'
!u2=u2
!else
!!if  ( u2<=u_set(1) print *, 'opa!!!!!!!!!!!!!!!!!!!!!1'
!call Herm_find_u(u1,l,u2)
!end if
end subroutine Herm_find_u

recursive subroutine printHerm_find_u(u1,l,u2)
real(kind(0.d0)), intent(in) :: u1,l
real(kind(0.d0)), intent(inout) :: u2
real(kind(0.d0)) :: l0
l0=adpt_Hermitian_length(u1,u2,m_par)
print *, l
print *, l0
print *, 'ok'
!if (abs(l-l0)<acc_Ntn*norm_Herm_velo(u2)) return
!u2=u2+(l-l0)/norm_Herm_velo(u2)
print *, norm(tanHermitian(u2))
if (abs(l-l0)<acc_Ntn*norm(tanHermitian(u2))) then
print *, 'ok'
u2=u2
print *, 'ok'
else
print *, 'ok'
u2=u2+(l-l0)/norm(tanHermitian(u2))
print *, norm(tanHermitian(u2))
print *, u2
call Herm_find_u(u1,l,u2)
end if
print *, 'finished'
end subroutine printHerm_find_u

end module Hermitian_n_stuff

module vp_init

use space3d
use implied_defs
use sing_elem
use linspaces

implicit none

contains

elemental real(kind(0.d0)) function elliptic(s) result(a)
real(kind(0.d0)), intent(in) :: s
a=(1.-2*s)/sqrt(s-s**2)
end function elliptic

function eq_area_circ_dstr(r0,en,er,R,nq) result(p_circ)
type(point), intent(in) :: r0
type(vector), intent(in) :: en, er
real(kind(0.d0)), intent(in) :: R
integer, dimension(:), intent(in) :: nq
type(mpoint), dimension(sum(nq)) :: p_circ
real(kind(0.d0)),dimension(size(nq)) :: rrr,rrr1
real(kind(0.d0)) ::sumh, ntot
integer :: cs, i, j, npart
 cs=size(nq)
ntot=sum(nq)
sumh=0.d0
do i=1,cs
sumh=sumh+nq(i)
rrr(i)=R/sqrt(ntot)*sqrt(sumh)
end do
rrr1(1)=2d0/3d0*rrr(1)*nq(1)*sin(pi/nq(1))/pi
rrr1(2:cs)=2d0/3d0*(rrr(2:cs)**2+rrr(2:cs)*rrr(1:cs-1)+rrr(1:cs-1)**2)/(rrr(2:cs)+rrr(1:cs-1))*nq(2:cs)*sin(pi/nq(2:cs))/pi
npart=0
rrr(1:cs:2)=0d0
rrr(2:cs:2)=-2d0*pi/nq(2:cs:2)
npart=0
do i=1,cs
   do j=1,nq(i)
      p_circ(j+npart)%p=r0+(rrr1(i)*cos(2d0*pi/nq(i)*(j-1)+rrr(i))*unit(er))+(rrr1(i)*sin(2d0*pi/nq(i)*(j-1)+rrr(i))*unit(er.x.en))
   end do
   npart=npart+nq(i)
end do
p_circ%v=zero_v
end function eq_area_circ_dstr

function vplinefun(r0,r1,n,vecG,GG) result(vpl)
type(point)      , intent(in)             :: r0, r1
integer            , intent(in)              :: n
type(vector)   , intent(in)               :: vecG
type(pvortex), dimension(n)          :: vpl
integer                                            :: i
real(kind(0.d0))                , dimension(2*n+1) :: sl
interface
  elemental real(kind(0.d0)) function GG(s) result(f)
  real(kind(0.d0)), intent(in) :: s
  end function GG
end interface
sl=midlinspace(0d0,1d0,n)
vpl(1:n)%mp%p=r0+(r1-r0)*sl(1:n)
vpl(1:n)%G=vecG*(norm(r1-r0)/n)*GG(sl(1:n))
end function vplinefun

function vpcircle(c,r,vec1,vec2,n,GG) result(vp_cir)
type(point),           intent(in) :: c
real(kind(0.d0)),                       intent(in) :: r
type(vector),         intent(in) :: vec1, vec2
integer,                  intent(in) :: n
real(kind(0.d0)),                       intent(in) :: GG
type(pvortex), dimension(n) :: vp_cir
type(vector)                          :: vx, vy
real(kind(0.d0))                , dimension(n) :: fi
if (vec1*vec2 /= 0.) then
   vx=(vec1*vec2)*unit(vec2)
   vy=vec1-vx
   vx=unit(vx)
   vy=unit(vy)
else
   vx=unit(vec2)
   vy=unit(vec1)
end if
fi=linspace0(0d0,2d0*pi,n)
vp_cir(1:n)%mp%p=c+(r*cos(fi(1:n)))*vx+(r*sin(fi(1:n)))*vy
vp_cir(1:n)%G=(((-1d0)*r*sin(fi(1:n)))*vx+(r*cos(fi(1:n)))*vy)*(GG/n)
vp_cir(1:n)%mp%v=zero_v
end function vpcircle

function per_vp_sin(r0,lel,ne,rr,lll,n,GG) result(pvpsin)
type(point)           , intent(in) :: r0
type(vector)         , intent(in) :: lel, ne
real(kind(0.d0))                      , intent(in) :: rr, lll, GG
integer                 , intent(in) :: n
type(pvortex), dimension(n) :: pvpsin
real(kind(0.d0))                , dimension(n) :: x
x=midlinspace(0d0,lll,n)
pvpsin%mp%p=r0+x*lel+sin(2d0*pi*x/lll)*ne*rr
pvpsin%G=GG*lll/n*(lel+2.*pi*ne*rr/lll*cos(2d0*pi*x/lll))/sqrt(0.1d1+0.4d1*pi**2/lll**2*rr**2*(cos(0.2d1*pi*x/lll))**2)
!pvpsin%G=GG*(lel.x.ne)
pvpsin%mp%v=zero_v
pvpsin%dG=zero_v
end function per_vp_sin

function per_vp_helix(r0,lel,ne,lll,rr,n,GG) result(pvpsin)
type(point)           , intent(in) :: r0
type(vector)         , intent(in) :: lel, ne
real(kind(0.d0))                      , intent(in) :: lll, GG, rr
integer                 , intent(in) :: n
type(pvortex), dimension(n) :: pvpsin
real(kind(0.d0))                , dimension(n) :: x
x=midlinspace(0d0,lll,n)
pvpsin%mp%p=r0+x*lel+rr*cos(2d0*pi*x/lll)*ne+rr*sin(2d0*pi*x/lll)*(lel.x.ne)
pvpsin%G=GG/n*(lel+2.*pi*rr/lll*((-1d0)*ne*sin(2d0*pi*x/lll)+(lel.x.ne)*cos(2d0*pi*x/lll)))/sqrt(1d0+4d0*(pi*rr)**2/lll**2)
pvpsin%mp%v=zero_v
pvpsin%dG=zero_v
end function per_vp_helix

function simple_vpline(r0,lel,lll,n,GG,ovrlp) result(vpl)
type(point), intent(in) :: r0
type(vector), intent(in):: lel
real(kind(0.d0)), intent(in) :: lll, GG, ovrlp
integer :: n
type(pvortex), dimension(n) :: vpl
vpl(1:n)%mp%p=r0+midlinspace(0d0,lll,n)*lel
vpl(1:n)%e_mol=ovrlp/9.136553695187546d-01*lll/n
vpl(1:n)%mp%v=zero_v
vpl(1:n)%G=GG*lll/n*lel
vpl(1:n)%dG=zero_v
vpl(1:n)%u=midlinspace(0d0,lll,n)
vpl(1:n)%pmove=0d0
end function simple_vpline

function set_sin_vp(r0,lel,ne,amp,lll,nv,GG,ovrlp) result(vp)
use sin_length_n_stuff
type(point), intent(in) :: r0
type(vector), intent(in) :: lel,ne
real(kind(0.d0)), intent(in) :: lll, amp, GG, ovrlp
integer, intent(in) :: nv
type(pvortex), dimension(nv):: vp
real(kind(0.d0)) :: totL
integer :: i
real(kind(0.d0)), dimension(nv+1) :: x
A_amp=amp
wl_ll=lll
totL=sin_length(0d0,lll)
print *, totL
vp%u=midlinspace(0d0,totL,nv)
x=linspace(0d0,lll,nv+1)
call sin_lngth_findx(x(1),totL/(2d0*nv),x(2))
do i=3,nv+1
   call sin_lngth_findx(x(i-1),totL/nv,x(i))
end do
vp(1:nv)%mp%p=r0+x(2:nv+1)*lel+sin(2d0*pi*x(2:nv+1)/lll)*ne*amp
vp(1:nv)%G=GG*totL/nv*(lel+2.d0*pi*ne*amp/lll*cos(2d0*pi*x(2:nv+1)/lll))/sqrt(0.1d1+0.4d1*pi**2/lll**2*amp**2*(cos(0.2d1*pi*x(2:nv+1)/lll))**2)
!pvpsin%G=GG*(lel.x.ne)
vp%mp%v=zero_v
vp%dG=zero_v
vp%e_mol=ovrlp/9.136553695187546d-01*totL/nv
vp%pmove=0d0
end function set_sin_vp

function vp_cyl_rankine(r0,lel,lll,nslices,R,er,nq,GG,ovrlp) result(vpcyl)
type(point), intent(in) :: r0
type(vector),intent(in) :: lel, er
real(kind(0.d0)), intent(in) :: lll, R, GG,ovrlp
integer, intent(in) :: nslices
integer, dimension(:), intent(in) :: nq
type(pvortex), dimension(nslices*sum(nq)) :: vpcyl
type(mpoint), dimension(sum(nq)) :: phelp
integer :: i, npart
phelp=eq_area_circ_dstr(r0,lel,er,R,nq)
npart=0
do i=1,sum(nq)
vpcyl(1+npart:nslices+npart)=simple_vpline(phelp(i)%p,lel,lll,nslices,GG/sum(nq),ovrlp)
npart=npart+nslices
end do
end function vp_cyl_rankine

function vp_cyl_lamb(r0,lel,lll,nslices,R,er,nq,GG,ovrlp) result(vpcyl)
type(point), intent(in) :: r0
type(vector),intent(in) :: lel, er
real(kind(0.d0)), intent(in) :: lll, R, GG,ovrlp
integer, intent(in) :: nslices
integer,dimension(:), intent(in) :: nq
type(pvortex), dimension(nslices*sum(nq)) :: vpcyl
type(mpoint), dimension(sum(nq)) :: phelp
integer :: i, npart
real(kind(0.d0)) :: sumh, totn
real(kind(0.d0)), dimension(size(nq)) :: rrr
real(kind(0.d0)), dimension(sum(nq)) :: ggg
sumh=0.d0
totn=sum(nq)
do i=1,size(nq)
sumh=sumh+nq(i)
rrr(i)=R/sqrt(totn)*sqrt(sumh)
end do
ggg(1:nq(1))=GG/nq(1)*(1d0-exp(-rrr(1)**2/R**2))
npart=nq(1)
do i=2,size(nq)
ggg(npart+1:npart+nq(i))=GG/nq(i)*(exp(-rrr(i-1)**2/R**2)-exp(-rrr(i)**2/R**2))
npart=npart+nq(i)
end do
phelp=eq_area_circ_dstr(r0,lel,er,R,nq)
vpcyl(1:nslices)=simple_vpline(phelp(1)%p,lel,lll,nslices,ggg(1),ovrlp)
print *,1d0-exp(-rrr(1)**2/R**2)
npart=nslices
do i=2,sum(nq)
vpcyl(1+npart:nslices+npart)=simple_vpline(phelp(i)%p,lel,lll,nslices,ggg(i),ovrlp)
npart=npart+nslices
end do
end function vp_cyl_lamb


function vp_cylinder(r0,lel,lll,R,nr,nth,nz,circ) result(pvpcyl)
type(point)    , intent(in)                         :: r0
type(vector)  , intent(in)                         :: lel
real(kind(0.d0))                , intent(in)                         :: lll, R, circ
integer           , intent(in)                         :: nr, nth, nz
type(pvortex), dimension(2*nr*nth*nz) :: pvpcyl
integer                                                     :: i, j, k
forall(i=1:nth, j=1:nr:2, k=1:nz)
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2.*nz)*(k-0.5))+(kk*(R/nr)*(j-0.5)*cos(2*pi/nth*(i-0.5)))+(jj*(R/nr)*(j-0.5)*sin(2*pi/nth*(i-0.5)))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2d0*nz)*(k-5d-1))+(kk*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*cos(2d0*pi/nth*(i-5d-1)))+(jj*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*sin(2d0*pi/nth*(i-5d-1)))+5d-2*kk*sin(pi*lll/nz*(k-5d-1))
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2.*nz)*(k-0.5))+(kk*sqrt((j-0.5)/nr)*cos(2*pi/nth*(i-0.5)))+(jj*sqrt((j-0.5)/nr)*sin(2*pi/nth*(i-0.5)))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%G=lel*circ*lll/(2d0*nr**2*nth*nz)*(2d0*j-1d0)
end forall
forall(i=1:nth, j=1:nr:2, k=nz+1:2*nz)
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(-lll)/(2.*nz)*(k-nz-0.5)*lel+(kk*(R/nr)*(j-0.5)*cos(2*pi/nth*(i-0.5)))+(jj*(R/nr)*(j-0.5)*sin(2*pi/nth*(i-0.5)))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+((-lll)/(2d0*nz)*(k-nz-5d-1)*lel)+(kk*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*cos(2d0*pi/nth*(i-5d-1)))+(jj*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*sin(2d0*pi/nth*(i-5d-1)))+5d-2*kk*sin(-pi*lll/nz*(k-nz-5d-1))
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2.*nz)*(k-0.5))+(kk*sqrt((j-0.5)/nr)*cos(2*pi/nth*(i-0.5)))+(jj*sqrt((j-0.5)/nr)*sin(2*pi/nth*(i-0.5)))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%G=lel*circ*lll/(2d0*nr**2*nth*nz)*(2d0*j-1d0)
end forall
forall(i=1:nth, j=2:nr:2, k=1:nz)
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2.*nz)*(k-0.5))+(kk*(R/nr)*(j-0.5)*cos(2*pi/nth*(i-0.5)))+(jj*(R/nr)*(j-0.5)*sin(2*pi/nth*(i-0.5)))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2d0*nz)*(k-5d-1))+(kk*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*cos(2d0*pi/nth*i))+(jj*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*sin(2d0*pi/nth*i))+5d-2*kk*sin(pi*lll/nz*(k-0.5))
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2.*nz)*(k-0.5))+(kk*sqrt((j-0.5)/nr)*cos(2*pi/nth*i))+(jj*sqrt((j-0.5)/nr)*sin(2*pi/nth*i))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%G=lel*circ*lll/(2d0*nr**2*nth*nz)*(2d0*j-1d0)
end forall
forall(i=1:nth, j=2:nr:2, k=nz+1:2*nz)
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(-lll)/(2.*nz)*(k-nz-0.5)*lel+(kk*(R/nr)*(j-0.5)*cos(2*pi/nth*(i-0.5)))+(jj*(R/nr)*(j-0.5)*sin(2*pi/nth*(i-0.5)))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+((-lll)/(2d0*nz)*(k-nz-5d-1)*lel)+(kk*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*cos(2d0*pi/nth*i))+(jj*(R/nr)/sqrt(2d0)*sqrt(2d0*j**2-2d0*j+1d0)*sin(2d0*pi/nth*i))+5d-2*kk*sin(-pi*lll/nz*(k-nz-5d-1))
!pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%mp%p=r0+(lel*lll/(2.*nz)*(k-0.5))+(kk*sqrt((j-0.5)/nr)*cos(2*pi/nth*i))+(jj*sqrt((j-0.5)/nr)*sin(2*pi/nth*i))
pvpcyl((i-1)*nr+j+(k-1)*nr*nth)%G=lel*circ*lll/(2d0*nr**2*nth*nz)*(2d0*j-1d0)
end forall
!pvpcyl%G=lel*circ*lll/(nr*nth*2.*nz)
pvpcyl(1:2*nr*nth*nz)%mp%v=zero_v
pvpcyl(1:2*nr*nth*nz)%dG=zero_v
end function vp_cylinder

function cyl_sin_pert(GG,lll,R,ra,ny,nz) result(pvpcyc)
real(kind(0.d0)), intent(in) :: GG, lll, R, ra
integer, intent(in) :: ny, nz
type(pvortex), dimension(2*nz*ny*(ny+1)) :: pvpcyc
real(kind(0.d0)), dimension(ny,ny) :: uu
integer :: i, j, k

uu=0.

forall(i=1:ny,j=1:ny,j<=i-1)  uu(i,j)=i**2*atan2(dble(j),sqrt(dble(i**2-j**2)))+j*sqrt(dble(i**2-j**2)) -i**2*atan2(dble(j-1),sqrt(dble(i**2-(j-1)**2)))-(j-1)*sqrt(dble(i**2-(j-1)**2))-(i-1)**2*atan2(dble(j),sqrt(dble((i-1)**2-j**2)))  &
   -j*sqrt(dble((i-1)**2-j**2))+(i-1)**2*atan2(dble(j-1),sqrt(dble((i-1)**2-(j-1)**2)))+(j-1)*sqrt(dble((i-1)**2-(j-1)**2))

do i=1,ny
uu(i,i)=i**2*pi/2d0-i**2*atan2(dble(i-1),sqrt(dble(2*i-1)))-(i-1)*sqrt(dble(2*i-1))
end do

forall(i=1:ny,j=1:ny,k=1:nz,j<=i-1)
pvpcyc((i*(i-1))/2+j+(k-1)*(ny*(ny+1))/2)%mp%p%x=ra*nz/(2d0*pi)*(cos(2d0*(k-1)*pi/nz)-cos(2d0*k*pi/nz))+R/ny*(2*i-1)/uu(i,j)
pvpcyc((i*(i-1))/2+j+(k-1)*(ny*(ny+1))/2)%mp%p%y=2d0*R/3d0/ny*(sqrt(dble((i**2-(j-1)**2)**3))+sqrt(dble(((i-1)**2-j**2)**3))-sqrt(dble(((i-1)**2-(j-1)**2)**3))-sqrt(dble((i**2-j**2)**3)))/uu(i,j)
pvpcyc((i*(i-1))/2+j+(k-1)*(ny*(ny+1))/2)%mp%p%z=(k-5d-1)*lll/nz
pvpcyc((i*(i-1))/2+j+(k-1)*(ny*(ny+1))/2)%G%vx=GG/pi*ra*(sin(2d0*k*pi/nz)-sin(2d0*(k-1d0)*pi/nz))*uu(i,j)/2d0/ny**2
pvpcyc((i*(i-1))/2+j+(k-1)*(ny*(ny+1))/2)%G%vy=0.
pvpcyc((i*(i-1))/2+j+(k-1)*(ny*(ny+1))/2)%G%vz=GG/pi*uu(i,j)*lll/2d0/ny**2/nz
end forall

forall(i=1:ny,k=1:nz)
pvpcyc((i*(i+1))/2+(k-1)*(ny*(ny+1))/2)%mp%p%x=ra*nz/(2d0*pi)*(cos(2d0*(k-1)*pi/nz)-cos(2d0*k*pi/nz))+R/ny*(i-1d0/3d0)/uu(i,i)
pvpcyc((i*(i+1))/2+(k-1)*(ny*(ny+1))/2)%mp%p%y=2d0*R/3d0/ny*sqrt(dble((2*i-1)**3))/uu(i,i)
pvpcyc((i*(i+1))/2+(k-1)*(ny*(ny+1))/2)%mp%p%z=(k-5d-1)*lll/nz
pvpcyc((i*(i+1))/2+(k-1)*(ny*(ny+1))/2)%G%vx=GG/pi*ra*(sin(2d0*k*pi/nz)-sin(2d0*(k-1)*pi/nz))*uu(i,i)/2d0/ny**2
pvpcyc((i*(i+1))/2+(k-1)*(ny*(ny+1))/2)%G%vy=0.
pvpcyc((i*(i+1))/2+(k-1)*(ny*(ny+1))/2)%G%vz=GG/pi*uu(i,i)*lll/2d0/ny**2/nz
end forall

do k=2,4
pvpcyc((k-1)*nz*(ny*(ny+1))/2+1:k*nz*(ny*(ny+1))/2)=pvpcyc(1:nz*(ny*(ny+1))/2)
end do

pvpcyc(nz*(ny*(ny+1))/2+1:nz*ny*(ny+1))%mp%p%y=-pvpcyc(1:nz*(ny*(ny+1))/2)%mp%p%y
pvpcyc(3*nz*(ny*(ny+1))/2+1:2*nz*(ny*(ny+1)))%mp%p%y=-pvpcyc(1:nz*(ny*(ny+1))/2)%mp%p%y

forall(i=1:ny,j=1:ny,k=1:nz,j<=i-1) pvpcyc(nz*ny*(ny+1)+(i*(i-1))/2+j+(k-1)*(ny*(ny+1))/2)%mp%p%x=ra*nz/(2d0*pi)*(cos(2d0*(k-1)*pi/nz)-cos(2d0*k*pi/nz))-R/ny*(2*i-1)/uu(i,j)
forall(i=1:ny,k=1:nz) pvpcyc(nz*ny*(ny+1)+(i*(i+1))/2+(k-1)*(ny*(ny+1))/2)%mp%p%x=ra*nz/(2d0*pi)*(cos(2d0*(k-1)*pi/nz)-cos(2d0*k*pi/nz))-R/ny*(i-1d0/3d0)/uu(i,i)

pvpcyc(3*nz*(ny*(ny+1))/2+1:2*nz*(ny*(ny+1)))%mp%p%x=pvpcyc(nz*ny*(ny+1)+1:3*nz*(ny*(ny+1))/2)%mp%p%x


end function cyl_sin_pert

function simple_cyl(GG,lll,R,nr,nth,nz) result(sscyl)
real(kind(0.d0)), intent(in) :: GG, lll, R
integer, intent(in) :: nr, nth, nz
type(pvortex), dimension(nr*nth*nz) :: sscyl
integer :: i, j, k

forall(i=1:nr,j=1:nth,k=1:nz)
sscyl((j-1)*nr+i+(k-1)*nth*nr)%mp%p=O+(i**2-i+1./3.)/(i-0.5)*R*nth/(2*pi*nr)*((sin(j*2*pi/nth)-sin((j-1)*2*pi/nth))*ii+(-cos(j*2*pi/nth)+cos((j-1)*2*pi/nth))*jj)+(k-0.5)*lll/nz*kk
sscyl((j-1)*nr+i+(k-1)*nth*nr)%G=kk*GG*(i-0.5)*(R**2*2*pi*lll/(nr**2*nth*nz))
end forall

end function simple_cyl

function cyl_velo(r0,lel,R,points,circ) result(veloc)
type(point), intent(in) :: r0
type(vector), intent(in) :: lel
real(kind(0.d0)), intent(in) :: R,circ
type(point), dimension(:),intent(in) :: points
type(vector), dimension(size(points)) :: veloc
real(kind(0.d0)),dimension(size(points)) :: radius
type(vector),dimension(size(points)) :: theta
radius=sqrt(norm2(points-r0)-((points-r0)*lel)**2)
theta=((points-r0).x.lel)/norm((points-r0).x.lel)
where(radius<=R) 
veloc=circ/(2*pi*R**2)*radius*theta
elsewhere
veloc=circ/(2*pi*radius)*theta
end where
end function cyl_velo 

function rec_grid(xd,xu,nx,yd,yu,ny,zd,zu,nz) result(grd)
real(kind(0.d0)), intent(in) :: xd,xu,yd,yu,zd,zu
integer, intent(in) :: nx,ny,nz
type(mpoint),dimension(nx*ny*nz) :: grd
integer :: i,j,k
real(kind(0.d0)),dimension(nx) :: xg
real(kind(0.d0)),dimension(ny) :: yg
real(kind(0.d0)),dimension(nz) :: zg
xg=midlinspace(xd,xu,nx)
yg=midlinspace(yd,yu,ny)
zg=midlinspace(zd,zu,nz)
forall(i=1:nx,j=1:ny,k=1:nz)
grd((i-1)*ny*nz+(k-1)*ny+j)%p=O+xg(i)*ii+yg(j)*jj+zg(k)*kk
end forall
grd%v=zero_v
end function rec_grid

end module vp_init


module redistribution

use space3d
use sing_elem
use Hermitian_n_stuff

implicit none

real(kind(0.d0)), dimension(:), allocatable :: circulation
real(kind(0.d0)), dimension(:), allocatable :: totL
real(kind(0.d0)), dimension(:), allocatable :: overlapping
integer, dimension(:), allocatable :: particles_at_line
integer, dimension(:), allocatable :: particles_at_line_new
type(pvortex), dimension(:), allocatable :: vpshelp
real(kind(0.d0)) :: dl

contains

subroutine periodic_rdstr_check_n_do(vps)
use periodic_parameters
type(pvortex), dimension(:), allocatable, intent(inout) :: vps
integer :: npart, q, nn, npart1, nn1, j
npart=0
do q=1,size(particles_at_line)
nn=particles_at_line(q)
allocate(point_set(nn+1),vector_set(nn+1),u_set(nn+1))
point_set(1:nn)=vps(npart+1:npart+nn)%mp%p+vps(npart+1:npart+nn)%pmove*ll*le
vector_set(1:nn)=vps(npart+1:npart+nn)%G*(nn/circulation(q)/(vps(npart+1)%u+vps(npart+nn)%u))
u_set(1:nn)=vps(npart+1:npart+nn)%u-vps(npart+1)%u
point_set(1+nn)=point_set(1)+le*ll
vector_set(1+nn)=vector_set(1)
u_set(1+nn)=vps(npart+nn)%u+vps(npart+1)%u
totL(q)=adpt_Hermitian_length(u_set(1),u_set(nn+1),m_par)
particles_at_line_new(q)=nint((totL(q)/u_set(nn+1))*nn)
!print *,particles_at_line_new(q)
deallocate(point_set,vector_set,u_set)
npart=nn+npart
end do

if (any(particles_at_line-particles_at_line_new /= 0)) then
   allocate(vpshelp(sum(particles_at_line_new)))
    print *, 'REDISTRIBUTION IN PROCESS'
    print *,particles_at_line_new
   npart=0
   npart1=0
   do q=1,size(particles_at_line)
      nn=particles_at_line(q)
      nn1=particles_at_line_new(q)
      if ((nn-nn1) /=0) then
         print *, 'fixing line numbered:',q
         allocate(point_set(nn+1),vector_set(nn+1),u_set(nn+1))
         point_set(1:nn)=vps(npart+1:npart+nn)%mp%p+vps(npart+1:npart+nn)%pmove*ll*le
         vector_set(1:nn)=vps(npart+1:npart+nn)%G*(nn/circulation(q)/(vps(npart+1)%u+vps(npart+nn)%u))
         u_set(1:nn)=vps(npart+1:npart+nn)%u-vps(npart+1)%u
         point_set(1+nn)=point_set(1)+le*ll
         vector_set(1+nn)=vector_set(1)
         u_set(1+nn)=vps(npart+nn)%u+vps(npart+1)%u
         dl=totL(q)/(2d0*nn1)
         print *, 'i will try to find u'
         !vpshelp(npart1+1:npart1+nn1)%u=midlinspace(0d0,u_set(1+nn),nn1)
         call Herm_find_u(u_set(1),dl,vpshelp(npart1+1)%u)
         dl=2d0*dl
         do j=1,nn1-1
            call Herm_find_u(vpshelp(npart1+j)%u,dl,vpshelp(npart1+j+1)%u)
         end do
         print *, 'found u'
         vpshelp(npart1+1:npart1+nn1)%mp%p=Hermitian(vpshelp(npart1+1:npart1+nn1)%u)
         vpshelp(npart1+1:npart1+nn1)%G=unit(tanHermitian(vpshelp(npart1+1:npart1+nn1)%u))*circulation(q)*dl
         vpshelp(npart1+1:npart1+nn1)%e_mol=vps(npart+1)%e_mol
  !  vpshelp(npart1+1:npart1+nn1)%e_mol=overlapping(q)/9.136553695187546d-01*dl 
         vpshelp(npart1+1:npart1+nn1)%pmove=0d0
         vpshelp(npart1+1:npart1+nn1)%u=midlinspace(0d0,totL(q),nn1)
         if (any((vpshelp(npart1+1:npart1+nn1)%mp%p-Rp)*le > ll/0.2d1)) print *, 'particles out of period, moved at left'
         if (any((vpshelp(npart1+1:npart1+nn1)%mp%p-Rp)*le < -ll/0.2d1)) print *, 'particles out of period, moved at right'
         where((vpshelp(npart1+1:npart1+nn1)%mp%p-Rp)*le >= ll/0.2d1) 
            vpshelp(npart1+1:npart1+nn1)%pmove=1d0
            vpshelp(npart1+1:npart1+nn1)%mp%p=vpshelp(npart1+1:npart1+nn1)%mp%p+(-ll)*le
         end where
        where((vpshelp(npart1+1:npart1+nn1)%mp%p-Rp)*le < -ll/0.2d1 )   
           vpshelp(npart1+1:npart1+nn1)%pmove=-1d0
            vpshelp(npart1+1:npart1+nn1)%mp%p=vpshelp(npart1+1:npart1+nn1)%mp%p+ll*le
         end where
            if (all(vpshelp(npart1+1:npart1+nn1)%pmove==1d0) .or. all(vpshelp(npart1+1:npart1+nn1)%pmove==-1d0)) vpshelp(npart1+1:npart1+nn1)%pmove=0d0
         deallocate(point_set,vector_set,u_set)
      else
         print *, 'did not have to fix line numbered:',q  
        vpshelp(npart1+1:npart1+nn1)=vps(npart+1:npart+nn)
     end if
      npart=nn+npart
      npart1=nn1+npart1
   end do
   deallocate(vps)
   allocate(vps(sum(particles_at_line_new)))
   vps=vpshelp
   deallocate(vpshelp)
   particles_at_line=particles_at_line_new
end if

end subroutine periodic_rdstr_check_n_do


subroutine rdstr_check_n_do(vps)
type(pvortex), dimension(:), allocatable, intent(inout) :: vps
integer :: npart, q, nn, npart1, nn1, j
npart=0
do q=1,size(particles_at_line)
nn=particles_at_line(q)
allocate(point_set(nn+1),vector_set(nn+1),u_set(nn+1))
point_set(1:nn)=vps(npart+1:npart+nn)%mp%p
vector_set(1:nn)=vps(npart+1:npart+nn)%G*(nn/circulation(q)/(vps(npart+1)%u+vps(npart+nn)%u))
u_set(1:nn)=vps(npart+1:npart+nn)%u-vps(npart+1)%u
point_set(1+nn)=point_set(1)
vector_set(1+nn)=vector_set(1)
u_set(1+nn)=vps(npart+nn)%u+vps(npart+1)%u
totL(q)=adpt_Hermitian_length(u_set(1),u_set(nn+1),m_par)
particles_at_line_new(q)=nint((totL(q)/u_set(nn+1))*nn)
print *,particles_at_line_new(q)
deallocate(point_set,vector_set,u_set)
npart=nn+npart
end do

if (any(particles_at_line-particles_at_line_new /= 0)) then
   allocate(vpshelp(sum(particles_at_line_new)))
   npart=0
   npart1=0
   do q=1,size(particles_at_line)
      nn=particles_at_line(q)
      nn1=particles_at_line_new(q)
      if ((nn-nn1) /=0) then
         print *, 'fixing line numbered:',q
         allocate(point_set(nn+1),vector_set(nn+1),u_set(nn+1))
         point_set(1:nn)=vps(npart+1:npart+nn)%mp%p
         vector_set(1:nn)=vps(npart+1:npart+nn)%G*(nn/circulation(q)/(vps(npart+1)%u+vps(npart+nn)%u))
         u_set(1:nn)=vps(npart+1:npart+nn)%u-vps(npart+1)%u
         point_set(1+nn)=point_set(1)
         vector_set(1+nn)=vector_set(1)
         u_set(1+nn)=vps(npart+nn)%u+vps(npart+1)%u
         dl=totL(q)/(2d0*nn1)
         print *, 'i will try to find u'
         !vpshelp(npart1+1:npart1+nn1)%u=midlinspace(0d0,u_set(1+nn),nn1)
         call Herm_find_u(u_set(1),dl,vpshelp(npart1+1)%u)
         dl=2d0*dl
         do j=1,nn1-1
            call Herm_find_u(vpshelp(npart1+j)%u,dl,vpshelp(npart1+j+1)%u)
         end do
         print *, 'found u'
         vpshelp(npart1+1:npart1+nn1)%mp%p=Hermitian(vpshelp(npart1+1:npart1+nn1)%u)
         vpshelp(npart1+1:npart1+nn1)%G=unit(tanHermitian(vpshelp(npart1+1:npart1+nn1)%u))*circulation(q)*dl
         vpshelp(npart1+1:npart1+nn1)%e_mol=vps(npart+1)%e_mol
  !  vpshelp(npart1+1:npart1+nn1)%e_mol=overlapping(q)/9.136553695187546d-01*dl 
         vpshelp(npart1+1:npart1+nn1)%u=midlinspace(0d0,totL(q),nn1)
         deallocate(point_set,vector_set,u_set)
      else
         print *, 'did not have to fix line numbered:',q  
        vpshelp(npart1+1:npart1+nn1)=vps(npart+1:npart+nn)
     end if
      npart=nn+npart
      npart1=nn1+npart1
   end do
   deallocate(vps)
   allocate(vps(sum(particles_at_line_new)))
   vps=vpshelp
   deallocate(vpshelp)
   particles_at_line=particles_at_line_new
end if

end subroutine rdstr_check_n_do

end module redistribution

module vortwalk

use space3d
use sing_elem
use implied_defs
use periodic_parameters

implicit none

contains

subroutine vo2vo_velo(vo)
use ieee_exceptions
implicit none
type(pvortex), dimension(:), intent(inout) :: vo 
integer                                                         :: i, j
vo%mp%v=zero_v
do i=1,size(vo)
   do j=1,size(vo)
      if ((i/=j) .and. (vo(i).dist.vo(j))/=0d0) call indvel(vo(i),vo(j))
      if (signal) then
         print *, 'PROBLEM FOUND :: VELOCITY'
         print *, 'from vortex',i,'to vortex',j
         return
      end if
   end do
end do
end subroutine vo2vo_velo

subroutine vo2vo_G(vo)
implicit none
type(pvortex), dimension(:), intent(inout) :: vo 
integer                                                         :: i, j
vo%dg=zero_v
do i=1,size(vo)
   do j=1,size(vo)
      if ((i/=j) .and. (vo(i).dist.vo(j))/=0d0) call ind_G_vv(vo(i),vo(j))
      if (signal) then
         print *, 'PROBLEM FOUND :: VORTICITY'
         print *, 'from vortex',i,'to vortex',j
         return
      end if
   end do
end do
end subroutine vo2vo_G

subroutine periodic_vo2vo_velo(vo)
use periodic_parameters
implicit none
type(pvortex), dimension(:), intent(inout) :: vo 
integer                                                         :: i, j
!vo%mp%v=Vinf
do i=1,size(vo)
   do j=1,size(vo) 
     if (i/=j) call periodic_indvel(vo(i),vo(j))  
     if (signal) then
         print *, 'PROBLEM FOUND :: PERIODIC VELOCITY'
         print *, 'from vortex',i,'to vortex',j
         return
      end if
   end do
end do
end subroutine periodic_vo2vo_velo

subroutine periodic_vo2vo_G(vo)
use periodic_parameters
implicit none
type(pvortex), dimension(:), intent(inout) :: vo 
integer                                                         :: i, j
do i=1,size(vo)
   do j=1,size(vo)
      call periodic_ind_G_vv(vo(i),vo(j))
      if (signal) then
         print *, 'PROBLEM FOUND :: PERIODIC VORTICITY'
         print *, 'from vortex',i,'to vortex',j
         return
      end if
   end do
end do
end subroutine periodic_vo2vo_G

subroutine vo2mp_velo(vo,mo)
use ieee_exceptions
implicit none
type(pvortex), dimension(:), intent(in) :: vo 
type(mpoint),  dimension(:), intent(inout) :: mo
integer                                                         :: i, j
mo%v=zero_v
do i=1,size(vo)
   do j=1,size(mo)
      if  ((vo(i).dist.mo(j))/=0d0) call indvel(vo(i),mo(j))
      if (signal) then
         print *, 'PROBLEM FOUND :: VELOCITY'
         print *, 'from vortex',i,'to vortex',j
         return
      end if
   end do
end do
end subroutine vo2mp_velo

subroutine periodic_vo2mp_velo(vo,mo)
use ieee_exceptions
implicit none
type(pvortex), dimension(:), intent(in) :: vo 
type(mpoint),  dimension(:), intent(inout) :: mo
integer                                                         :: i, j
do i=1,size(vo)
   do j=1,size(mo)
      if  ((vo(i).dist.mo(j))/=0d0) call periodic_indvel(vo(i),mo(j))
      if (signal) then
         print *, 'PROBLEM FOUND :: VELOCITY'
         print *, 'from vortex',i,'to vortex',j
         return
      end if
   end do
end do
end subroutine periodic_vo2mp_velo

subroutine vp_walk(vo,dt)
type(pvortex), dimension(:)            , intent(inout) :: vo
real(kind(0.d0))                                , intent(in)      :: dt
type(vector)  , dimension(size(vo))                        :: ph, qh, pg, qg
call vo2vo_velo(vo)
call vo2vo_G(vo)
ph=dt*vo%mp%v
pg=dt*vo%dG
vo%mp%p=vo%mp%p+ph/2d0
vo%G=vo%G+pg/2d0
qh=ph
qg=pg
call vo2vo_velo(vo)
call vo2vo_G(vo)
ph=dt*vo%mp%v
pg=dt*vo%dG
vo%mp%p=vo%mp%p+ph/2d0+(-1d0)*qh/2d0
vo%G=vo%G+pg/2d0+(-1d0)*qg/2d0
qh=qh/6d0
qg=qg/6d0
call vo2vo_velo(vo)
call vo2vo_G(vo)
ph=dt*vo%mp%v-ph/2d0
pg=dt*vo%dG-pg/2d0
vo%mp%p=vo%mp%p+ph
vo%G=vo%G+pg
qh=qh-ph
qg=qg-pg
call vo2vo_velo(vo)
call vo2vo_G(vo)
ph=dt*vo%mp%v+2d0*ph
pg=dt*vo%dG+2d0*pg
vo%mp%p=vo%mp%p+qh+ph/6d0
vo%G=vo%G+qg+pg/6d0
end subroutine vp_walk


subroutine periodic_vp_walk(vo,dt)
use ieee_exceptions
!use vp_init
implicit none
type(pvortex), dimension(:)             , intent(inout) :: vo
real(kind(0.d0))                                 , intent(in)      :: dt
type(vector)  , dimension(size(vo))                        :: ph, qh, pg, qg
type(pvortex), dimension(3*size(vo))                    :: vohelp
logical :: l1
call make_vps_help(vo,vohelp)
call vo2vo_velo(vohelp)
if (signal) return
call vo2vo_G(vohelp)
if (signal) return
vo=vohelp(1:size(vo))
call periodic_vo2vo_velo(vo)
if (signal) return
call periodic_vo2vo_G(vo)
if (signal) return
ph=dt*vo%mp%v
pg=dt*vo%dG
vo%mp%p=vo%mp%p+ph/2d0
vo%G=vo%G+pg/2d0
qh=ph
qg=pg
!where((vo%mp%p-Rp)*le >= ll/0.2d1)
!   vo(1:size(vo))%mp%p=vo%mp%p+(-ll)*le
!   pmove=pmove+0.1d1
!end where
!where((vo%mp%p-Rp)*le < -ll/0.2d1 ) 
!   vo(1:size(vo))%mp%p=vo%mp%p+ll*le
!   pmove=pmove-0.1d1
!end where
!vo=Herm_redistr(vo,size(vo),0.1664d4)
call make_vps_help(vo,vohelp)
call vo2vo_velo(vohelp)
if (signal) return
call vo2vo_G(vohelp)
if (signal) return
vo=vohelp(1:size(vo))
call periodic_vo2vo_velo(vo)
if (signal) return
call periodic_vo2vo_G(vo)
if (signal) return
ph=dt*vo%mp%v
pg=dt*vo%dG
vo%mp%p=vo%mp%p+ph/2d0+(-1d0)*qh/2d0
vo%G=vo%G+pg/2d0+(-1d0)*qg/2d0
qh=qh/6d0
qg=qg/6d0
!where((vo%mp%p-Rp)*le >= ll/0.2d1)
!   vo(1:size(vo))%mp%p=vo%mp%p+(-ll)*le
!   pmove=pmove+0.1d1
!end where
!where((vo%mp%p-Rp)*le < -ll/0.2d1 ) 
!   vo(1:size(vo))%mp%p=vo%mp%p+ll*le
!   pmove=pmove-0.1d1
!end where
!vo=Herm_redistr(vo,size(vo),0.1664d4)
call make_vps_help(vo,vohelp)
call vo2vo_velo(vohelp)
if (signal) return
call vo2vo_G(vohelp)
if (signal) return
vo=vohelp(1:size(vo))
call periodic_vo2vo_velo(vo)
if (signal) return
call periodic_vo2vo_G(vo)
if (signal) return
ph=dt*vo%mp%v-ph/2d0
pg=dt*vo%dG-pg/2d0
vo%mp%p=vo%mp%p+ph
vo%G=vo%G+pg
qh=qh-ph
qg=qg-pg
!where((vo%mp%p-Rp)*le >= ll/0.2d1)
!   vo(1:size(vo))%mp%p=vo%mp%p+(-ll)*le
!   pmove=pmove+0.1d1
!end where
!where((vo%mp%p-Rp)*le < -ll/0.2d1 ) 
!   vo(1:size(vo))%mp%p=vo%mp%p+ll*le
!   pmove=pmove-0.1d1
!end where
!vo=Herm_redistr(vo,size(vo),0.1664d4)
call make_vps_help(vo,vohelp)
call vo2vo_velo(vohelp)
if (signal) return
call vo2vo_G(vohelp)
if (signal) return
vo=vohelp(1:size(vo))
call periodic_vo2vo_velo(vo)
if (signal) return
call periodic_vo2vo_G(vo)
if (signal) return
ph=dt*vo%mp%v+2d0*ph
pg=dt*vo%dG+2d0*pg
vo%mp%p=vo%mp%p+qh+ph/6d0
vo%G=vo%G+qg+pg/6d0
where((vo%mp%p-Rp)*le >= ll/0.2d1)
   vo%mp%p=vo%mp%p+(-ll)*le
   vo%pmove=vo%pmove+0.1d1
end where
where((vo%mp%p-Rp)*le < -ll/0.2d1 ) 
   vo%mp%p=vo%mp%p+ll*le
   vo%pmove=vo%pmove-0.1d1
end where
!vo=Herm_redistr(vo,size(vo),0.1664d4)
end subroutine periodic_vp_walk

end module vortwalk


program vpwalk

use ieee_exceptions
use space3d
use implied_defs
use sing_elem
use vortwalk
use vp_init
use periodic_parameters
use Hermitian_n_stuff
use redistribution

implicit none

character(len=*),parameter :: fileext='case2'
integer, parameter ::  ntsteps=1500, nvort=400 !nysm=2, nybg=3, nzsm=120, nzbg=120 !!!!ny=2,nz=100!nvort=350!!, nth=8, nr=5, nz=10,nvort=350
real(kind(0.d0))     , parameter :: dt=0.2d-2, dv1v2=0.10d0
type(pvortex), dimension(:), allocatable :: vps
!type(pvortex), dimension(2*nzsm*nysm*(nysm+1)+2*nzbg*nybg*(nybg+1)) :: vps
!type(pvortex), dimension(2*2*2*nz*ny*(ny+1)) :: vps
!type(pvortex), dimension((2*ns-1)*2*nth*nz*nr) :: vpshelp
!type(pvortex), dimension(nth*nz*nr) :: vps
integer :: i, q, j, no_of_lines, npart
real :: time1, time2
logical :: invalid_stop

call find_M_N


!subroutine set_vline_sin_pert(vortex_line,loc_circ,nv,ovrl_deg,r0,lll,amp,lel,amp_e)
!subroutine set_vline_simple(vortex_line,loc_circ,nv,ovrl_deg,r0,lll,lel)

!set_vline_sin_pert(vlset(1),0.1649d4,nvort,0.2d1,O+(-0.5)*ll*le,ll,0.1d1,le,kk)
!set_vline_simple(vlset(2),0.1649d4,nvort,0.2d1,O+(-0.5)*ll*le+9.682*ii,ll,le)
!make_vp_set(vlset,vps)

!function set_sin_vp(r0,lel,ne,amp,lll,nv,GG,ovrlp) result(vp)
!function simple_vpline(r0,lel,lll,n,GG,ovrlp) result(vpl)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!cylinder runs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!no_of_lines=60
!allocate(vps(no_of_lines*nvort),circulation(no_of_lines),overlapping(no_of_lines), particles_at_line(no_of_lines), totL(no_of_lines),particles_at_line_new(no_of_lines))

! vps=vp_cyl_Lamb(O+(-0.5)*ll*le,le,ll,nvort,3d0,kk,(/6, 12, 18, 24/),0.1234d4,4d0)
!print *, vps(1)%e_mol
 !vps(18*nvort+1:36*nvort)=vp_cyl_rankine(O+(-0.5)*ll*le-5d0*ii,le,ll,nvort,3d0,kk,(/3, 6, 9/),0.1234d4,4d0)

!do i=1,no_of_lines
! circulation(i)=norm(vps((i-1)*nvort+1)%G)*nvort/ll
! end do
! overlapping=1.12d0
! particles_at_line=nvort


!allocate(vps(2*nvort),overlapping(2),particles_at_line(2),circulation(2))

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!simple runs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!no_of_lines=1
!allocate(vps(no_of_lines*nvort),circulation(no_of_lines),overlapping(no_of_lines), particles_at_line(no_of_lines), totL(no_of_lines),particles_at_line_new(no_of_lines))

!circulation(1)=-0.1234d4
! overlapping(1)=9d0
! particles_at_line(1)=nvort

 !circulation(2)=-0.1234d4
 !overlapping(2)=6d0
 !particles_at_line(2)=nvort

!vps(1:particles_at_line(1))=simple_vpline(O+(-0.5)*ll*le,le,ll,nvort,circulation(1),overlapping(1))
!print *, vps(1)%e_mol
!vps(particles_at_line(1)+1:particles_at_line(1)+particles_at_line(2))=simple_vpline(O+(-0.5)*ll*le+(-5d0)*ii,le,ll,nvort,circulation(2),overlapping(2))
!print *, vps(particles_at_line(1)+1)%e_mol
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!ortega runs!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
no_of_lines=4
allocate(vps(no_of_lines*nvort),circulation(no_of_lines),overlapping(no_of_lines), particles_at_line(no_of_lines), totL(no_of_lines),particles_at_line_new(no_of_lines))

 circulation(1)=0.589d3
  overlapping(1)=0.8d1
  particles_at_line(1)=nvort

 circulation(2)=-0.1071d4
 overlapping(2)=2.4d1
 particles_at_line(2)=nvort

 circulation(3)=-0.589d3
 overlapping(3)=0.8d1
 particles_at_line(3)=nvort

 circulation(4)=0.1071d4
 overlapping(4)=2.4d1
 particles_at_line(4)=nvort

vps(1:particles_at_line(1))=set_sin_vp(O+(-0.5)*ll*le+29.15d0*ii+(-8.629d0)*kk,le,ii,0.1d1,ll,particles_at_line(1),circulation(1),overlapping(1))
print *, vps(1)%e_mol
vps(particles_at_line(1)+1:particles_at_line(1)+particles_at_line(2))=simple_vpline(O+(-0.5)*ll*le+29.15d0*ii,le,ll,particles_at_line(2),circulation(2),overlapping(2))
print *, vps(particles_at_line(1)+1)%e_mol
vps(particles_at_line(1)+particles_at_line(2)+1:particles_at_line(1)+particles_at_line(2)+particles_at_line(3))=set_sin_vp(O+(-0.5)*ll*le+(-29.15d0)*ii+(-8.629d0)*kk,le,ii,0.1d1,ll,particles_at_line(3),circulation(3),overlapping(3))
print *, vps(particles_at_line(1)+particles_at_line(2)+1)%e_mol
vps(particles_at_line(1)+particles_at_line(2)+particles_at_line(3)+1:particles_at_line(1)+particles_at_line(2)+particles_at_line(3)+particles_at_line(4))=simple_vpline(O+(-0.5)*ll*le+(-29.15d0)*ii,le,ll,particles_at_line(4),circulation(4),overlapping(4))
print *, vps(particles_at_line(1)+particles_at_line(2)+particles_at_line(3)+1)%e_mol

totL(1)=vps(1)%u+vps(particles_at_line(1))%u
totL(2)=vps(particles_at_line(1)+1)%u+vps(particles_at_line(1)+particles_at_line(2))%u
totL(3)=vps(particles_at_line(1)+particles_at_line(2)+1)%u+vps(particles_at_line(1)+particles_at_line(2)+particles_at_line(3))%u
totL(4)=vps(particles_at_line(1)+particles_at_line(2)+particles_at_line(3)+1)%u+vps(particles_at_line(1)+particles_at_line(2)+particles_at_line(3)+particles_at_line(4))%u


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 !circulation(1:8)=-0.1649d4/0.8d1
 !overlapping(1:8)=0.4d1
 !particles_at_line=nvort

!npart=0
!do i=1,8
!vps(1+npart:particles_at_line(i)+npart)=set_sin_vp(O+(-0.5)*ll*le+0.153073d1*cos(pi/0.8d1+(i-1)*pi/4d0)*ii+0.153073d1*sin(pi/0.8d1+(i-1)*pi/4d0)*kk,le,kk,0.1d1,ll,particles_at_line(i),circulation(i),overlapping(i))
!vps(1+npart:particles_at_line(i)+npart)=simple_vpline(O+(-0.5)*ll*le+21.8418*ii+0.153073d1*cos(pi/0.8d1+(i-1)*pi/4d0)*ii+0.153073d1*sin(pi/0.8d1+(i-1)*pi/4d0)*kk,le,ll,particles_at_line(i),circulation(i),overlapping(i))
!totL(i)=vps(1+npart)%u+vps(particles_at_line(i)+npart)%u
!npart=npart+particles_at_line(i)
!end do  


! circulation(9:16)=0.1649d4/0.8d1
!overlapping(9:18)=0.4d1
 
!do i=9,16
!vps(1+npart:particles_at_line(i)+npart)=set_sin_vp(O+(-0.5)*ll*le+0.153073d1*cos(pi/0.8d1+(i-1)*pi/4d0)*ii+0.153073d1*sin(pi/0.8d1+(i-1)*pi/4d0)*ii,le,kk,0.1d1,ll,particles_at_line(i),circulation(i),overlapping(i))
!vps(1+npart:particles_at_line(i)+npart)=simple_vpline(O+(-0.5)*ll*le+(-21.8418)*ii+0.153073d1*cos(pi/0.8d1+(i-9)*pi/4d0)*ii+0.153073d1*sin(pi/0.8d1+(i-9)*pi/4d0)*kk,le,ll,particles_at_line(i),circulation(i),overlapping(i))
!totL(i)=vps(1+npart)%u+vps(particles_at_line(i)+npart)%u
!npart=npart+particles_at_line(i)
!end do

! circulation(17)=0.608d3
!i=17
!vps(1+npart:particles_at_line(i)+npart)=set_sin_vp(O+(-0.5)*ll*le+31.5238d0*ii,le,kk,0.1d1,ll,particles_at_line(i),circulation(i),overlapping(i))
!totL(i)=vps(1+npart)%u+vps(particles_at_line(i)+npart)%u
!npart=npart+particles_at_line(i)


! circulation(18)=-0.608d3
!i=18
!vps(1+npart:particles_at_line(i)+npart)=set_sin_vp(O+(-0.5)*ll*le+(-31.5238d0)*ii,le,kk,0.1d1,ll,particles_at_line(i),circulation(i),overlapping(i))
!totL(i)=vps(1+npart)%u+vps(particles_at_line(i)+npart)%u



print *,'No of vortices is', size(vps)
print *, 'Time steps',ntsteps,'step is',dt 
print *, fileext

open(9,file=('velo'//fileext//'.txt'))
open(10,file=('pos'//fileext//'.txt'))
open(11,file=('G'//fileext//'.txt'))
open(12,file=('n'//fileext//'.txt'))
open(13,file=('nperline'//fileext//'.txt'))
open(14,file=('totLperline'//fileext//'.txt'))
open(15,file=('tcplot'//fileext//'.txt'))



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!MATLAB INPUT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(9,'(3f30.15)') vps%mp%v
write(10,'(3f30.15)') vps%mp%p
write(11,'(3f30.15)') vps%G
write(12,'(i20)') size(vps)
write(13,'(i20)') particles_at_line
write(14,'(f30.15)') totL
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!TECPLOT INPUT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(15,*) 'TITLE ='//fileext
write(15,'(a53)') 'VARIABLES = "x","y","z","vx","vy","vz","wx","wy","wz"'
npart=0
do i=1,no_of_lines
write(15,'(a14,i5,a5,i10,a24,f10.5)') 'ZONE T = "line',i,'", I=',particles_at_line(i),', F=POINT, SOLUTIONTIME=',0d0
do j=1+npart,particles_at_line(i)+npart
write(15,'(9f30.15)') vps(j)%mp%p,vps(j)%mp%v,vps(j)%G
!write(15,'(9f30.15)') vps(1+npart:particles_at_line(i)+npart)%mp%p
end do
npart=particles_at_line(i)+npart
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



do i=1,ntsteps
print *, 'Time step',i
call cpu_time(time1)

call periodic_vp_walk(vps,dt)
! call vp_walk(vps,dt)

call cpu_time(time2)

print *, 'inductions took',time2-time1,'sec'

   if (signal) then 
      print *, ' I stoped after doing',i,'time steps'
      exit
   end if

   write(9,'(3f30.15)') vps%mp%v
   write(10,'(3f30.15)') vps%mp%p
   write(11,'(3f30.15)') vps%G
   write(12,'(i20)')        size(vps)
   write(13,'(i20)')        particles_at_line
   write(14,'(f30.15)')   totL

call cpu_time(time1)

call periodic_rdstr_check_n_do(vps)

call cpu_time(time2)
print *, 'redstr took',time2-time1,'sec'

call cpu_time(time1)


   npart=0
   do q=1,no_of_lines
   write(15,'(a14,i5,a5,i10,a24,f10.5)') 'ZONE T = "line',q,'", I=',particles_at_line(q),', F=POINT, SOLUTIONTIME=',dt*i
   do j=1+npart,particles_at_line(q)+npart
      write(15,'(9f30.15)') vps(j)%mp%p,vps(j)%mp%v,vps(j)%G
   end do
   npart=particles_at_line(q)+npart
   end do
  ! write(14,'(f25.10)')  norm(vps%mp%v)
  ! write(15,'(3f25.10)') vps%dG
  
call cpu_time(time2)
print *, 'output took', time2-time1,'sec'

end do

end program vpwalk