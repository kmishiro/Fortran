real curx(260),cury(260),wx(260),wy(260),acurx(31),acury(31),awndx(31),awndy(31)
integer idcx(260),idcy(260),idwx(260),idwy(260)
!open(99,file="s4_mar01-12")
open(95,file='p-k_2003_s4.bc')
open(94,file='p-k_2003_s4hr.bc')
open(1,file='Sorted_OFFSHOREWB_Mar01-12_03_org.txt')
read(1,*)
N=260
i1=0
i2=0
i3=0
i4=0
do i=1,N
read(1,100)id,ih,x1,x2,x3,x4
if(x1.gt.-999.)then
i1=i1+1
idcx(i1)=id
curx(i1)=x1
endif
if(x2.gt.-999.)then
i2=i2+1
idcy(i2)=id
cury(i2)=x2
endif
if(x3.gt.-999.)then
i3=i3+1
idwx(i3)=id
wx(i3)=x3
endif
if(x4.gt.-999.)then
i4=i4+1
idwy(i4)=id
wy(i4)=x4
endif
!write(*,*)i,curx(i),cury(i),wx(i),wy(i)
!write(*,*) idcx(i1)
wspd=sqrt(x3*x3+x4*x4)
wang=atan2(x4,x3)*180./3.1415
rh=float(ih)
if(wang.lt.0.)wang=wang+360.
write(94,102)'DT',0.5,2003,id+59,rh
write(94,'(a4)')'BQTS'
write(94,103)'QC',4,0,0.01,0.,0.,27.,2.
write(94,103)'QC',5,0,0.05,0.,0.,27.,2.
write(94,103)'QC',6,0,0.37,0.,0.,27.,2.
if(wspd.lt.999.)then
write(94,104)'WVA',wspd,wang
elseif(wspd.gt.999.)then
open(15,file='../met/met_smooth-data.txt')
do kk=1,7921
read(15,*)iy_mt,im_mt,id_mt,ih_mt,imin_mt,is_mt,wang_mt,wspd_mt
if(id_mt.eq.id+59.and.ih_mt.eq.ih.and.(imin_mt.ge.0.and.imin_mt.le.2))then
write(94,104)'WVA',wspd_mt,wang_mt
endif
enddo
close(15)
endif
write(94,'(a7)')'ENDSTEP'
enddo
call avr(99,acurx,curx,i1,idcx)
call avr(98,acury,cury,i2,idcy)
call avr(97,awndx,wx,i3,idwx)
call avr(96,awndy,wy,i4,idwy)
do i=1,12
wspd=sqrt(awndx(i)*awndx(i)+awndy(i)*awndy(i))
wang=atan2(awndy(i),awndx(i))*180./3.1415
if(wang.lt.0.)wang=wang+360.
write(95,102)'DT',0.5,2003,i+59,0.
write(95,'(a4)')'BQTS'
write(95,103)'QC',4,0,0.01,0.,0.,27.,2.
write(95,103)'QC',5,0,0.05,0.,0.,27.,2.
write(95,103)'QC',6,0,0.37,0.,0.,27.,2.
if(i+59.eq.69)then
write(95,104)'WVA',2.0,181.1
else
write(95,104)'WVA',wspd,wang
endif
write(95,'(a7)')'ENDSTEP'
enddo
!write(99,101)acurx,acury,awndx,awndy
!100 format(19x,2f13.2,65x,2f13.3)
100 format(8x,i2,1x,i2,7x,2f13.2,65x,2f13.3)
101 format(4f13.3)
102 format(a2,f14.1,i8,i8,f8.2)
103 format(a2,i14,i8,f8.4,f8.0,f8.0,f8.0,f8.0)
104 format(a3,13x,f8.1,f8.1)
stop
end
!--------------------
subroutine avr(fn,x1,x2,num,idy)
!--------------------
real x1(31),x2(260)
integer num,idy(260),fn
do iid=1,12
s=0.
n=0
do i=1,num
!write(*,*)idy(i)
if(iid.eq.idy(i))then
s=s+x2(i)
n=n+1
endif
enddo
if(n.ne.0)then
x1(iid)=s/float(n)
elseif(n.eq.0)then
x1(iid)=-9999.
endif
!x1=x2/float(num)
!stop
write(fn,*)iid,x1(iid)
enddo
return
end
