	 subroutine cap4(ibrk,z2,aC)

	 implicit complex*16(c,w,z), real*8(a-b,d-h,o-v,x-y)
	 dimension ibrk(4),z2(1),z3(4),z4(4),betam4(4),qwork4(88)
	 
		nq=5				!  Точность
		zero=(0.,0.)
		c4=(1.,0.)

	 do 2 k=1,4
	 betam4(k)=-0.5
2	 z3(k)=z2(ibrk(k))		! Отображение с диска z2 на диск z3 (перенумерация)
	
	 call qinit(4,betam4,nq,qwork4)

	 do 3 k = 1,4			! WSC Отображение с диска z3 на финальный прямоугольник z4 
3    z4(k)=wsc(z3(k),k,zero,zero,0,4, c4, z3, betam4,nq, qwork4)

		c4 = (1.,0.)/(z4(3)- z4(2))
		wc4 = -c4*z4(2)

     do 4 k = 1,4
4    z4(k) = wc4 + c4*z4(k)
	 aC = abs(z4(1)-z4(2)) ! Емк.структуры z4 с вертикал.электрод. и единич.зазором

	 ip=1 ! внутренняя  печать для тестирования ip=0
	 if(ip==0) write(*,5)(z4(k),k=1,4)
	 if(ip==0) write(*,6) aC
5    format(' Vertices of final rectangle:',4(/' (',f13.6,',',f13.6,')')/)
6    format(' C =', g12.5 / )

	 return
	 end