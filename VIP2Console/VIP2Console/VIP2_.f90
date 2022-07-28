    program vip2
    implicit real*8(a-b,d-h,o-v,x-y)
    dimension dL(2,2),dC(2,2),Um(2,2),dZ0(2,2),em(2),dCC(2,2),dLL(2,2)
    n = 2
    aw = 2
    h1 = 1.
    h2 = 0.5
    e1 = 10.
    e2 = 1.
    e3 = 1.
    t = 0.35
    call main(aw, h1, h2, t, e1, e2, e3, dL, dC)
    dCC=dC
    dLL=dL
    call dminv(dLL,n,ad)
    call nroot(n,dCC,11.127*dLL,em,Um)
    call impedance(n,dC,Um,em,dZ0)
    print*,'Capacitance matrix [C] (pF/m)';		call DPRINT(dC, n)
    print*,'Inductance matrix [L] (uH/m)';		call DPRINT(dL, n)
    print*,'Modal voltage matrix [Um] (V)'; 	call DPRINT(Um, n)
    print*,'Impedance matrix [Z0] (Ohm)'; 	call DPRINT(dZ0, n)
    print*,'Modal dielectric permitivities [em]';
    do i =1,n;
        print '(f8.2)',em(i);
    enddo;
    if(n==2) print'(a20,f8.4)','m = sqrt(em1/em2) =', sqrt(em(1)/em(2));  print*
    end