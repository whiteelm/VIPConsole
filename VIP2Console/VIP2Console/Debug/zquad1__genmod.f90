        !COMPILER-GENERATED INTERFACE MODULE: Sat Mar 26 14:58:03 2022
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ZQUAD1__genmod
          INTERFACE 
            FUNCTION ZQUAD1(ZA,ZB,KA,N,Z,BETAM,NPTSQ,QWORK)
              INTEGER(KIND=4) :: N
              COMPLEX(KIND=8) :: ZA
              COMPLEX(KIND=8) :: ZB
              INTEGER(KIND=4) :: KA
              COMPLEX(KIND=8) :: Z(N)
              REAL(KIND=8) :: BETAM(N)
              INTEGER(KIND=4) :: NPTSQ
              REAL(KIND=8) :: QWORK(1)
              COMPLEX(KIND=8) :: ZQUAD1
            END FUNCTION ZQUAD1
          END INTERFACE 
        END MODULE ZQUAD1__genmod
