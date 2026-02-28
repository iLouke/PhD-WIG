      PROGRAM ROS
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),ALX(8000),ALY(8000),
     1ALZ(8000)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GW/XW(100,8000),YW(100,8000),ZW(100,8000)
      COMMON/MP/AL(8000)
      COMMON/ANPORT/DS(8000)
      COMMON/GVV/C(8000),IPIV(8000)
      COMMON/VW/CW(100,8000),ICW(8000,2)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/PPG/IPROP(8000)
      COMMON/WAICOR/NC(8000)
      COMMON/WIN/IPW(8000)
      COMMON/WINTER/IPROPW(8000)
      COMMON/NEIB/KN(8000,8000)
      COMMON/VG1/A(8000,8000),A_ORIGINAL(8000,8000)
      COMMON/VG2/B(8000)
      COMMON/GEOM/ROT(3,3)

      REAL MAC, MAX_WAKE_LENGTH
      REAL HIST_LIFT(100)
      INTEGER MAX_ITER

      MAX_ITER = 100

      OPEN(UNIT=20,FILE='WN.WAK'  ,STATUS='UNKNOWN')
      OPEN(UNIT=48,FILE='loads.txt',STATUS='UNKNOWN')
      OPEN(UNIT=50,FILE='log.txt' ,STATUS='UNKNOWN')

      RHO=1.225
      ITER=0
      ALIFT1=0.
      VAIP=0.
      ICONV = 0
      ERROR = 0.


      CALL READ_SETTINGS(NPAN,NGRID,ALF,BET,GAM,
     1VINIT,EPS,DT,NSYM,INCH,NGRND,HFL,CGX,CGY,CGZ,WINGAREA,MAC)

      MAX_WAKE_LENGTH = DT * VINIT * MAX_ITER
      IF (MAX_WAKE_LENGTH .LE. 50*MAC) THEN
         WRITE(*,*)  'PLEASE SELECT BIGGER TIMESTEP OR VINIT'
         WRITE(50,*) 'PLEASE SELECT BIGGER TIMESTEP OR VINIT'
         STOP
      END IF
      ! ITERATION = 0
      CALL GEOM_MODDED(NPAN,NGRID,ALF,BET,GAM,NSYM,NGRND,
     1HFL,CGX,CGY,CGZ,WINGAREA,MAC)

      call print_run_settings(ALF,BET,GAM,VINIT,EPS,DT,NSYM,NGRND,
     1CGX,CGY,CGZ,WINGAREA,MAC)

      CALL ANALGEO_MODDED(NPAN)

      CALL VORCALC(VINIT,NPAN,ITER,NPW,NSYM,NGRND)
      ! ITERATION = 1
      ALIFT1 = ALIFT
      ITER = 1
      CALL WAKE(ITER,VINIT,DT,NPAN,NPW,NGW,NGRID)
      CALL NEIBORG(NPAN,NGW)
      CALL WAKREL(ITER,NPAN,NGW,NPW,DT,NSYM,NGRND)
      CALL WAKINT(NPW,ITER)
      CALL VORCALC(VINIT,NPAN,ITER,NPW,NSYM,NGRND)
      CALL CPAIP(ITER,NPAN,NPW,NSYM,VINIT,NGRND,CGX,CGY,CGZ,
     1WINGAREA,MAC,ALIFT,DRAG,SIDE,AMROLL,AMPITCH,AMYAW)

      HIST_LIFT(ITER) = ALIFT
      ERROR = 0.0

      WRITE(48,'(I2,6F15.5)') ITER, ALIFT, DRAG, SIDE,
     1AMROLL,AMPITCH,AMYAW
      WRITE(* ,100) ITER, ALIFT, DRAG, SIDE,
     1AMROLL,AMPITCH,AMYAW, ERROR, EPS
      WRITE(50 ,100) ITER, ALIFT, DRAG, SIDE,
     1AMROLL,AMPITCH,AMYAW, ERROR, EPS

      ALIFT1 = ALIFT
      ITER = ITER + 1
      
      ! ITERATION = 2 - MAX_ITERATIONS
      DO WHILE (ICONV.EQ.0)
      
         CALL WAKE(ITER,VINIT,DT,NPAN,NPW,NGW,NGRID)
         CALL WAKINT(NPW,ITER)
         CALL WAKCOR(NPAN,NGW,ITER)
         CALL WAKREL(ITER,NPAN,NGW,NPW,DT,NSYM,NGRND)
         CALL WAKINT(NPW,ITER)
         CALL WAKCOR(NPAN,NGW,ITER)
         CALL VORCALC(VINIT,NPAN,ITER,NPW,NSYM,NGRND)
         CALL CPAIP(ITER,NPAN,NPW,NSYM,VINIT,NGRND,CGX,CGY,CGZ,
     1WINGAREA,MAC,ALIFT,DRAG,SIDE,AMROLL,AMPITCH,AMYAW)

         HIST_LIFT(ITER) = ALIFT

         ERROR = SQRT((ALIFT1-ALIFT)**2/ALIFT**2)
         IF (ITER.EQ.MAX_ITER) THEN
            WRITE(* ,*) 'MAX ITERATIONS REACHED'
            WRITE(50,*) 'MAX ITERATIONS REACHED'
            ICONV = 1
         END IF
            WRITE(48,'(I2,3F15.5)') ITER, ALIFT, DRAG, SIDE
            WRITE(* ,100) ITER, ALIFT, DRAG, SIDE,
     1AMROLL,AMPITCH,AMYAW, ERROR, EPS
            WRITE(50 ,100) ITER, ALIFT, DRAG, SIDE,
     1AMROLL,AMPITCH,AMYAW, ERROR, EPS
            IF (ERROR.LE.EPS) THEN
               WAKE_LENGTH = DT * VINIT * ITER
               IF (WAKE_LENGTH .LE. 50*MAC) THEN
                  WRITE(*,*) 'WAKE LENGTH IS TOO SMALL. CONTINUING'
                  WRITE(50,*) 'WAKE LENGTH IS TOO SMALL. CONTINUING'
               ELSE
                  WRITE(*,*) 'CONVERGENCE REACHED'
                  WRITE(50,*) 'CONVERGENCE REACHED'
                  ICONV = 1
               END IF
            END IF
            ALIFT1 = ALIFT
            ITER = ITER + 1
      END DO

      DO I=1,ITER
         DO J=1,NGW
            WRITE(20,1001) I,J,XW(I,J),YW(I,J),ZW(I,J)
         END DO
      END DO
      CLOSE(20)
         
  100 FORMAT(14('-'),/,'ITERATION = ',I2,/,14('-'),/,
     1'LIFT   = ',F15.3  ,' DRAG    = ',F15.3,' SIDE  = ',F15.3,/,
     2'M_ROLL = ',F15.3  ,' M_PITCH = ',F15.3,' M_YAW = ',F15.3,/,
     3'ERROR  = ', F15.10,' EPS     = ',F15.10)
 1001 FORMAT (5X,2I8,3F15.4)

      END PROGRAM ROS


      SUBROUTINE VORTEX(X,Y,Z,X1,Y1,Z1,X2,Y2,Z2,GAMA,U,V,W)
C     SUBROUTINE VORTEX CALCULATES THE INDUCED VELOCITY (U,V,W) AT A POI
C     (X,Y,Z) DUE TO A VORTEX ELEMENT VITH STRENGTH GAMA PER UNIT LENGTH
C     POINTING TO THE DIRECTION (X2,Y2,Z2)-(X1,Y1,Z1).
      PAY=3.141592654
      RCUT=1.0E-15

C     CALCULATION OF R1 X R2
      R1R2X=(Y-Y1)*(Z-Z2)-(Z-Z1)*(Y-Y2)
      R1R2Y=-((X-X1)*(Z-Z2)-(Z-Z1)*(X-X2))
      R1R2Z=(X-X1)*(Y-Y2)-(Y-Y1)*(X-X2)

C     CALCULATION OF (R1 X R2 )**2
      SQUARE=R1R2X*R1R2X+R1R2Y*R1R2Y+R1R2Z*R1R2Z

C     CALCULATION OF R0(R1/R(R1)-R2/R(R2))
      R1=SQRT((X-X1)*(X-X1)+(Y-Y1)*(Y-Y1)+(Z-Z1)*(Z-Z1))
      R2=SQRT((X-X2)*(X-X2)+(Y-Y2)*(Y-Y2)+(Z-Z2)*(Z-Z2))
      IF((R1.LT.RCUT).OR.(R2.LT.RCUT).OR.(SQUARE.LT.RCUT)) THEN
C     WHEN POINT (X,Y,Z) LIES ON VORTEX ELEMENT; ITS INDUCED VELOCITY IS
      U=0.
      V=0.
      W=0.
      ELSE
      R0R1=(X2-X1)*(X-X1)+(Y2-Y1)*(Y-Y1)+(Z2-Z1)*(Z-Z1)
      R0R2=(X2-X1)*(X-X2)+(Y2-Y1)*(Y-Y2)+(Z2-Z1)*(Z-Z2)
      COEF=GAMA/(4.0*PAY*SQUARE)*(R0R1/R1-R0R2/R2)
      U=R1R2X*COEF
      V=R1R2Y*COEF
      W=R1R2Z*COEF
      END IF


      END

      SUBROUTINE VORTEX_RING( X, Y, Z,
     1                       X1,Y1,Z1,
     2                       X2,Y2,Z2,
     3                       X3,Y3,Z3,
     4                       X4,Y4,Z4,
     5                  GAMA, U, V, W,SYM,GRND)

      REAL   , INTENT(IN) :: X, Y, Z
      REAL   , INTENT(IN) :: X1, Y1, Z1
      REAL   , INTENT(IN) :: X2, Y2, Z2
      REAL   , INTENT(IN) :: X3, Y3, Z3
      REAL   , INTENT(IN) :: X4, Y4, Z4
      REAL   , INTENT(IN) :: GAMA
      INTEGER, INTENT(IN) :: SYM, GRND

      REAL   , INTENT(OUT) :: U, V, W

      REAL :: U1, V1, W1
      REAL :: U2, V2, W2
      REAL :: U3, V3, W3
      REAL :: U4, V4, W4

      G_LOC = GAMA
      X_LOC = X; Y_LOC = Y; Z_LOC = Z

      IF (SYM  .EQ. 1) Y_LOC = -Y; G_LOC = -GAMA
      
      IF (GRND .EQ. 1) Z_LOC = -Z

      
      ! Calculate induced velocities from each segment of the ring
      CALL VORTEX(X_LOC,Y_LOC,Z_LOC,X1,Y1,Z1,X2,Y2,Z2,G_LOC,U1,V1,W1)
      CALL VORTEX(X_LOC,Y_LOC,Z_LOC,X2,Y2,Z2,X3,Y3,Z3,G_LOC,U2,V2,W2)
      CALL VORTEX(X_LOC,Y_LOC,Z_LOC,X3,Y3,Z3,X4,Y4,Z4,G_LOC,U3,V3,W3)
      CALL VORTEX(X_LOC,Y_LOC,Z_LOC,X4,Y4,Z4,X1,Y1,Z1,G_LOC,U4,V4,W4)

      ! Sum the contributions from all segments
      U = U1 + U2 + U3 + U4
      V = V1 + V2 + V3 + V4
      W = W1 + W2 + W3 + W4

      IF (SYM  .EQ. 1) V = -V
      IF (GRND .EQ. 1) W = -W

      END

      SUBROUTINE CHECK_FAR_FIELD(X, Y, Z,
     1                           X1,Y1,Z1,
     2                           X2,Y2,Z2,
     3                           X3,Y3,Z3,
     4                           X4,Y4,Z4,FAR)
      IMPLICIT NONE
      REAL, INTENT(IN) :: X, Y, Z
      REAL, INTENT(IN) :: X1, Y1, Z1
      REAL, INTENT(IN) :: X2, Y2, Z2
      REAL, INTENT(IN) :: X3, Y3, Z3
      REAL, INTENT(IN) :: X4, Y4, Z4
      LOGICAL, INTENT(OUT) :: FAR

      REAL :: DIAGONAL, DIST1, DIST2, CPX, CPY, CPZ
      REAL :: DISTANCE

      ! INITIALIZE FAR FIELD CHECK
      FAR = .FALSE.

      ! Calculate the diagonal distance of the rectangle formed by the points
      ! DISTANCE OF POINT 1 --> 3
      DIST1 = SQRT((X1-X3)**2 + (Y1-Y3)**2 + (Z1-Z3)**2)
      ! DISTANCE OF POINT 2 --> 4
      DIST2 = SQRT((X2-X4)**2 + (Y2-Y4)**2 + (Z2-Z4)**2)
      ! SELECT THE MAXIMUM DIAGONAL DISTANCE
      DIAGONAL = MAX(DIST1, DIST2)
      ! CALCULATE THE DISTANCE FROM THE POINT TO THE CENTER OF THE RECTANGLE
      IF ((X3-X4 .LT. 1E-15).AND.
     1    (Y3-Y4 .LT. 1E-15).AND.
     2    (Z3-Z4 .LT. 1E-15)) THEN
         CPX = (X1 + X2 + X3) / 3.0
         CPY = (Y1 + Y2 + Y3) / 3.0
         CPZ = (Z1 + Z2 + Z3) / 3.0
      ELSE
         CPX = (X1 + X2 + X3 + X4) / 4.0
         CPY = (Y1 + Y2 + Y3 + Y4) / 4.0
         CPZ = (Z1 + Z2 + Z3 + Z4) / 4.0
      END IF

      DISTANCE = SQRT((X-CPX)**2 + (Y-CPY)**2 + (Z-CPZ)**2)

      IF (DISTANCE > 5.0 * DIAGONAL) THEN
         ! IF THE DISTANCE IS GREATER THAN TWICE THE DIAGONAL, IT IS FAR FIELD
         FAR = .TRUE.
      END IF
      

      END SUBROUTINE CHECK_FAR_FIELD

      SUBROUTINE VORCALC(VINIT,NPAN,ITER,NPW,NSYM,NGRND)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/VG1/A(8000,8000),A_ORIGINAL(8000,8000)
      COMMON/VG2/B(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/GVV/XP(8000),IPIV(8000)

      REAL :: NORMAL(3), QINF(3), VEL_IND(3)
      INTEGER :: I, J, SYM, GRND
      LOGICAL :: FAR


      QINF  = (/VINIT, 0.0, 0.0/)

      IF(ITER.EQ.0) THEN
         A = 0.0
         B = 0.0
         DO I = 1, NPAN
            PX = GX(I); PY = GY(I); PZ = GZ(I)
            NORMAL = (/ANX(I), ANY(I), ANZ(I)/)
            

            B(I) = - DOT_PRODUCT(QINF, NORMAL)
            
            DO J = 1, NPAN

               X1=X(IC(J,1)); Y1=Y(IC(J,1)); Z1=Z(IC(J,1))
               X2=X(IC(J,2)); Y2=Y(IC(J,2)); Z2=Z(IC(J,2))
               X3=X(IC(J,3)); Y3=Y(IC(J,3)); Z3=Z(IC(J,3))
               X4=X(IC(J,4)); Y4=Y(IC(J,4)); Z4=Z(IC(J,4))

               U = 0.0; V = 0.0; W = 0.0

               ! Check if the point is in the far field
               CALL CHECK_FAR_FIELD(PX, PY, PZ, X1, Y1, Z1,
     1                                          X2, Y2, Z2, 
     2                                          X3, Y3, Z3, 
     3                                          X4, Y4, Z4, FAR)
               IF (FAR) CYCLE

               DO SYM = 0, NSYM
                  DO GRND = 0, NGRND
                     CALL VORTEX_RING(PX, PY, PZ,
     1                                X1, Y1, Z1,
     2                                X2, Y2, Z2,
     3                                X3, Y3, Z3,
     4                                X4, Y4, Z4,
     5                           1.0, UI, VI, WI, SYM, GRND)
                  U = U + UI
                  V = V + VI
                  W = W + WI
                  END DO
               END DO

               A(I,J) = DOT_PRODUCT((/U, V, W/), NORMAL)
            END DO
         END DO
      ELSE
         DO I=1,NPAN
            PX = GX(I); PY = GY(I); PZ = GZ(I)
            NORMAL = (/ANX(I), ANY(I), ANZ(I)/)

            U = 0.0; V = 0.0; W = 0.0

            CALL VELWAK(ITER,NPW,NSYM,NGRND,PX,PY,PZ,U,V,W)

            VEL_IND = (/U, V, W/)

            B(I) = - DOT_PRODUCT(QINF + VEL_IND, NORMAL)
         END DO
      END IF

      CALL SOLVE(ITER,NPAN)
      END SUBROUTINE VORCALC



      SUBROUTINE VELWAK(ITER,NPW,NSYM,NGRND,GX,GY,GZ,U,V,W)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/VW/CW(100,8000),ICW(8000,2)
      COMMON/GW/WX(100,8000),WY(100,8000),WZ(100,8000)

      INTEGER :: SYM, GRND
      LOGICAL :: FAR

      U = 0.0; V = 0.0; W = 0.0

      DO IK=1,ITER
         DO JK=1,NPW
            NA=0
            X1=WX(IK,ICW(JK,1))
            Y1=WY(IK,ICW(JK,1))
            Z1=WZ(IK,ICW(JK,1))
            X2=WX(IK,ICW(JK,2))
            Y2=WY(IK,ICW(JK,2))
            Z2=WZ(IK,ICW(JK,2))
            X3=WX(IK+1,ICW(JK,2))
            Y3=WY(IK+1,ICW(JK,2))
            Z3=WZ(IK+1,ICW(JK,2))
            X4=WX(IK+1,ICW(JK,1))
            Y4=WY(IK+1,ICW(JK,1))
            Z4=WZ(IK+1,ICW(JK,1))

            DU = 0.0; DV = 0.0; DW = 0.0

            ! Check if the point is in the far field
            CALL CHECK_FAR_FIELD(GX, GY, GZ, X1, Y1, Z1,
     1                                       X2, Y2, Z2, 
     2                                       X3, Y3, Z3, 
     3                                       X4, Y4, Z4, FAR)
            IF (FAR) CYCLE

            DO SYM = 0, NSYM
               DO GRND = 0, NGRND
                  CALL VORTEX_RING(GX, GY, GZ,
     1                             X1, Y1, Z1,
     2                             X2, Y2, Z2,
     3                             X3, Y3, Z3,
     4                             X4, Y4, Z4,
     5                  CW(IK,JK), UI, VI, WI, SYM, GRND)
                  DU = DU + UI
                  DV = DV + VI
                  DW = DW + WI
               END DO
            END DO

            U = U + DU
            V = V + DV
            W = W + DW
         END DO
      END DO
      END

      SUBROUTINE VELPAN(NPAN,NSYM,NGRND,GX,GY,GZ,U,V,W) 
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GVV/C(8000),IPIV(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)

      INTEGER :: SYM, GRND
      LOGICAL :: FAR

      U = 0.0; V = 0.0; W = 0.0


      DO IK=1,NPAN

         X1=X(IC(IK,1)); Y1=Y(IC(IK,1)); Z1=Z(IC(IK,1))
         X2=X(IC(IK,2)); Y2=Y(IC(IK,2)); Z2=Z(IC(IK,2))
         X3=X(IC(IK,3)); Y3=Y(IC(IK,3)); Z3=Z(IC(IK,3))
         X4=X(IC(IK,4)); Y4=Y(IC(IK,4)); Z4=Z(IC(IK,4))

         DU = 0.0; DV = 0.0; DW = 0.0

         ! Check if the point is in the far field
         CALL CHECK_FAR_FIELD(GX, GY, GZ, X1, Y1, Z1,
     1                                    X2, Y2, Z2, 
     2                                    X3, Y3, Z3, 
     3                                    X4, Y4, Z4, FAR)
         IF (FAR) CYCLE

         DO SYM = 0, NSYM
            DO GRND = 0, NGRND
               CALL VORTEX_RING(GX, GY, GZ,
     1                          X1, Y1, Z1,
     2                          X2, Y2, Z2,
     3                          X3, Y3, Z3,
     4                          X4, Y4, Z4,
     5                   C(IK), UI, VI, WI, SYM, GRND)
               DU = DU + UI
               DV = DV + VI
               DW = DW + WI
            END DO
         END DO
         U = U + DU
         V = V + DV
         W = W + DW
      END DO
      END

      SUBROUTINE WAKREL(ITER,NPAN,NGW,NPW,DT,NSYM,NGRND) 
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/ANPORT/DS(8000)
      COMMON/VW/CW(100,8000),ICW(8000,2)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GW/WX(100,8000),WY(100,8000),WZ(100,8000)
      COMMON/GVV/C(8000),IPIV(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/WIN/IPW(8000)

      DIMENSION TEMP_WX(100,500),TEMP_WY(100,500),TEMP_WZ(100,500)

      DO I=1,ITER  
         DO J=1,NGW
            TEMP_WX(I,J) = WX(I,J)
            TEMP_WY(I,J) = WY(I,J)
            TEMP_WZ(I,J) = WZ(I,J)

            XG=WX(I,J)
            YG=WY(I,J)
            ZG=WZ(I,J)

            CALL VELPAN(NPAN,    NSYM,NGRND,XG,YG,ZG,U1,V1,W1) 
            CALL VELWAK(ITER,NPW,NSYM,NGRND,XG,YG,ZG,U2,V2,W2)

            U = U1 + U2
            V = V1 + V2
            W = W1 + W2

            TEMP_WX(I,J) = TEMP_WX(I,J) + U*DT
            TEMP_WY(I,J) = TEMP_WY(I,J) + V*DT
            TEMP_WZ(I,J) = TEMP_WZ(I,J) + W*DT
         END DO
      END DO
      DO I=1,ITER
         DO J=1,NGW
            WX(I,J) = TEMP_WX(I,J)
            WY(I,J) = TEMP_WY(I,J)
            WZ(I,J) = TEMP_WZ(I,J)
         END DO
      END DO
      END

          SUBROUTINE WAKE(ITER, VINIT, DT, NPAN, NPW, NGW, NGRID)
      !
      !=======================================================================
      ! == WAKE - Modern Refactored Version ==
      !
      ! Purpose:
      !   This subroutine generates and evolves the computational wake shed
      !   from a body defined by panels. It performs three main tasks:
      !   1. Identifies the "trailing edges" of the body.
      !   2. Creates a new layer of wake elements at these trailing edges,
      !      calculating their strength (circulation).
      !   3. Convects the entire wake system (new and old elements)
      !      downstream based on the free-stream velocity.
      !
      ! Modernization Notes:
      !   - All GOTO statements have been removed and replaced with structured
      !     loops (DO/END DO), conditional blocks (IF/THEN/ELSE/END IF),
      !     and loop control statements (CYCLE, EXIT).
      !   - Added comments to explain the logic and physical meaning.
      !   - Code is indented for readability.
      !=======================================================================
      IMPLICIT NONE

      !-----------------------------------------------------------------------
      ! Argument Declarations
      !-----------------------------------------------------------------------
      INTEGER, INTENT(IN) :: ITER      ! Current iteration or time-step number
      REAL(KIND=4), INTENT(IN) :: VINIT   ! Initial velocity for convection (likely free-stream)
      REAL(KIND=4), INTENT(IN) :: DT      ! Time step size
      INTEGER, INTENT(IN) :: NPAN      ! Number of panels on the body
      INTEGER, INTENT(IN) :: NGRID     ! Number of grid points on the body
      INTEGER, INTENT(OUT) :: NPW       ! Number of wake panel elements (vortices)
      INTEGER, INTENT(OUT) :: NGW       ! Number of wake grid points

      REAL(KIND=4) :: XW,YW,ZW
      REAL(KIND=4) :: X,Y,Z
      REAL(KIND=4) :: CW, C
      INTEGER :: ICW, IPIV, IC, MARK, MARKW
      REAL(KIND=4) :: ANX, ANY, ANZ, GX, GY, GZ
      REAL(KIND=4) :: ATX, ATY, ATZ, ALX, ALY, ALZ
      REAL(KIND=4) :: DS
      REAL(KIND=4) :: X1X, Y1Y, Z1Z
      INTEGER :: NC
      INTEGER :: IPW, IPROPW, IPROP


      !-----------------------------------------------------------------------
      ! Common Block Data (Global Variables)
      ! These are used to share large arrays between different subroutines.
      !-----------------------------------------------------------------------
      ! Wake point coordinates (x,y,z) for each time step and wake point index
      COMMON/GW/XW(100,8000),YW(100,8000),ZW(100,8000)
      ! Body grid point coordinates (x,y,z)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      ! Wake element circulation and connectivity
      COMMON/VW/CW(100,8000),ICW(8000,2)
      ! Body panel circulation and pivot info
      COMMON/GVV/C(8000),IPIV(8000)
      ! Body panel connectivity and markers
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      ! Panel normals, centers, and tangent vectors
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),
     1GX(8000),GY(8000),GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),
     1ALX(8000),ALY(8000),ALZ(8000)
      COMMON/ANPORT/DS(8000)
      ! Wake point coordinates at the trailing edge
      COMMON/CORECT/ X1X(8000),Y1Y(8000),Z1Z(8000)
      ! Node index associated with each wake point
      COMMON/WAICOR/NC(8000)
      ! Wake element property flags
      COMMON/WIN/IPW(8000)
      COMMON/WINTER/IPROPW(8000)
      ! Body panel property flags
      COMMON/PPG/IPROP(8000)

      !-----------------------------------------------------------------------
      ! Local Variables
      !-----------------------------------------------------------------------
      INTEGER :: I, J, K, L, M, LL, KL
      INTEGER :: MK, MKK, K1, K2, KK1, KK2
      INTEGER :: IT1, II
      INTEGER :: ADJACENT_PANEL_IDX
      INTEGER :: ICHECK(8000) ! Flag to prevent processing a panel twice
      LOGICAL :: IS_TRAILING_EDGE
      REAL(KIND=4) :: CIRCULATION_MOD

      !=======================================================================
      ! STEP 1: INITIALIZATION
      !=======================================================================

      ! Initialize counters for new wake elements and points
      NPW = 0 ! Number of Panel Wake elements
      II = 0  ! Counter for new wake grid points

      ! The first set of wake coordinates (attached to the body) are stored
      ! at index ITER+1. We will convect them to index ITER later.
      IT1 = ITER + 1

      ! Initialize marker arrays.
      ! MARKW stores the index of the wake grid point associated with a body grid point.
      ! A value of 0 means the body point is not yet part of the wake.
      DO I = 1, NGRID
      MARKW(I) = 0
      END DO

      ! ICHECK flags which panels have already been accounted for (as an adjacent panel).
      DO I = 1, NPAN
      ICHECK(I) = 0
      END DO

      !=======================================================================
      ! STEP 2: FIND TRAILING EDGES AND CREATE NEW WAKE ELEMENTS
      !
      ! This is the most complex part. We loop through all panels and their
      ! edges. If an edge is a "trailing edge" and hasn't been processed,
      ! we create a new wake element (a vortex segment) shedding from it.
      !=======================================================================

      ! --- Primary Loop to find Trailing Edges ---
      main_shedding_loop: DO J = 1, NPAN
      ! If this panel was already processed as an adjacent panel, skip it.
      IF (ICHECK(J) == 1) CYCLE main_shedding_loop

      ! Determine if the panel is a triangle (3 vertices) or a quadrilateral (4).
      IF (IC(J,3) == IC(J,4)) THEN
      MK = 3
      ELSE
      MK = 4
      END IF

      ! Loop through all edges of the current panel 'J'.
      edge_loop_1: DO K = 1, MK
      ! Define the start (K1) and end (K2) vertices of the current edge.
      K1 = K
      K2 = K + 1
      IF (K == MK) K2 = 1 ! Handle the last edge that wraps around.

      ! --- Condition to check if this is a trailing edge ---
      ! Condition 1: Both vertices of the edge must be marked as part of the trailing edge line.
      ! Condition 2: A wake element must not have already been created from this edge.
      IS_TRAILING_EDGE = (MARK(IC(J,K1)) /= 0 .AND. 
     1 MARK(IC(J,K2)) /= 0) .AND. 
     2 (MARKW(IC(J,K1)) == 0 .OR. MARKW(IC(J,K2)) == 0)

      IF (IS_TRAILING_EDGE) THEN
      ! We found a valid trailing edge to shed a wake from.
      ! Now, perform the shedding procedure for this edge.

      ! --- Find the panel on the other side of this trailing edge ---
      ADJACENT_PANEL_IDX = 0
      search_adjacent_loop: DO L = J + 1, NPAN
      ! Determine if panel L is a triangle or quad.
      IF (IC(L,3) == IC(L,4)) THEN
      MKK = 3
      ELSE
      MKK = 4
      END IF
      KK1 = 0
      KK2 = 0
      ! Check if panel L shares the same two vertices K1 and K2.
      DO M = 1, MKK
      IF (IC(L,M) == IC(J,K1)) KK1 = M
      IF (IC(L,M) == IC(J,K2)) KK2 = M
      END DO

      ! If both vertices were found, we've found the adjacent panel.
      IF (KK1 /= 0 .AND. KK2 /= 0) THEN
      ADJACENT_PANEL_IDX = L
      ICHECK(ADJACENT_PANEL_IDX) = 1 ! Mark it as processed.
      EXIT search_adjacent_loop
      END IF
      END DO search_adjacent_loop


      ! --- Create wake grid points at the vertices of the trailing edge (if they don't exist) ---
      ! For the first vertex (K1)
      IF (MARKW(IC(J,K1)) == 0) THEN
      II = II + 1 ! Increment total wake point count
      MARKW(IC(J,K1)) = II ! Link body point to wake point
      IPW(II) = 1
      IF (IPROP(J) == 2) IPW(II) = 2
      ! Store initial coordinates and node index
      X1X(II) = X(IC(J,K1))
      Y1Y(II) = Y(IC(J,K1))
      Z1Z(II) = Z(IC(J,K1))
      NC(II)  = IC(J,K1)
      ! Place the new wake point at the trailing edge for the first step
      XW(IT1,II) = X(IC(J,K1))
      YW(IT1,II) = Y(IC(J,K1))
      ZW(IT1,II) = Z(IC(J,K1))
      END IF

      ! For the second vertex (K2)
      IF (MARKW(IC(J,K2)) == 0) THEN
      II = II + 1 ! Increment total wake point count
      MARKW(IC(J,K2)) = II ! Link body point to wake point
      IPW(II) = 1
      IF (IPROP(J) == 2) IPW(II) = 2
      ! Store initial coordinates and node index
      X1X(II) = X(IC(J,K2))
      Y1Y(II) = Y(IC(J,K2))
      Z1Z(II) = Z(IC(J,K2))
      NC(II)  = IC(J,K2)
      ! Place the new wake point at the trailing edge for the first step
      XW(IT1,II) = X(IC(J,K2))
      YW(IT1,II) = Y(IC(J,K2))
      ZW(IT1,II) = Z(IC(J,K2))
      END IF

      ! --- Create the new wake element and calculate its circulation ---
      NPW = NPW + 1 ! Increment wake element count

      ! The strength of the shed wake vortex is based on the circulation
      ! of the panels meeting at the trailing edge (Kelvin's Theorem).
      CW(ITER,NPW) = C(J)
      IF ((K2 - K1) == (MK - 1)) CW(ITER,NPW) = -C(J)

      ! Adjust circulation based on the adjacent panel's contribution
      IF (ADJACENT_PANEL_IDX /= 0) THEN
      ! This logic adjusts the circulation strength based on the relative
      ! orientation of the edge in the two adjacent panels.
      IF ((KK2 < KK1) .AND. ((KK1 - KK2) == 1)) THEN
         CIRCULATION_MOD = -C(ADJACENT_PANEL_IDX)
      ELSE IF ((KK2 < KK1) .AND. ((KK1 - KK2) == (MKK - 1))) THEN
         CIRCULATION_MOD = +C(ADJACENT_PANEL_IDX)
      ELSE IF ((KK2 > KK1) .AND. ((KK2 - KK1) == 1)) THEN
         CIRCULATION_MOD = +C(ADJACENT_PANEL_IDX)
      ELSE IF ((KK2 > KK1) .AND. ((KK2 - KK1) == (MKK - 1))) THEN
         CIRCULATION_MOD = -C(ADJACENT_PANEL_IDX)
      ELSE
         CIRCULATION_MOD = 0.0
      END IF
      CW(ITER,NPW) = CW(ITER,NPW) + CIRCULATION_MOD
      END IF

      ! Store the connectivity of the new wake element
      ICW(NPW,1) = MARKW(IC(J,K1))
      ICW(NPW,2) = MARKW(IC(J,K2))

      ! Store property flag for the new wake element
      IPROPW(NPW) = 0
      IF (IPROP(J) == 2) IPROPW(NPW) = 1

      ! Since we've processed an edge for this panel, we can move to the next panel.
      EXIT edge_loop_1
      END IF
      END DO edge_loop_1
      END DO main_shedding_loop

      ! Set the total number of wake grid points created
      NGW = II

      !=======================================================================
      ! STEP 3: SECONDARY SWEEP FOR INTERNAL WAKE EDGES
      !
      ! This loop handles more complex cases, like wing-body junctions, where
      ! an edge might connect two existing wake points but wasn't created as a
      ! wake element in the first pass.
      !=======================================================================
      secondary_sweep_loop: DO I = 1, NPAN
      ! Skip panels that were already handled as adjacent panels
      IF (ICHECK(I) == 1) CYCLE secondary_sweep_loop

      IF (IC(I,3) == IC(I,4)) THEN; MK = 3; ELSE; MK = 4; END IF

      edge_loop_2: DO K = 1, MK
      K1 = K
      K2 = K + 1
      IF (K == MK) K2 = 1

      ! Check if this edge connects two points that are ALREADY part of the wake
      IF (MARKW(IC(I,K1)) * MARKW(IC(I,K2)) /= 0) THEN
      ! Now, check if this wake connection already exists
      DO L = 1, NPW
      IF ((ICW(L,1) == MARKW(IC(I,K1))) .AND. 
     1 (ICW(L,2) == MARKW(IC(I,K2)))) THEN
      CYCLE edge_loop_2 ! It exists, so skip to the next edge
      END IF
      END DO

      ! If we are here, the connection does NOT exist, so we must create it.
      ! This logic is identical to the main loop's creation process.
      NPW = NPW + 1

      ! Find the adjacent panel
      ADJACENT_PANEL_IDX = 0
      find_adjacent_2: DO LL = I + 1, NPAN
      IF (IC(LL,3) == IC(LL,4)) THEN; MKK = 3; ELSE; MKK = 4; END IF
      KK1 = 0; KK2 = 0
      DO M = 1, MKK
      IF (IC(LL,M) == IC(I,K1)) KK1 = M
      IF (IC(LL,M) == IC(I,K2)) KK2 = M
      END DO
      IF (KK1 /= 0 .AND. KK2 /= 0) THEN
      ADJACENT_PANEL_IDX = LL
      ICHECK(ADJACENT_PANEL_IDX) = 1
      EXIT find_adjacent_2
      END IF
      END DO find_adjacent_2

      ! Calculate circulation
      CW(ITER,NPW) = C(I)
      IF ((K2 - K1) == (MK - 1)) THEN
         CW(ITER,NPW) = -C(I)
      END IF
      IF (ADJACENT_PANEL_IDX /= 0) THEN
      IF ((KK2 < KK1) .AND. ((KK1 - KK2) == 1)) THEN
         CIRCULATION_MOD = -C(ADJACENT_PANEL_IDX)
      ELSE IF ((KK2 < KK1) .AND. ((KK1 - KK2) == (MKK - 1))) THEN
         CIRCULATION_MOD = +C(ADJACENT_PANEL_IDX)
      ELSE IF ((KK2 > KK1) .AND. ((KK2 - KK1) == 1)) THEN
         CIRCULATION_MOD = +C(ADJACENT_PANEL_IDX)
      ELSE IF ((KK2 > KK1) .AND. ((KK2 - KK1) == (MKK - 1))) THEN
         CIRCULATION_MOD = -C(ADJACENT_PANEL_IDX)
      ELSE
         CIRCULATION_MOD = 0.0
      END IF
      CW(ITER,NPW) = CW(ITER,NPW) + CIRCULATION_MOD
      END IF

      ! Store connectivity and properties
      ICW(NPW,1) = MARKW(IC(I,K1))
      ICW(NPW,2) = MARKW(IC(I,K2))
      IPROPW(NPW) = 0
      IF (IPROP(I) == 2) IPROPW(NPW) = 1
      END IF
      END DO edge_loop_2
      END DO secondary_sweep_loop

      !=======================================================================
      ! STEP 4: CONVECT THE ENTIRE WAKE
      !
      ! Move all wake points (new and old) downstream by VINIT * DT.
      !=======================================================================

      ! Convect the brand new wake points from the trailing edge (at time ITER)
      ! to their first position in the wake.
      DO KL = 1, NGW
      XW(ITER,KL) = X1X(KL) + VINIT*DT
      YW(ITER,KL) = Y1Y(KL)
      ZW(ITER,KL) = Z1Z(KL)
      END DO

      ! Convect all previously existing wake points from older time steps.
      IF (ITER > 1) THEN
      DO I = 1, ITER - 1
      DO J = 1, NGW
      XW(I,J) = XW(I,J) + VINIT*DT
      ! Y and Z convection is currently zero in this model
      YW(I,J) = YW(I,J)
      ZW(I,J) = ZW(I,J)
      END DO
      END DO
      END IF

      RETURN
      END SUBROUTINE WAKE
      
      SUBROUTINE WAKE_OLD(ITER,VINIT,DT,NPAN,NPW,NGW,NGRID)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/GW/XW(100,8000),YW(100,8000),ZW(100,8000)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/VW/CW(100,8000),ICW(8000,2)
      COMMON/GVV/C(8000),IPIV(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      DIMENSION ICHECK(8000)
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),ALX(8000),ALY(8000),
     1ALZ(8000)
      COMMON/ANPORT/DS(8000)
      COMMON/CORECT/ X1X(8000),Y1Y(8000),Z1Z(8000)
      COMMON/WAICOR/NC(8000)
      COMMON/WIN/IPW(8000)
      COMMON/WINTER/IPROPW(8000)
      COMMON/PPG/IPROP(8000)
      DO 5 I=1,NGRID
      MARKW(I)=0
    5 CONTINUE
      DO 7 I=1,NPAN
      ICHECK(I)=0
    7 CONTINUE
      IT1=ITER+1
      II=0
      NPW=0
      DO 20 J=1,NPAN
      IF (ICHECK(J).EQ.1) GO TO 20
      MK=4
      IF(IC(J,3).EQ.IC(J,4)) MK=3
      DO 10 K=1,MK
      K1=K
      K2=K+1
      IF (K.NE.MK) GO TO 11
      K1=1
      K2=MK
   11 IF((MARK(IC(J,K1)).NE.0).AND.(MARK(IC(J,K2)).NE.0)) GO TO 21
      GOTO 10
   21 IF((MARKW(IC(J,K1)).NE.0).AND.(MARKW(IC(J,K2)).NE.0)) GOTO 10
      GOTO 31
   10 CONTINUE
      GO TO 20
   31 DO 17 L=J+1,NPAN
      LPAN=0
      KK1=0
      KK2=0
      MKK=4
      IF(IC(L,3).EQ.IC(L,4)) MKK=3
      DO 16 M=1,MKK
      IF (IC(L,M).EQ.IC(J,K1)) KK1=M
      IF (IC(L,M).EQ.IC(J,K2)) KK2=M
   16 CONTINUE
      IF ((KK1*KK2).EQ.0) GOTO 17
      LPAN=L
      ICHECK(LPAN)=1
      GOTO 18
   17 CONTINUE
   18 CONTINUE
      IF(MARKW(IC(J,K1)).NE.0) GO TO 15
      II=II+1
      IPW(II)=1
      IF(IPROP(J).EQ.2) IPW(II)=2
      MARKW(IC(J,K1))=II
      X1X(II)=X(IC(J,K1))
      Y1Y(II)=Y(IC(J,K1))
      Z1Z(II)=Z(IC(J,K1))
      NC(II)=IC(J,K1)
      XW(IT1,II)=X(IC(J,K1))
      YW(IT1,II)=Y(IC(J,K1))
      ZW(IT1,II)=Z(IC(J,K1))
   15 IF(MARKW(IC(J,K2)).NE.0) GO TO 19
      II=II+1
      IPW(II)=1
      IF(IPROP(J).EQ.2) IPW(II)=2
      MARKW(IC(J,K2))=II
      X1X(II)=X(IC(J,K2))
      Y1Y(II)=Y(IC(J,K2))
      Z1Z(II)=Z(IC(J,K2))
      NC(II)=IC(J,K2)
      XW(IT1,II)=X(IC(J,K2))
      YW(IT1,II)=Y(IC(J,K2))
      ZW(IT1,II)=Z(IC(J,K2))
   19 NPW=NPW+1
      CW(ITER,NPW)=C(J)
      IF ((K2-K1).EQ.(MK-1)) CW(ITER,NPW)=-CW(ITER,NPW)
      IF (LPAN.EQ.0) GOTO 90
      IF ((KK2.LT.KK1).AND.((KK1-KK2).EQ.1)) CW(ITER,NPW)=CW(ITER,NPW)
     *-C(LPAN)
      IF ((KK2.LT.KK1).AND.((KK1-KK2).EQ.(MKK-1))) CW(ITER,NPW)=
     *CW(ITER,NPW)+C(LPAN)
      IF ((KK2.GT.KK1).AND.((KK2-KK1).EQ.1)) CW(ITER,NPW)=CW(ITER,NPW)
     *+C(LPAN)
      IF ((KK2.GT.KK1).AND.((KK2-KK1).EQ.(MKK-1))) CW(ITER,NPW)=
     *CW(ITER,NPW)-C(LPAN)
   90 CONTINUE
      ICW(NPW,1)=MARKW(IC(J,K1))
      ICW(NPW,2)=MARKW(IC(J,K2))
      IPROPW(NPW)=0
      IF(IPROP(J).EQ.2) IPROPW(NPW)=1
   20 CONTINUE
      NGW=II
      DO 30 I=1,NPAN
      IF (ICHECK(I).EQ.1) GO TO 30
      MK=4
      IF(IC(I,3).EQ.IC(I,4)) MK=3
      DO 40 K=1,MK
      K1=K
      K2=K+1
      IF (K.NE.MK) GO TO 35
      K1=1
      K2=MK
   35 IF (MARKW(IC(I,K1))*MARKW(IC(I,K2)).NE.0) GO TO 38
      GOTO 40
   38 DO 37 L=1,NPW
      IF ((ICW(L,1).EQ.MARKW(IC(I,K1))).AND.(ICW(L,2).EQ.MARKW
     1(IC(I,K2)))) GO TO 40
   37 CONTINUE
      NPW=NPW+1
      DO 117 LL=I+1,NPAN
      LPAN=0
      KK1=0
      KK2=0
      MKK=4
      IF(IC(LL,3).EQ.IC(LL,4)) MKK=3
      DO 116 M=1,MKK
      IF (IC(LL,M).EQ.IC(I,K1)) KK1=M
      IF (IC(LL,M).EQ.IC(I,K2)) KK2=M
  116 CONTINUE
      IF (KK1*KK2.EQ.0) GOTO 117
      LPAN=LL
      ICHECK(LPAN)=1
      GOTO 118
  117 CONTINUE
  118 CONTINUE
      CW(ITER,NPW)=C(I)
      IF ((K2-K1).EQ.(MK-1)) CW(ITER,NPW)=-CW(ITER,NPW)
      IF (LPAN.EQ.0) GOTO 190
      IF ((KK2.LT.KK1).AND.((KK1-KK2).EQ.1)) CW(ITER,NPW)=CW(ITER,NPW)
     *-C(LPAN)
      IF ((KK2.LT.KK1).AND.((KK1-KK2).EQ.(MKK-1))) CW(ITER,NPW)=
     *CW(ITER,NPW)+C(LPAN)
      IF ((KK2.GT.KK1).AND.((KK2-KK1).EQ.1)) CW(ITER,NPW)=CW(ITER,NPW)
     *+C(LPAN)
      IF ((KK2.GT.KK1).AND.((KK2-KK1).EQ.(MKK-1))) CW(ITER,NPW)=
     *CW(ITER,NPW)-C(LPAN)
  190 CONTINUE
      ICW(NPW,1)=MARKW(IC(I,K1))
      ICW(NPW,2)=MARKW(IC(I,K2))
      IPROPW(NPW)=0
      IF(IPROP(I).EQ.2) IPROPW(NPW)=1
   40 CONTINUE
   30 CONTINUE
      DO 121 KL=1,NGW
      X1=X1X(KL)
      Y1=Y1Y(KL)
      Z1=Z1Z(KL)
      XW(ITER,KL)=X1+VINIT*DT
      YW(ITER,KL)=Y1
      ZW(ITER,KL)=Z1
  121 CONTINUE
C      CALL IWCOR(ITER,NGW,NPAN)
      IF(ITER.EQ.1) GO TO 124
      DO 123 I=1,ITER-1
      DO 122 J=1,NGW
      XW(I,J)=XW(I,J)+VINIT*DT
      YW(I,J)=YW(I,J)
      ZW(I,J)=ZW(I,J)
  122 CONTINUE
  123 CONTINUE
  124 CONTINUE
      RETURN
      END



 


      SUBROUTINE WAKCOR(NPAN,NGW,ITER)
      IMPLICIT REAL*4 (A-H,O-Z)
C     COMMON BLOCKS ARE ASSUMED TO BE THE SAME
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),ALX(8000),ALY(8000),
     1ALZ(8000)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/GW/XW(100,8000),YW(100,8000),ZW(100,8000)
      COMMON/ANPORT/DS(8000)
      COMMON/CORECT/ X1X(8000),Y1Y(8000),Z1Z(8000)
      COMMON/WAICOR/NC(8000)

      DO 130 J=1,NGW
      DO 129 I=1,ITER
          II=ITER+1-I
          XXW=XW(II,J)
          YYW=YW(II,J)
          ZZW=ZW(II,J)
          XXW1=XW(II+1,J)
          YYW1=YW(II+1,J)
          ZZW1=ZW(II+1,J)

C         ORIGINAL CALL:
         NCC=NC(J)
         CALL PENETR_OLD(NPAN,XXW,YYW,ZZW,XXW1,YYW1,ZZW1,NP1,NCC)

C         REPLACEMENT CALL:
C         The line segment is from (XXW1, YYW1, ZZW1) to (XXW, YYW, ZZW).
C         PENETR takes (start_point, end_point) and modifies the end_point.
C          CALL PENETR(NPAN,XXW1,YYW1,ZZW1,XXW,YYW,ZZW,NC(J))

          XW(II,J)=XXW
          YW(II,J)=YYW
          ZW(II,J)=ZZW
 129  CONTINUE
 130  CONTINUE
      RETURN
      END


      SUBROUTINE PENETR_DEMI_WORKING(NPAN,XA,YA,ZA,XB,YB,ZB,NCC)
         IMPLICIT REAL*4 (A-H,O-Z)
         COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
         COMMON/GAVV/X(8000),Y(8000),Z(8000)
         COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
         COMMON/ANPORT/DS(8000)

         REAL P(3), PA(3), PB(3), PC(3), PD(3)
         REAL AREA_1, AREA_2, AREA_3, AREA_4, PANEL_AREA
         REAL NORMAL(3), K
         REAL NOMINATOR, DENOMINATOR, TAU
         REAL RI(3), R1(3), RR(3)

         AB = SQRT((XB-XA)**2 + (YB-YA)**2 + (ZB-ZA)**2)


C        CHECK THE PANELS IF THEY ARE PENETRATED
         DO I = 1, NPAN


C           CHECK IF THE CENTER OF THE PANEL AND THE DISTANCE FROM PB IS LARGER THAN PA-PB LENGTH
            CP_TO_B = SQRT((GX(I)-XB)**2+(GY(I)-YB)**2+(GZ(I)-ZB)**2)

            IF (CP_TO_B > AB) CYCLE ! Skip this panel if the center is farther than the line segment length

C           CHECK IF THIS PANEL SHOULD BE SKIPPED
            DO J = 1, 4
               IF(IC(I,J).EQ.NCC) CYCLE ! CYCLE is modern Fortran for 'skip to next iteration'
            END DO


C           PLANE EQUATION BY THE NORMAL VECTOR
            NORMAL = (/ANX(I), ANY(I), ANZ(I)/)
C           PLANE EQUATION IS NORMAL(1)*X + NORMAL(2)*Y + NORMAL(3)*Z + K = 0
C           FIND K
            K = - (NORMAL(1)*GX(I) + NORMAL(2)*GY(I) + NORMAL(3)*GZ(I))
C           CHECK IF THE LINE A-B INTERSECTS THE PLANE
C           LINE EQUATION IS P = PA + TAU*(PB-PA)
C           WE FIND THE VALUE OF TAU WHERE THE INTERSECTION OCCURS
C           WE PLUG THE LINE EQUATION INTO THE PLANE EQUATION
            NOMINATOR   = NORMAL(1)*    XA  +
     1                    NORMAL(2)*    YA  +
     2                    NORMAL(3)*    ZA  +
     3                                   K   
            DENOMINATOR = NORMAL(1)*(XB-XA) +
     1                    NORMAL(2)*(YB-YA) +
     2                    NORMAL(3)*(ZB-ZA)

C           Avoid division by zero if line is parallel to the plane
            IF (ABS(DENOMINATOR) .LT. 1E-9) CYCLE


            TAU = NOMINATOR / DENOMINATOR
C           IF TAU IS BETWEEN 0 AND 1, THEN THE LINE INTERSECTS THE PLANE
            IF (TAU.GE.0.0 .AND. TAU.LE.1.0) THEN
C              THE INTERSECTION POINT IS
               P = PA + TAU*(PB-PA)

C              FIND IF P IS IN THE PANEL
               PA = (/X(IC(I,1)), Y(IC(I,1)), Z(IC(I,1))/)
               PB = (/X(IC(I,2)), Y(IC(I,2)), Z(IC(I,2))/)
               PC = (/X(IC(I,3)), Y(IC(I,3)), Z(IC(I,3))/)
               PD = (/X(IC(I,4)), Y(IC(I,4)), Z(IC(I,4))/)
C              FIND THE AREAS OF THE PANEL
               PANEL_AREA = AREA(PA, PB, PC) + AREA(PC, PD, PA)

C              FIND THE AREAS OF THE TRIANGLES FOR THE INTERSECTION
               AREA_1 = AREA(PA, PB, P)
               AREA_2 = AREA(PB, PC, P)
               AREA_3 = AREA(PC, PD, P)
               AREA_4 = AREA(PD, PA, P)
               SUM_AREA = AREA_1 + AREA_2 + AREA_3 + AREA_4
C              IF THE SUM OF THE AREAS OF THE TRIANGLES IS EQUAL TO THE
C              AREA OF THE PANEL, THEN THE POINT IS INSIDE THE PANEL
               IF (ABS(SUM_AREA - PANEL_AREA) .LT. 1E-6) THEN
C                  PRINT *, 'PENETRATION DETECTED IN PANEL:', I
C                  PRINT *, 'INTERSECTION POINT:', P
                  PB = P


C                  RESERVED FOR FUTURE USE
C                 REFLECT THE PA TO PB FROM THE SURFACE
C                 USING https://paulbourke.net/geometry/reflected/ NOTES ON REFLECTION
C                  RI = P - PA
C                  R1 = - RI
C                  RR = RI - 2.0 * NORMAL*(DOT_PRODUCT(RI, NORMAL))
C                 SO THE NEW PB IS THE REFLECTED POINT BUT THE LENGTH IS P-PB (WHAT'S LEFT BY THE PENETRATION)
C                  PB = P + (RR/NORM2(RR))*NORM2(PB-P)
C                  XB = PB(1)
C                  YB = PB(2)
C                  ZB = PB(3)
               END IF
            END IF
         END DO
      END SUBROUTINE

      FUNCTION AREA2(P1, P2, P3)
         IMPLICIT REAL*4 (A-H,O-Z)
         REAL P1(3), P2(3), P3(3)
         REAL AREA2

         REAL VECTOR1(3), VECTOR2(3), CROSS_PROD(3)

         VECTOR1 = P2 - P1
         VECTOR2 = P3 - P2

         CALL CROSS_PRODUCT(VECTOR1, VECTOR2, CROSS_PROD)

         AREA2 = 0.5*NORM2(CROSS_PROD)

      END FUNCTION





      SUBROUTINE PENETR_OLD(NPAN,XW,YW,ZW,XW1,YW1,ZW1,NP1,NCC)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),ALX(8000),ALY(8000),
     1ALZ(8000)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      DIMENSION XX(4),YY(4),ZZ(4),PX(4),PY(4),PV(4)
  124 INT=0
      DO 128 K=1,NPAN
      INTR=0
      IF(K.EQ.NP1) GO TO 131
      KKI=4
      IF(IC(K,3).EQ.IC(K,4)) KKI=3
      DO 123 IK=1,KKI
      IF(IC(K,IK).EQ.NCC) GO TO 131
  123 CONTINUE
      INTR=1                         
      PGX=XW-GX(K)
      PGY=YW-GY(K)
      PGZ=ZW-GZ(K)
      WT=PGX*ATX(K)+PGY*ATY(K)+PGZ*ATZ(K)
      WL=PGX*ALX(K)+PGY*ALY(K)+PGZ*ALZ(K)
      WN=PGX*ANX(K)+PGY*ANY(K)+PGZ*ANZ(K)
      PGX1=XW1-GX(K)
      PGY1=YW1-GY(K)
      PGZ1=ZW1-GZ(K)
      WT1=PGX1*ATX(K)+PGY1*ATY(K)+PGZ1*ATZ(K)
      WL1=PGX1*ALX(K)+PGY1*ALY(K)+PGZ1*ALZ(K)
      WN1=PGX1*ANX(K)+PGY1*ANY(K)+PGZ1*ANZ(K)
c      IF(WN1.LT.0.) GO TO 127
c      IF((WN1.GT.0.).AND.(WN.GT.0.)) GO TO 127
      IF((WN1*WN).GT.0.OR.WN1.EQ.WN) GO TO 127
      XC=(WN1*WT-WN*WT1)/(WN1-WN)
      YC=(WN*WL1-WN1*WL)/(WN-WN1)
      DO 125 L=1,KKI
      XX(L)=X(IC(K,L))
      YY(L)=Y(IC(K,L))
      ZZ(L)=Z(IC(K,L))
      XP=XX(L)-GX(K)
      YP=YY(L)-GY(K)
      ZP=ZZ(L)-GZ(K)
      PX(L)=XP*ATX(K)+YP*ATY(K)+ZP*ATZ(K)
      PY(L)=XP*ALX(K)+YP*ALY(K)+ZP*ALZ(K)
  125 CONTINUE
      DO 126 L=1,KKI
      LL=L+1
      IF(L.EQ.KKI) LL=1
      XS=PX(LL)-PX(L)
      YS=PY(LL)-PY(L)
      XS1=PX(L)-XC
      YS1=PY(L)-YC
      PV(L)=XS*YS1-YS*XS1
  126 CONTINUE
      DO 132 L=1,KKI
      LL=L+1
      IF(L.EQ.KKI) LL=1
      PR=PV(L)*PV(LL)
      IF(PR.LT.0.) GO TO 127
  132 CONTINUE
      X1=XW-XW1
      Y1=YW-YW1
      Z1=ZW-ZW1
      DIST=-X1*ANX(K)-Y1*ANY(K)-Z1*ANZ(K)
      XW=XW+DIST*ANX(K)
      YW=YW+DIST*ANY(K)
      ZW=ZW+DIST*ANZ(K)
      GO TO 131
  127 INTR=0
  131 INT=INT+INTR
  128 CONTINUE        
      IF(INT.NE.0) GO TO 124
      RETURN
      END
      
      
      SUBROUTINE WAKINT(NPW,ITER)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/GW/XW(100,8000),YW(100,8000),ZW(100,8000)
      COMMON/VW/CW(100,8000),ICW(8000,2)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/WINTER/IPROPW(8000)
      DIMENSION WX(100,8000),WY(100,8000),WZ(100,8000) 
      DO 9 I=1,ITER+1
      DO 5 J=1,NPW
      WX(I,ICW(J,1))=XW(I,ICW(J,1))
      WY(I,ICW(J,1))=YW(I,ICW(J,1))
      WZ(I,ICW(J,1))=ZW(I,ICW(J,1))
      WX(I,ICW(J,2))=XW(I,ICW(J,2))
      WY(I,ICW(J,2))=YW(I,ICW(J,2))
      WZ(I,ICW(J,2))=ZW(I,ICW(J,2))
    5 CONTINUE
    9 CONTINUE
      DO 30 II=1,ITER
      IK=ITER+1-II
      DO 20 I=1,NPW
      IF(IPROPW(I).NE.0) GO TO 20
      X1=WX(IK,ICW(I,1))   
      Y1=WY(IK,ICW(I,1)) 
      Z1=WZ(IK,ICW(I,1))   
      X2=WX(IK,ICW(I,2))   
      Y2=WY(IK,ICW(I,2))   
      Z2=WZ(IK,ICW(I,2))   
      X3=WX(IK+1,ICW(I,2))   
      Y3=WY(IK+1,ICW(I,2))   
      Z3=WZ(IK+1,ICW(I,2))   
      X4=WX(IK+1,ICW(I,1))   
      Y4=WY(IK+1,ICW(I,1))   
      Z4=WZ(IK+1,ICW(I,1)) 
      DO 12 IJ=1,ITER
      IJ1=ITER+1-IJ 
      DO 11 J=1,NPW
      IF(IPROPW(J).NE.1) GO TO 11
      XX1=WX(IJ1,ICW(J,1))   
      YY1=WY(IJ1,ICW(J,1))   
      ZZ1=WZ(IJ1,ICW(J,1))   
      XX2=WX(IJ1,ICW(J,2))   
      YY2=WY(IJ1,ICW(J,2))   
      ZZ2=WZ(IJ1,ICW(J,2))   
      XX3=WX(IJ1+1,ICW(J,2))   
      YY3=WY(IJ1+1,ICW(J,2))   
      ZZ3=WZ(IJ1+1,ICW(J,2))   
      XX4=WX(IJ1+1,ICW(J,1))   
      YY4=WY(IJ1+1,ICW(J,1))   
      ZZ4=WZ(IJ1+1,ICW(J,1)) 
      CALL CROSS(XX3,YY3,ZZ3,XX2,YY2,ZZ2,X1,Y1,Z1,X2,Y2,Z2,
     1X3,Y3,Z3,X4,Y4,Z4,ANEWXW2,ANEWYW2,ANEWZW2)
      CALL CROSS(XX4,YY4,ZZ4,XX1,YY1,ZZ1,X1,Y1,Z1,X2,Y2,Z2,
     1X3,Y3,Z3,X4,Y4,Z4,ANEWXW1,ANEWYW1,ANEWZW1)
      WX(IJ1,ICW(J,1))=ANEWXW1
      WY(IJ1,ICW(J,1))=ANEWYW1
      WZ(IJ1,ICW(J,1))=ANEWZW1
      WX(IJ1,ICW(J,2))=ANEWXW2
      WY(IJ1,ICW(J,2))=ANEWYW2
      WZ(IJ1,ICW(J,2))=ANEWZW2
   11 CONTINUE
   12 CONTINUE
   20 CONTINUE
   30 CONTINUE
      DO 50 JK=1,ITER
      DO 40 J=1,NPW
      IF(IPROPW(J).NE.1) GO TO 40
      XW(JK,ICW(J,1))=WX(JK,ICW(J,1))
      YW(JK,ICW(J,1))=WY(JK,ICW(J,1)) 
      ZW(JK,ICW(J,1))=WZ(JK,ICW(J,1))
      XW(JK,ICW(J,2))=WX(JK,ICW(J,2))
      YW(JK,ICW(J,2))=WY(JK,ICW(J,2)) 
      ZW(JK,ICW(J,2))=WZ(JK,ICW(J,2))
   40 CONTINUE
   50 CONTINUE
      RETURN
      END
      
      
      SUBROUTINE CROSS(XXA,YYA,ZZA,XXB,YYB,ZZB,X1,Y1,Z1,X2,Y2,Z2,
     1X3,Y3,Z3,X4,Y4,Z4,ANEWXW,ANEWYW,ANEWZW)
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION XI(4),YI(4),ZI(4),PR(4)
      ABX=XXB-XXA
      ABY=YYB-YYA
      ABZ=ZZB-ZZA
      ANORM=SQRT(ABX**2+ABY**2+ABZ**2)
      ANEWXW=XXB
      ANEWYW=YYB
      ANEWZW=ZZB
      XI(1)=X1
      YI(1)=Y1
      ZI(1)=Z1
      XI(2)=X2
      YI(2)=Y2
      ZI(2)=Z2
      XI(3)=X3
      YI(3)=Y3
      ZI(3)=Z3
      XI(4)=X4
      YI(4)=Y4
      ZI(4)=Z4
      DO 10 I=1,4
      II=I+1
      IF(I.EQ.4) II=1
      DAX1=XI(I)-XXA
      DAY1=YI(I)-YYA
      DAZ1=ZI(I)-ZZA
      DAX2=XI(II)-XXA
      DAY2=YI(II)-YYA
      DAZ2=ZI(II)-ZZA
      PRAX12=DAY1*DAZ2-DAZ1*DAY2
      PRAY12=DAZ1*DAX2-DAX1*DAZ2
      PRAZ12=DAX1*DAY2-DAY1*DAX2
      PR(I)=ABX*PRAX12+ABY*PRAY12+ABZ*PRAZ12
      IF(I.EQ.3.OR.I.EQ.4) PR(I)=-PR(I)
   10 CONTINUE
      DXA=X1-XXA
      DYA=Y1-YYA
      DZA=Z1-ZZA
      DXB=X1-XXB
      DYB=Y1-YYB
      DZB=Z1-ZZB
      PRA=ABX*DXA+ABY*DYA+ABZ*DZA
      PRB=ABX*DXB+ABY*DYB+ABZ*DZB
      IF((PRA*PRB).GT.0.) GO TO 20
      PR13=PR(1)*PR(3)
      PR24=PR(2)*PR(4)
      IF(PR13.GT.0.OR.PR24.GT.0.) GO TO 20
      DX1=X3-X1
      DY1=Y3-Y1
      DZ1=Z3-Z1
      DX2=X2-X4
      DY2=Y2-Y4
      DZ2=Z2-Z4
      PRX=DY1*DZ2-DZ1*DY2
      PRY=DZ1*DX2-DX1*DZ2
      PRZ=DX1*DY2-DY1*DX2
      WNORM=SQRT(PRX**2+PRY**2+PRZ**2)
      WNX=PRX/WNORM
      WNY=PRY/WNORM
      WNZ=PRZ/WNORM
      ABPR=WNX*ABX+WNY*ABY+WNZ*ABZ
      ANEWXW=XXB-WNX*ABPR
      ANEWYW=YYB-WNY*ABPR
      ANEWZW=ZZB-WNZ*ABPR
   20 CONTINUE
      RETURN
      END
      
     
    
      SUBROUTINE NEIBORG(NPAN,NGW)    
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/NEIB/KN(8000,8000)
      COMMON/WAICOR/NC(8000)
      DO 6 I=1,NPAN
      KKI=4
      IF(IC(I,3).EQ.IC(I,4)) KKI=3
      DO 5 IK=1,KKI
      KN(I,IC(I,IK))=0
    5 CONTINUE
    6 CONTINUE
      DO 30 L=1,NGW
      DO 20 I=1,NPAN
      KKI=4
      IF(IC(I,3).EQ.IC(I,4)) KKI=3
      DO 10 IK=1,KKI
      IF(IC(I,IK).NE.NC(L)) GO TO 10
      KN(I,IC(I,IK))=NC(L)
   10 CONTINUE
   20 CONTINUE
   30 CONTINUE
      RETURN
      END


      SUBROUTINE IWCOR(ITER,NGW,NPAN)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),ALX(8000),ALY(8000),
     1ALZ(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GW/XW(100,8000),YW(100,8000),ZW(100,8000)
      COMMON/WAICOR/NC(8000)
      COMMON/NEIB/KN(8000,8000)
      DIMENSION ICHECK(8000)
      DO 80 J=1,NGW
      DX=XW(ITER,J)-X(NC(J))
      DY=YW(ITER,J)-Y(NC(J))
      DZ=ZW(ITER,J)-Z(NC(J))
      DO 7 I=1,NPAN
      ICHECK(I)=0
      KKI=4
      IF(IC(I,3).EQ.IC(I,4)) KKI=3
      DO 5 IK=1,KKI
      IF(KN(I,IC(I,IK)).EQ.NC(J)) GO TO 6
    5 CONTINUE
      GO TO 7
    6 PR=DX*ANX(I)+DY*ANY(I)+DZ*ANZ(I)
      IF(PR.GT.0.) GO TO 80
      ICHECK(I)=1
    7 CONTINUE
      DO 70 I=1,NPAN
      IF(ICHECK(I).EQ.0) GO TO 70
      KKI=4
      IF(IC(I,3).EQ.IC(I,4)) KKI=3

      IF((IK-2).LT.0) GOTO 10
      IF((IK-2).EQ.0) GOTO 20
      IF((IK-2).GT.0) GOTO 30

   10 AX=X(IC(I,KKI))-X(IC(I,IK))
      AY=Y(IC(I,KKI))-Y(IC(I,IK))
      AZ=Z(IC(I,KKI))-Z(IC(I,IK))
      BX=X(IC(I,2))-X(IC(I,IK))
      BY=Y(IC(I,2))-Y(IC(I,IK))
      BZ=Z(IC(I,2))-Z(IC(I,IK))
      GO TO 60
   20 AX=X(IC(I,1))-X(IC(I,IK))
      AY=Y(IC(I,1))-Y(IC(I,IK))
      AZ=Z(IC(I,1))-Z(IC(I,IK))
      BX=X(IC(I,3))-X(IC(I,IK))
      BY=Y(IC(I,3))-Y(IC(I,IK))
      BZ=Z(IC(I,3))-Z(IC(I,IK))
      GO TO 60
   30 IF(KKI.NE.3) GO TO 40
      AX=X(IC(I,2))-X(IC(I,IK))
      AY=Y(IC(I,2))-Y(IC(I,IK))
      AZ=Z(IC(I,2))-Z(IC(I,IK))
      BX=X(IC(I,1))-X(IC(I,IK))
      BY=Y(IC(I,1))-Y(IC(I,IK))
      BZ=Z(IC(I,1))-Z(IC(I,IK))
      GO TO 60
   40 IF(IK.NE.3) GO TO 50
      AX=X(IC(I,2))-X(IC(I,IK))
      AY=Y(IC(I,2))-Y(IC(I,IK))
      AZ=Z(IC(I,2))-Z(IC(I,IK))
      BX=X(IC(I,4))-X(IC(I,IK))
      BY=Y(IC(I,4))-Y(IC(I,IK))
      BZ=Z(IC(I,4))-Z(IC(I,IK))
      GO TO 60
   50 AX=X(IC(I,3))-X(IC(I,IK))
      AY=Y(IC(I,3))-Y(IC(I,IK))
      AZ=Z(IC(I,3))-Z(IC(I,IK))
      BX=X(IC(I,1))-X(IC(I,IK))
      BY=Y(IC(I,1))-Y(IC(I,IK))
      BZ=Z(IC(I,1))-Z(IC(I,IK))
   60 PRADX=AY*DZ-AZ*DY
      PRADY=AZ*DX-AX*DZ
      PRADZ=AX*DY-AY*DX
      PRABX=AY*BZ-AZ*BY
      PRABY=AZ*BX-AX*BZ
      PRABZ=AX*BY-AY*BX
      PRODV=PRADX*PRABX+PRADY*PRABY+PRADZ*PRABZ
      IF(PRODV.LT.0.) GO TO 70
      IF(PR.EQ.0.) PR=-0.1
      XW(ITER,J)=XW(ITER,J)-1.1*PR*ANX(I)
      YW(ITER,J)=YW(ITER,J)-1.1*PR*ANY(I)
      ZW(ITER,J)=ZW(ITER,J)-1.1*PR*ANZ(I)
      NP1=I
      XX=XW(ITER,J)
      YY=YW(ITER,J)
      ZZ=ZW(ITER,J)
      XX1=X(NC(J))
      YY1=Y(NC(J))
      NCC=NC(J)
      CALL PENETR_OLD(NPAN,XX,YY,ZZ,XX1,YY1,ZZ1,NP1,NCC)
      XW(ITER,J)=XX
      YW(ITER,J)=YY
      ZW(ITER,J)=ZZ
   70 CONTINUE
   80 CONTINUE
      RETURN
      END

        SUBROUTINE PENETR(NPAN,XA,YA,ZA,XB,YB,ZB)
         IMPLICIT NONE


         INTEGER, INTENT(IN) :: NPAN
         REAL(4), INTENT(IN) :: XA, YA, ZA
         REAL(4), INTENT(INOUT) :: XB, YB, ZB

         REAL(4) :: ANX(8000), ANY(8000), ANZ(8000)
         REAL(4) :: GX(8000),   GY(8000),  GZ(8000)
         REAL(4) :: X(8000),     Y(8000),   Z(8000)
         INTEGER :: IC(8000,4), MARK(8000), MARKW(8000)
         REAL(4) :: DS(8000)

         INTEGER :: I
         INTEGER :: PENETRATED_ID(8000)
         INTEGER :: PENETR_COUNTER
         INTEGER :: MIN_DIST_ID
         INTEGER :: ID

         REAL(4) :: DIST_AB, CP_TO_A, CP_TO_B
         REAL(4) :: NORMAL(3), K, TAU
         REAL(4) :: NOMINATOR, DENOMINATOR
         REAL(4) :: P(3), PA(3), PB(3), PC(3), PD(3)
         REAL(4) :: PANEL_AREA,AREA_1,AREA_2,AREA_3,AREA_4,SUM_AREA
         REAL(4) :: DIST_TO_PANEL, MIN_DIST, AREA




         COMMON/AVV1/ANX,ANY,ANZ, GX, GY, GZ
         COMMON/GAVV/X,Y,Z
         COMMON/GRID/IC,MARK,MARKW
         COMMON/ANPORT/DS

C        WE NEED TO FIND THE NEAREST PANEL THAT GOT PENETRATED
C        A LINE SEGMENT FROM POINT A TO POINT B MAY PENETRATE MULTIPLE PANELS
C        WE WILL CHECK ALL PANELS AND FIND THE PANELS THAT ARE PENETRATED
C        WE ARE GOING TO CHOOSE THE ONE THAT IS CLOSEST TO THE POINT A
C        THE POINT A IS THE POINT WHERE THE LINE SEGMENT STARTS
C        THE POINT B IS THE POINT WHERE THE LINE SEGMENT ENDS

         PENETRATED_ID = 0
         PENETR_COUNTER = 0
C        LOOP OVER THE PANELS TO CHECK FOR PENETRATION
         DO I = 1, NPAN
            DIST_AB = SQRT((XB-XA)**2 + (YB-YA)**2 + (ZB-ZA)**2)
            DIST_AB = DIST_AB + 0.1*DIST_AB ! ADD A SMALL TOLERANCE TO AVOID FLOATING POINT ERRORS
C           IF THE CENTER OF THE PANEL IS FARTHER THAN THE LENGTH OF THE LINE SEGMENT, SKIP THIS PANEL
            CP_TO_A = SQRT((GX(I)-XA)**2+(GY(I)-YA)**2+(GZ(I)-ZA)**2)
            CP_TO_B = SQRT((GX(I)-XB)**2+(GY(I)-YB)**2+(GZ(I)-ZB)**2)
            IF (CP_TO_A > DIST_AB .AND. CP_TO_B > DIST_AB) CYCLE

C           FIND ALL THE PENETRATED PANELS
C           PLANE EQUATION BY THE NORMAL VECTOR
            NORMAL = (/ANX(I), ANY(I), ANZ(I)/)
C           PLANE EQUATION IS NORMAL(1)*X + NORMAL(2)*Y + NORMAL(3)*Z + K = 0
C           FIND K
            K = - (NORMAL(1)*GX(I) + NORMAL(2)*GY(I) + NORMAL(3)*GZ(I))
C           CHECK IF THE LINE A-B INTERSECTS THE PLANE
C           LINE EQUATION IS P = PA + TAU*(PB-PA)
C           WE FIND THE VALUE OF TAU WHERE THE INTERSECTION OCCURS
C           WE PLUG THE LINE EQUATION INTO THE PLANE EQUATION
            NOMINATOR   = NORMAL(1)*    XA  +
     1                    NORMAL(2)*    YA  +
     2                    NORMAL(3)*    ZA  +
     3                                   K   
            DENOMINATOR = NORMAL(1)*(XB-XA) +
     1                    NORMAL(2)*(YB-YA) +
     2                    NORMAL(3)*(ZB-ZA)

C           Avoid division by zero if line is parallel to the plane
            IF (ABS(DENOMINATOR) .LT. 1E-9) CYCLE


            TAU = NOMINATOR / DENOMINATOR
C           IF TAU IS BETWEEN 0 AND 1, THEN THE LINE INTERSECTS THE PLANE
            IF (TAU.GE.0.0 .AND. TAU.LE.1.0) THEN
C              THE INTERSECTION POINT IS
               P = PA + TAU*(PB-PA)

C              FIND IF P IS IN THE PANEL
               PA = (/X(IC(I,1)), Y(IC(I,1)), Z(IC(I,1))/)
               PB = (/X(IC(I,2)), Y(IC(I,2)), Z(IC(I,2))/)
               PC = (/X(IC(I,3)), Y(IC(I,3)), Z(IC(I,3))/)
               PD = (/X(IC(I,4)), Y(IC(I,4)), Z(IC(I,4))/)
C              FIND THE AREAS OF THE PANEL
               PANEL_AREA = AREA(PA, PB, PC) + AREA(PC, PD, PA)

C              FIND THE AREAS OF THE TRIANGLES FOR THE INTERSECTION
               AREA_1 = AREA(PA, PB, P)
               AREA_2 = AREA(PB, PC, P)
               AREA_3 = AREA(PC, PD, P)
               AREA_4 = AREA(PD, PA, P)
               SUM_AREA = AREA_1 + AREA_2 + AREA_3 + AREA_4
C              IF THE SUM OF THE AREAS OF THE TRIANGLES IS EQUAL TO THE
C              AREA OF THE PANEL, THEN THE POINT IS INSIDE THE PANEL
               IF (ABS(SUM_AREA - PANEL_AREA) .LT. 1E-6) THEN
                  PENETR_COUNTER = PENETR_COUNTER + 1
                  PENETRATED_ID(PENETR_COUNTER) = I
               END IF
            END IF
         END DO

C      LOOP OVER THE PENETRATED PANELS AND FIND THE CLOSEST ONE
         IF (PENETR_COUNTER > 0) THEN
            MIN_DIST = 1E10
            MIN_DIST_ID = 0
            DO I = 1, PENETR_COUNTER
               ID = PENETRATED_ID(I)
               DIST_TO_PANEL = SQRT((GX(ID)-XA)**2 + 
     1                              (GY(ID)-YA)**2 + 
     2                              (GZ(ID)-ZA)**2)
               IF (DIST_TO_PANEL < MIN_DIST) THEN
                  MIN_DIST = DIST_TO_PANEL
                  MIN_DIST_ID = ID
               END IF
            END DO

C         NOW MOVE THE PB TO THE CENTER OF THE PENETRATED PANEL
            IF (MIN_DIST_ID > 0) THEN
               XB = GX(MIN_DIST_ID) + 0.1*ANX(MIN_DIST_ID)
               YB = GY(MIN_DIST_ID) + 0.1*ANY(MIN_DIST_ID)
               ZB = GZ(MIN_DIST_ID) + 0.1*ANZ(MIN_DIST_ID)
            END IF
         END IF
      END SUBROUTINE

      FUNCTION AREA(P1, P2, P3)
         IMPLICIT REAL*4 (A-H,O-Z)
         REAL P1(3), P2(3), P3(3)
         REAL AREA

         REAL VECTOR1(3), VECTOR2(3), CROSS_PROD(3)

         VECTOR1 = P2 - P1
         VECTOR2 = P3 - P2

         CALL CROSS_PRODUCT(VECTOR1, VECTOR2, CROSS_PROD)

         AREA = 0.5*NORM2(CROSS_PROD)

      END FUNCTION

            subroutine print_run_settings(ALF,BET,GAM,VINIT,EPS,
     1DT,NSYM,NGRND,CGX,CGY,CGZ,WINGAREA,MAC)
         implicit none
         real(4) :: ALF, BET, GAM, VINIT, EPS, DT
         real(4) :: CGX, CGY, CGZ, WINGAREA,MAC
         real(4) :: PI
         integer :: NSYM, NGRND

         PI = 4.0*atan(1.0)

         write (*,'(a)') "--------------------------"
         write (*,'(a)') "Printing ROS Configuration"
         write (*,'(a)') "Angles  (deg)"
         write (*,'(a)') "Lengths (meters)"
         write (*,'(a)') "Max Iterations : 100"
         write (*,'(a)') "--------------------------"
         write (*,'(a)') "Angles"
         write (*,'(a,f10.3)') "   Yaw   : ", ALF*180/PI
         write (*,'(a,f10.3)') "   Pitch : ", BET*180/PI
         write (*,'(a,f10.3)') "   Roll  : ", GAM*180/PI
         write (*,'(a)') "--------------------------"
         write (*,'(a,f10.3)') "Wind Speed   : ", VINIT
         write (*,'(a,f10.6)') "Epsilon      : ", EPS
         write (*,'(a,f10.6)') "Time Step    : ", DT
         write (*,'(a)') "--------------------------"
         write (*,'(a)') "Indices"
         write (*,'(a,I1)') "   Symmetry      : ", NSYM
         write (*,'(a,I1)') "   Ground Effect : ", NGRND
         write (*,'(a)') "--------------------------"
         write (*,'(a,3f7.3)') "Center of Gravity  : ", CGX,CGY,CGZ
         write (*,'(a,3f7.3)') "Wing Surface (m^2) : ", WINGAREA
         write (*,'(a,3f7.3)') "Mean Aerod. Chord  : ", MAC
         write (*,'(a)') "--------------------------"
         write (*,'(a)') " "
         write (*,'(a)') "STARTING SOLVER"

         write (50,'(a)') "--------------------------"
         write (50,'(a)') "Printing ROS Configuration"
         write (50,'(a)') "Angles  (deg)"
         write (50,'(a)') "Lengths (meters)"
         write (50,'(a)') "Max Iterations : 100"
         write (50,'(a)') "--------------------------"
         write (50,'(a)') "Angles"
         write (50,'(a,f10.3)') "   Yaw   : ", ALF*180/PI
         write (50,'(a,f10.3)') "   Pitch : ", BET*180/PI
         write (50,'(a,f10.3)') "   Roll  : ", GAM*180/PI
         write (50,'(a)') "--------------------------"
         write (50,'(a,f10.3)') "Wind Speed   : ", VINIT
         write (50,'(a,f10.6)') "Epsilon      : ", EPS
         write (50,'(a,f10.6)') "Time Step    : ", DT
         write (50,'(a)') "--------------------------"
         write (50,'(a)') "Indices"
         write (50,'(a,I1)') "   Symmetry      : ", NSYM
         write (50,'(a,I1)') "   Ground Effect : ", NGRND
         write (50,'(a)') "--------------------------"
         write (50,'(a,3f7.3)') "Center of Gravity  : ", CGX,CGY,CGZ
         write (50,'(a,3f7.3)') "Wing Surface (m^2) : ", WINGAREA
         write (50,'(a,3f7.3)') "Mean Aerod. Chord  : ", MAC
         write (50,'(a)') "--------------------------"
         write (50,'(a)') " "
         write (50,'(a)') "STARTING SOLVER"

         
      end subroutine print_run_settings


      SUBROUTINE SOLVE(ITER,NPAN)
      IMPLICIT NONE

      INTEGER, PARAMETER :: PR = 4
      INTEGER, PARAMETER :: MAX_N = 8000
      INTEGER, INTENT(IN) :: ITER, NPAN
      INTEGER :: INFO


      REAL(PR) :: A,B, C, A_ORIGINAL, RHS(8000)
      INTEGER  :: IPIV


      COMMON/VG1/A(8000,8000),A_ORIGINAL(8000,8000)
      COMMON/VG2/B(8000)
      COMMON/GVV/C(8000),IPIV(8000)


      IF (ITER.EQ.0) THEN
      A_ORIGINAL = A
      WRITE( *,*) "LU FACTORIZATION"
      WRITE(50,*) "LU FACTORIZATION"

      CALL SGETRF(NPAN, NPAN, A, MAX_N, IPIV, INFO)
      IF (INFO.NE.0) THEN
      WRITE( *,*) "Error in LU factorization"
      WRITE(50,*) "Error in LU factorization"
      STOP
      END IF
      WRITE( *,*) "LU FACTORIZATION DONE"
      WRITE(50,*) "LU FACTORIZATION DONE"
      END IF

      
      C = B

      CALL SGETRS('N', NPAN, 1, A, MAX_N, IPIV, C, MAX_N, INFO)

      RETURN
      ! CHECK IF THE SOLUTION IS CORRECT
      IF (INFO.NE.0) THEN
      WRITE( *,*) "Error in LU solve"
      WRITE(50,*) "Error in LU solve"
      STOP
      ELSE
         RHS = MATMUL(A_ORIGINAL, C)
         WRITE( *,*) "LU solve A*C= B"
         WRITE( *,*) "RHS :"
         WRITE(*,'(10F15.5)') RHS(1:10)
         WRITE( *,*) "B   :"
         WRITE(*,'(10F15.5)') B(1:10)
         PRINT *

      END IF
      END SUBROUTINE SOLVE

            SUBROUTINE READ_SETTINGS(NPAN,NGRID,ALF,BET,GAM,
     1VINIT,EPS,DT,NSYM,INCH,NGRND,HFL,CGX,CGY,CGZ,WINGAREA,MAC)
            IMPLICIT NONE
            INTEGER, INTENT(OUT) :: NPAN, NGRID, NSYM, NGRND, INCH
            REAL(4), INTENT(OUT) :: ALF, BET, GAM
            REAL(4), INTENT(OUT) :: VINIT, EPS, DT
            REAL(4), INTENT(OUT) :: HFL
            REAL(4), INTENT(OUT) :: CGX, CGY, CGZ
            REAL(4), INTENT(OUT) :: WINGAREA, MAC

            OPEN(UNIT=9,FILE='DELTA.INP',STATUS='UNKNOWN')

            READ(9,'(I10)') NPAN
            READ(9,'(I10)') NGRID
            READ(9,'(F10.3)') ALF
            READ(9,'(F10.3)') BET
            READ(9,'(F10.3)') GAM
            READ(9,'(F10.3)') VINIT
            READ(9,'(F10.8)') EPS
            READ(9,'(F10.8)') DT
            READ(9,'(I10)') NSYM
            READ(9,'(I10)') INCH
            READ(9,'(I10)') NGRND
            READ(9,'(F10.3)') HFL
            READ(9,'(F10.3)') CGX
            READ(9,'(F10.3)') CGY
            READ(9,'(F10.3)') CGZ
            READ(9,'(F10.3)') WINGAREA
            READ(9,'(F10.3)') MAC

            CLOSE(9)
      END SUBROUTINE READ_SETTINGS

      SUBROUTINE GEOM_MODDED(NPAN,NGRID,ALF,BET,GAM,NSYM,NGRND,
     1HFL,CGX,CGY,CGZ,WINGAREA,MAC)
      IMPLICIT NONE

      INTEGER NPAN,NGRID,NSYM,NGRND,INCH
      REAL ALF,BET,GAM,HFL,CGX,CGY,CGZ,WINGAREA,MAC
      REAL PI

      REAL X(8000),Y(8000),Z(8000)
      REAL ANX(8000),ANY(8000),ANZ(8000)
      REAL GX(8000),GY(8000),GZ(8000)
      REAL DS(8000)
      INTEGER IC(8000,4),MARK(8000),MARKW(8000)
      INTEGER IPROP(8000)


      INTEGER I,J
      REAL, DIMENSION(3,3) :: ROT, INVROT
      REAL, DIMENSION(3)   :: XYZ, NORM_VEC
      REAL :: sina, cosa, sinb, cosb, sing, cosg
      

      COMMON/GAVV/X,Y,Z
      COMMON/GRID/IC,MARK,MARKW
      COMMON/PPG/IPROP
      COMMON/AVV1/ANX,ANY,ANZ,GX,GY,GZ
      COMMON/ANPORT/DS
      COMMON/GEOM/ROT,INVROT



      PI = 4.0*ATAN(1.0)

      OPEN(UNIT=7,FILE='DELTA.PAN',STATUS='UNKNOWN')
      OPEN(UNIT=8,FILE='DELTA.DAT',STATUS='UNKNOWN')

      OPEN(UNIT=30,FILE='WN.PAN',STATUS='UNKNOWN')
      OPEN(UNIT=40,FILE='WN.DAT',STATUS='UNKNOWN')

C=======================================================================
      IF (INCH.EQ.1) THEN
      CGX = CGX * 0.0254
      CGY = CGY * 0.0254
      CGZ = CGZ * 0.0254
      HFL = HFL * 0.0254
      WINGAREA = WINGAREA * 0.00064516
      MAC = MAC * 0.0254
      END IF
C=======================================================================
C     LOAD THE ORIGINAL POINT CLOUD DATA
      DO I=1,NGRID
C     READ THE POINTS GEOMETRY DATA      
         READ(8,110) J,X(I),Y(I),Z(I),MARK(I)
C        CONVERT THE GEOMETRY TO METERS
         IF(INCH.EQ.1) THEN
               X(I)=X(I)*0.0254
               Y(I)=Y(I)*0.0254
               Z(I)=Z(I)*0.0254
         END IF
C        ADD THE HFL TO THE Z COORDINATE
         Z(I) = Z(I) + HFL
      END DO
C     IF CGX,CGY,CGZ ARE ZERO THEN CALCULATE THE GEOMETRIC CENTER
      IF ((CGX.LT.1E-5).AND.(CGY.LT.1E-5).AND.(CGZ.LT.1E-5)) THEN
            CGX = (MAXVAL(X) + MINVAL(X)) / 4.
            CGY = (MAXVAL(Y) + MINVAL(Y)) / 2.
            CGZ = (MAXVAL(Z) + MINVAL(Z)) / 2.
            IF (NSYM.EQ.1) THEN
                  CGY = MINVAL(Y)
            END IF
      END IF

      GZ = GZ + HFL

C      PRINT *
C      PRINT *
C      PRINT *, "NORMAL VECTOR ARE FLIPPED, FIX IN GEOMETRY.FOR"
      DO I=1,NPAN
C     READ THE PANEL DATA
      READ(7,120) J,IC(I,1),IC(I,2),IC(I,3),IC(I,4),IPROP(I),
     1ANX(I),ANY(I),ANZ(I),GX(I),GY(I),GZ(I),DS(I)
     
      
      ANX(I) = ANX(I)
      ANY(I) = ANY(I)
      ANZ(I) = ANZ(I)

C     WRITE THE FINAL PANEL DATA (DOESN'T CHANGE BUT NEEDED FOR THE POST PROCESS TO WORK)      
      WRITE(30,130) I,IC(I,1),IC(I,2),IC(I,3),IC(I,4),IPROP(I)
      END DO

      GZ = GZ + HFL


C=======================================================================
C     CONVERT THE ANGLES TO RADIANS
      ALF=ALF*PI/180.
      BET=BET*PI/180.
      GAM=GAM*PI/180.
C     CREATE THE ROTATION MATRIX - ROT
      sina = sin(ALF); cosa = cos(ALF)
      sinb = sin(BET); cosb = cos(BET)
      sing = sin(GAM); cosg = cos(GAM)

      ROT(1,1) = cosa*cosb
      ROT(1,2) = cosa*sinb*sing-sina*cosg
      ROT(1,3) = cosa*sinb*cosg+sina*sing

      ROT(2,1) = sina*cosb
      ROT(2,2) = sina*sinb*sing+cosa*cosg
      ROT(2,3) = sina*sinb*cosg-cosa*sing

      ROT(3,1) = -sinb
      ROT(3,2) = cosb*sing
      ROT(3,3) = cosb*cosg

      IF ((ALF.LT.1.0E-5).AND.
     1    (BET.LT.1.0E-5).AND.
     2    (GAM.LT.1.0E-5)) THEN

            INVROT = ROT
            
      ELSE
            call matinv3(ROT,INVROT)
      END IF

c      PRINT *, "ROT"
c      print *, ROT(1,1), ROT(1,2), ROT(1,3)
c      print *, ROT(2,1), ROT(2,2), ROT(2,3)
c      print *, ROT(3,1), ROT(3,2), ROT(3,3)

c      print *, "INVROT"
c      print *, INVROT(1,1), INVROT(1,2), INVROT(1,3)
c      print *, INVROT(2,1), INVROT(2,2), INVROT(2,3)
c      print *, INVROT(3,1), INVROT(3,2), INVROT(3,3)
C=======================================================================
C      
C     ROTATE THE GEOMETRY AROUND THE CG OR THE GEOMETRIC CENTER
C
C     1. TRANSLATE THE GEOMETRY TO THE ORIGIN
C     2. ROTATE THE GEOMETRY
C     3. TRANSLATE THE GEOMETRY BACK TO THE ORIGINAL POSITION

C       TRANSLATE THE GEOMETRY TO THE ORIGIN
      X = X - CGX
      Y = Y - CGY
      Z = Z - CGZ

      GX = GX - CGX
      GY = GY - CGY
      GZ = GZ - CGZ
C       ROTATE THE GEOMETRY
      DO I=1,NGRID
      XYZ = (/X(I),Y(I),Z(I)/)
      XYZ = MATMUL(ROT, XYZ)
      X(I) = XYZ(1); Y(I) = XYZ(2); Z(I) = XYZ(3)
      END DO

      DO I = 1, NPAN
C     ROTATE THE GEOMETRIC CENTER
      XYZ = (/GX(I),GY(I),GZ(I)/)
      XYZ = MATMUL(ROT, XYZ)

      GX(I) = XYZ(1)
      GY(I) = XYZ(2)
      GZ(I) = XYZ(3)
C     ROTATE THE NORMAL VECTORS
      NORM_VEC = (/ANX(I),ANY(I),ANZ(I)/)
      
      NORM_VEC    = MATMUL(ROT, NORM_VEC)
      
      ANX(I) = NORM_VEC(1)
      ANY(I) = NORM_VEC(2)
      ANZ(I) = NORM_VEC(3)
      END DO
C       TRANSLATE THE GEOMETRY BACK TO THE ORIGINAL POSITION
      X = X + CGX
      Y = Y + CGY
      Z = Z + CGZ

      GX = GX + CGX
      GY = GY + CGY 
      GZ = GZ + CGZ 
C=======================================================================
C     WRITE THE FINAL GEOMETRY DATA
      DO I=1,NGRID
      WRITE(40,110) I,X(I),Y(I),Z(I),MARK(I)
      END DO
C=======================================================================
C     CLOSE THE FILES
      CLOSE(7)
      CLOSE(8)
      CLOSE(9)
      CLOSE(30)
      CLOSE(40)
C=======================================================================


  110 FORMAT(I10,3F15.5,I10)
  120 FORMAT(6I10,7F15.5)
  130 FORMAT(6I10)
      END SUBROUTINE GEOM_MODDED


      SUBROUTINE ANALGEO_MODDED(NPAN)
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),ALX(8000),ALY(8000),
     1ALZ(8000)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/ANPORT/DS(8000)
      DIMENSION vec_TEMP(3)

      DO I=1,NPAN

            XX = X(IC(I,1)) - GX(I)
            YY = Y(IC(I,1)) - GY(I)
            ZZ = Z(IC(I,1)) - GZ(I)


            ANORM=NORM2((/XX,YY,ZZ/))

            ALX(I)=XX/ANORM
            ALY(I)=YY/ANORM
            ALZ(I)=ZZ/ANORM

            CALL CROSS_PRODUCT((/ANX(I),ANY(I),ANZ(I)/),
     1                         (/ALX(I),ALY(I),ALZ(I)/),
     2                         vec_TEMP)
            ATX(I) = vec_TEMP(1)
            ATY(I) = vec_TEMP(2)
            ATZ(I) = vec_TEMP(3)

      END DO
      END SUBROUTINE ANALGEO_MODDED 


      SUBROUTINE CROSS_PRODUCT(a, b, c)
            REAL*4, DIMENSION(3), INTENT(IN)  :: a, b
            REAL*4, DIMENSION(3), INTENT(OUT) :: c
                      
            c(1) = a(2) * b(3) - a(3) * b(2)
            c(2) = a(3) * b(1) - a(1) * b(3)
            c(3) = a(1) * b(2) - a(2) * b(1)
      END SUBROUTINE CROSS_PRODUCT
          
      subroutine matinv3(A,B)
C     Performs a direct calculation of the inverse of a 3×3 matrix.
      integer, parameter    :: pr = 4
      real(pr), intent(in)  :: A(3,3)   !! Matrix
      real(pr) ,intent(out) :: B(3,3)   !! Inverse matrix
      real(pr)              :: detinv

      ! Calculate the inverse determinant of the matrix
      detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)
     1          - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)
     2          + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

      ! Calculate the inverse of the matrix
      B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
      B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
      B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
      B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
      B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
      B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
      B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
      B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
      B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))
      end subroutine matinv3


            SUBROUTINE CPAIP(ITER,NPAN,NPW,NSYM,VINIT,NGRND,CGX,CGY,CGZ,
     1WINGAREA,MAC,ALIFT,DRAG,SIDE,AMROLL,AMPITCH,AMYAW)
C GGX,GGY,GGZ: K.B. A/FOYS
      IMPLICIT REAL*4 (A-H,O-Z)
      COMMON/VVOR/XG,YG,ZG,X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,X4,Y4,Z4,VVX,
     1VVY,VVZ
      COMMON/AVV1/ANX(8000),ANY(8000),ANZ(8000),GX(8000),GY(8000),
     1GZ(8000)
      COMMON/AVV2/ATX(8000),ATY(8000),ATZ(8000),ALX(8000),ALY(8000),
     1ALZ(8000)
      COMMON/ANPORT/DS(8000)
      COMMON/VW/CW(100,8000),ICW(8000,2)
      COMMON/GAVV/X(8000),Y(8000),Z(8000)
      COMMON/GW/XW(100,8000),YW(100,8000),ZW(100,8000)
      COMMON/GVV/C(8000)
      COMMON/GRID/IC(8000,4),MARK(8000),MARKW(8000)
      COMMON/PPG/IPROP(8000)
      COMMON/GEOM/ROT(3,3)

      REAL MAC
      REAL vec_n(3), vec_l(3), vec_m(3), vec_VINIT(3)
      REAL vec_V_per(3)
      REAL Q_k_1(3), Q_k_2(3), Q_k(3) 
      REAL dF(3), F(3), r(3), M(3)
      REAL DRAG_VECTOR(3), SIDE_VECTOR(3), LIFT_VECTOR(3)
      REAL CG_GLOBAL(3), FORCES(3)

      RHO=1.225
      PATM=1.D+5
      DRAG=0.D0
      SIDE=0.D0
      ALIFT=0.D0
      AMROLL=0.D0
      AMPITCH=0.D0
      AMYAW=0.D0

      DRAG_VECTOR = (/1.0, 0.0, 0.0/)  ! Drag vector in the x-direction
      SIDE_VECTOR = (/0.0, 1.0, 0.0/)  ! Side vector in the y-direction
      LIFT_VECTOR = (/0.0, 0.0, 1.0/)  ! Lift vector in the z-direction

      DRAG_VECTOR = MATMUL(ROT, DRAG_VECTOR)
      SIDE_VECTOR = MATMUL(ROT, SIDE_VECTOR)
      LIFT_VECTOR = MATMUL(ROT, LIFT_VECTOR)

      OPEN(UNIT=60,FILE='CPK.DAT'  ,STATUS='UNKNOWN')
      OPEN(UNIT=61,FILE='LOADS.DAT'  ,STATUS='UNKNOWN')

      DO I=1,NPAN
         BIT = SQRT(DS(I))
         
         CPX = GX(I) + ANX(I)*BIT
         CPY = GY(I) + ANY(I)*BIT
         CPZ = GZ(I) + ANZ(I)*BIT

         CALL VELPAN(NPAN,    NSYM,NGRND,CPX,CPY,CPZ,U1,V1,W1)
         CALL VELWAK(ITER,NPW,NSYM,NGRND,CPX,CPY,CPZ,U2,V2,W2)

         U = U1 + U2
         V = V1 + V2
         W = W1 + W2
         
C	SET UP THE VECTORS
         vec_n = (/ANX(I), ANY(I), ANZ(I)/)
         vec_l = (/ALX(I), ALY(I), ALZ(I)/)
         vec_m = (/ATX(I), ATY(I), ATZ(I)/)

C	Vinf Vector
         vec_VINIT = (/VINIT, 0., 0./)
C	Calculate first part of Qk
         Q_k_11 = DOT_PRODUCT(vec_VINIT,vec_l)
         Q_k_12 = DOT_PRODUCT(vec_VINIT,vec_m)
         Q_k_13 = DOT_PRODUCT(vec_VINIT,vec_n)
         Q_k_1 = (/Q_k_11, Q_k_12, Q_k_13/)
C	Calculate the second part of Qk (Pertubation Velocity Vector)
         vec_V_per = (/U,V,W/)
         Q_k_21 = DOT_PRODUCT(vec_V_per,vec_l)
         Q_k_22 = DOT_PRODUCT(vec_V_per,vec_m)
         Q_k_23 = DOT_PRODUCT(vec_V_per,vec_n)
         Q_k_2 = (/Q_k_21, Q_k_22, Q_k_23/)
C	Calculate Q_k
         Q_k = Q_k_1 + Q_k_2
C	Calculate Cpk
         Cpk = 1 - (NORM2(Q_k)**2)/(NORM2(vec_VINIT)**2)
C     WRITE CPK TO CPK.DAT
         WRITE(60,'(I10,4F15.5)') I, GX(I), GY(I), GZ(I), Cpk
C	Calculate contribution in aerodynamic loads
         dF = - Cpk * (0.5*RHO*VINIT**2) * DS(I) * vec_n

C         P = (0.5 * RHO * VINIT**2)*Cpk + PATM
C         dF = -P*DS(I)*vec_n

         WRITE(61,'(I10,6F15.5)') I, GX(I), GY(I), GZ(I), 
     1                               dF(1), dF(2), dF(3)

         DRAG  = DRAG  + DOT_PRODUCT(dF, DRAG_VECTOR)
         SIDE  = SIDE  + DOT_PRODUCT(dF, SIDE_VECTOR)
         ALIFT = ALIFT + DOT_PRODUCT(dF, LIFT_VECTOR)


      END DO

      IF (NSYM .EQ. 1) THEN
         DRAG  = 2.0 * DRAG
         SIDE  = 0.0 * SIDE
         ALIFT = 2.0 * ALIFT
      END IF

      CG_GLOBAL = (/CGX, CGY, CGZ/)
      FORCES    = (/DRAG, SIDE, ALIFT/)
      
      CALL cross_product(CG_GLOBAL, FORCES, M)

      AMROLL   = M(1)
      AMPITCH  = M(2)
      AMYAW    = M(3)

      CLOSE(60)
      CLOSE(61)

      RETURN
      END SUBROUTINE CPAIP

      SUBROUTINE CDTDRAG(NSYM, VINIT, WINGAREA, MAC, CDT)
      IMPLICIT NONE

      ! Input and output variable declarations
      INTEGER, INTENT(IN) :: NSYM       ! Symmetry flag (1 for symmetric, otherwise 0)
      REAL(4), INTENT(IN) :: VINIT      ! Initial velocity
      REAL(4), INTENT(IN) :: WINGAREA   ! Wing area
      REAL(4), INTENT(IN) :: MAC        ! Mean aerodynamic chord
      REAL(4), INTENT(OUT) :: CDT       ! Calculated drag coefficient

      ! Local variables
      REAL(4) :: RE, CT, VISQ, DSURF, AL  ! RE: Reynolds number, CT: Drag coefficient, VISQ: Kinematic viscosity, DSURF: Surface area, AL: Characteristic length
      REAL(4), DIMENSION(8000) :: DS, X, Y, Z  ! Arrays for surface data and coordinates

      ! Common blocks to share data with other parts of the program
      COMMON/GAVV/X, Y, Z  ! Coordinates of the surface
      COMMON/ANPORT/DS     ! Surface area elements

      ! Kinematic viscosity of air (approximate value for standard conditions)
      VISQ = 1.46E-5

      ! Calculate total surface area (DSURF) by summing all surface elements (DS)
      DSURF = SUM(DS)

      ! Calculate Reynolds number (RE) based on velocity, characteristic length, and viscosity
      RE = VINIT * MAC / VISQ


      ! Calculate drag coefficient (CT) based on Reynolds number
      ! For laminar flow (RE <= 500,000), use Blasius solution
      CT = 1.3280 / SQRT(RE)
      ! For turbulent flow (RE > 500,000), use empirical correlation
      IF (RE .GT. 5.D+5) THEN
            CT = 0.074 / RE**0.2
      END IF

      ! Calculate total drag coefficient (CDT) by scaling CT with surface area and wing area
      CDT = CT * DSURF / WINGAREA

      ! If symmetry is applied (NSYM = 1), double the drag coefficient
      IF (NSYM .EQ. 1) THEN
            CDT = 2.0 * CDT
      END IF

      END SUBROUTINE CDTDRAG
