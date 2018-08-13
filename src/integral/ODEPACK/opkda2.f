*DECK DGEFA
      SUBROUTINE DGEFA (A, LDA, N, IPVT, INFO)
C***BEGIN PROLOGUE  DGEFA
C***PURPOSE  Factor a matrix using Gaussian elimination.
C***CATEGORY  D2A1
C***TYPE      DOUBLE PRECISION (SGEFA-S, DGEFA-D, CGEFA-C)
C***KEYWORDS  GENERAL MATRIX, LINEAR ALGEBRA, LINPACK,
C             MATRIX FACTORIZATION
C***AUTHOR  Moler, C. B., (U. of New Mexico)
C***DESCRIPTION
C
C     DGEFA factors a double precision matrix by Gaussian elimination.
C
C     DGEFA is usually called by DGECO, but it can be called
C     directly with a saving in time if  RCOND  is not needed.
C     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) .
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the matrix to be factored.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C     On Return
C
C        A       an upper triangular matrix and the multipliers
C                which were used to obtain it.
C                The factorization can be written  A = L*U  where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an integer vector of pivot indices.
C
C        INFO    INTEGER
C                = 0  normal value.
C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
C                     condition for this subroutine, but it does
C                     indicate that DGESL or DGEDI will divide by zero
C                     if called.  Use  RCOND  in DGECO for a reliable
C                     indication of singularity.
C
C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
C***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
C***REVISION HISTORY  (YYMMDD)
C   780814  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DGEFA
      INTEGER LDA,N,IPVT(*),INFO
      DOUBLE PRECISION A(LDA,*)
C
      DOUBLE PRECISION T
      INTEGER IDAMAX,J,K,KP1,L,NM1
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
C***FIRST EXECUTABLE STATEMENT  DGEFA
      INFO = 0
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 70
      DO 60 K = 1, NM1
         KP1 = K + 1
C
C        FIND L = PIVOT INDEX
C
         L = IDAMAX(N-K+1,A(K,K),1) + K - 1
         IPVT(K) = L
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (A(L,K) .EQ. 0.0D0) GO TO 40
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. K) GO TO 10
               T = A(L,K)
               A(L,K) = A(K,K)
               A(K,K) = T
   10       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0D0/A(K,K)
            CALL DSCAL(N-K,T,A(K+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            DO 30 J = KP1, N
               T = A(L,J)
               IF (L .EQ. K) GO TO 20
                  A(L,J) = A(K,J)
                  A(K,J) = T
   20          CONTINUE
               CALL DAXPY(N-K,T,A(K+1,K),1,A(K+1,J),1)
   30       CONTINUE
         GO TO 50
   40    CONTINUE
            INFO = K
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      IPVT(N) = N
      IF (A(N,N) .EQ. 0.0D0) INFO = N
      RETURN
      END
*DECK DGESL
      SUBROUTINE DGESL (A, LDA, N, IPVT, B, JOB)
C***BEGIN PROLOGUE  DGESL
C***PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the
C            factors computed by DGECO or DGEFA.
C***CATEGORY  D2A1
C***TYPE      DOUBLE PRECISION (SGESL-S, DGESL-D, CGESL-C)
C***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
C***AUTHOR  Moler, C. B., (U. of New Mexico)
C***DESCRIPTION
C
C     DGESL solves the double precision system
C     A * X = B  or  TRANS(A) * X = B
C     using the factors computed by DGECO or DGEFA.
C
C     On Entry
C
C        A       DOUBLE PRECISION(LDA, N)
C                the output from DGECO or DGEFA.
C
C        LDA     INTEGER
C                the leading dimension of the array  A .
C
C        N       INTEGER
C                the order of the matrix  A .
C
C        IPVT    INTEGER(N)
C                the pivot vector from DGECO or DGEFA.
C
C        B       DOUBLE PRECISION(N)
C                the right hand side vector.
C
C        JOB     INTEGER
C                = 0         to solve  A*X = B ,
C                = nonzero   to solve  TRANS(A)*X = B  where
C                            TRANS(A)  is the transpose.
C
C     On Return
C
C        B       the solution vector  X .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains a
C        zero on the diagonal.  Technically this indicates singularity
C        but it is often caused by improper arguments or improper
C        setting of LDA .  It will not occur if the subroutines are
C        called correctly and if DGECO has set RCOND .GT. 0.0
C        or DGEFA has set INFO .EQ. 0 .
C
C     To compute  INVERSE(A) * C  where  C  is a matrix
C     with  P  columns
C           CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
C           IF (RCOND is too small) GO TO ...
C           DO 10 J = 1, P
C              CALL DGESL(A,LDA,N,IPVT,C(1,J),0)
C        10 CONTINUE
C
C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
C***ROUTINES CALLED  DAXPY, DDOT
C***REVISION HISTORY  (YYMMDD)
C   780814  DATE WRITTEN
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DGESL
      INTEGER LDA,N,IPVT(*),JOB
      DOUBLE PRECISION A(LDA,*),B(*)
C
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB,L,NM1
C***FIRST EXECUTABLE STATEMENT  DGESL
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
C
C        JOB = 0 , SOLVE  A * X = B
C        FIRST SOLVE  L*Y = B
C
         IF (NM1 .LT. 1) GO TO 30
         DO 20 K = 1, NM1
            L = IPVT(K)
            T = B(L)
            IF (L .EQ. K) GO TO 10
               B(L) = B(K)
               B(K) = T
   10       CONTINUE
            CALL DAXPY(N-K,T,A(K+1,K),1,B(K+1),1)
   20    CONTINUE
   30    CONTINUE
C
C        NOW SOLVE  U*X = Y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/A(K,K)
            T = -B(K)
            CALL DAXPY(K-1,T,A(1,K),1,B(1),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
C
C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
C        FIRST SOLVE  TRANS(U)*Y = B
C
         DO 60 K = 1, N
            T = DDOT(K-1,A(1,K),1,B(1),1)
            B(K) = (B(K) - T)/A(K,K)
   60    CONTINUE
C
C        NOW SOLVE TRANS(L)*X = Y
C
         IF (NM1 .LT. 1) GO TO 90
         DO 80 KB = 1, NM1
            K = N - KB
            B(K) = B(K) + DDOT(N-K,A(K+1,K),1,B(K+1),1)
            L = IPVT(K)
            IF (L .EQ. K) GO TO 70
               T = B(L)
               B(L) = B(K)
               B(K) = T
   70       CONTINUE
   80    CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END
*DECK DGBFA
      SUBROUTINE DGBFA (ABD, LDA, N, ML, MU, IPVT, INFO)
C***BEGIN PROLOGUE  DGBFA
C***PURPOSE  Factor a band matrix using Gaussian elimination.
C***CATEGORY  D2A2
C***TYPE      DOUBLE PRECISION (SGBFA-S, DGBFA-D, CGBFA-C)
C***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION
C***AUTHOR  Moler, C. B., (U. of New Mexico)
C***DESCRIPTION
C
C     DGBFA factors a double precision band matrix by elimination.
C
C     DGBFA is usually called by DGBCO, but it can be called
C     directly with a saving in time if  RCOND  is not needed.
C
C     On Entry
C
C        ABD     DOUBLE PRECISION(LDA, N)
C                contains the matrix in band storage.  The columns
C                of the matrix are stored in the columns of  ABD  and
C                the diagonals of the matrix are stored in rows
C                ML+1 through 2*ML+MU+1 of  ABD .
C                See the comments below for details.
C
C        LDA     INTEGER
C                the leading dimension of the array  ABD .
C                LDA must be .GE. 2*ML + MU + 1 .
C
C        N       INTEGER
C                the order of the original matrix.
C
C        ML      INTEGER
C                number of diagonals below the main diagonal.
C                0 .LE. ML .LT.  N .
C
C        MU      INTEGER
C                number of diagonals above the main diagonal.
C                0 .LE. MU .LT.  N .
C                More efficient if  ML .LE. MU .
C     On Return
C
C        ABD     an upper triangular matrix in band storage and
C                the multipliers which were used to obtain it.
C                The factorization can be written  A = L*U  where
C                L  is a product of permutation and unit lower
C                triangular matrices and  U  is upper triangular.
C
C        IPVT    INTEGER(N)
C                an integer vector of pivot indices.
C
C        INFO    INTEGER
C                = 0  normal value.
C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
C                     condition for this subroutine, but it does
C                     indicate that DGBSL will divide by zero if
C                     called.  Use  RCOND  in DGBCO for a reliable
C                     indication of singularity.
C
C     Band Storage
C
C           If  A  is a band matrix, the following program segment
C           will set up the input.
C
C                   ML = (band width below the diagonal)
C                   MU = (band width above the diagonal)
C                   M = ML + MU + 1
C                   DO 20 J = 1, N
C                      I1 = MAX(1, J-MU)
C                      I2 = MIN(N, J+ML)
C                      DO 10 I = I1, I2
C                         K = I - J + M
C                         ABD(K,J) = A(I,J)
C                10    CONTINUE
C                20 CONTINUE
C
C           This uses rows  ML+1  through  2*ML+MU+1  of  ABD .
C           In addition, the first  ML  rows in  ABD  are used for
C           elements generated during the triangularization.
C           The total number of rows needed in  ABD  is  2*ML+MU+1 .
C           The  ML+MU by ML+MU  upper left triangle and the
C           ML by ML  lower right triangle are not referenced.
C
C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
C***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
C***REVISION HISTORY  (YYMMDD)
C   780814  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DGBFA
      INTEGER LDA,N,ML,MU,IPVT(*),INFO
      DOUBLE PRECISION ABD(LDA,*)
C
      DOUBLE PRECISION T
      INTEGER I,IDAMAX,I0,J,JU,JZ,J0,J1,K,KP1,L,LM,M,MM,NM1
C
C***FIRST EXECUTABLE STATEMENT  DGBFA
      M = ML + MU + 1
      INFO = 0
C
C     ZERO INITIAL FILL-IN COLUMNS
C
      J0 = MU + 2
      J1 = MIN(N,M) - 1
      IF (J1 .LT. J0) GO TO 30
      DO 20 JZ = J0, J1
         I0 = M + 1 - JZ
         DO 10 I = I0, ML
            ABD(I,JZ) = 0.0D0
   10    CONTINUE
   20 CONTINUE
   30 CONTINUE
      JZ = J1
      JU = 0
C
C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C
      NM1 = N - 1
      IF (NM1 .LT. 1) GO TO 130
      DO 120 K = 1, NM1
         KP1 = K + 1
C
C        ZERO NEXT FILL-IN COLUMN
C
         JZ = JZ + 1
         IF (JZ .GT. N) GO TO 50
         IF (ML .LT. 1) GO TO 50
            DO 40 I = 1, ML
               ABD(I,JZ) = 0.0D0
   40       CONTINUE
   50    CONTINUE
C
C        FIND L = PIVOT INDEX
C
         LM = MIN(ML,N-K)
         L = IDAMAX(LM+1,ABD(M,K),1) + M - 1
         IPVT(K) = L + K - M
C
C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
C
         IF (ABD(L,K) .EQ. 0.0D0) GO TO 100
C
C           INTERCHANGE IF NECESSARY
C
            IF (L .EQ. M) GO TO 60
               T = ABD(L,K)
               ABD(L,K) = ABD(M,K)
               ABD(M,K) = T
   60       CONTINUE
C
C           COMPUTE MULTIPLIERS
C
            T = -1.0D0/ABD(M,K)
            CALL DSCAL(LM,T,ABD(M+1,K),1)
C
C           ROW ELIMINATION WITH COLUMN INDEXING
C
            JU = MIN(MAX(JU,MU+IPVT(K)),N)
            MM = M
            IF (JU .LT. KP1) GO TO 90
            DO 80 J = KP1, JU
               L = L - 1
               MM = MM - 1
               T = ABD(L,J)
               IF (L .EQ. MM) GO TO 70
                  ABD(L,J) = ABD(MM,J)
                  ABD(MM,J) = T
   70          CONTINUE
               CALL DAXPY(LM,T,ABD(M+1,K),1,ABD(MM+1,J),1)
   80       CONTINUE
   90       CONTINUE
         GO TO 110
  100    CONTINUE
            INFO = K
  110    CONTINUE
  120 CONTINUE
  130 CONTINUE
      IPVT(N) = N
      IF (ABD(M,N) .EQ. 0.0D0) INFO = N
      RETURN
      END
*DECK DGBSL
      SUBROUTINE DGBSL (ABD, LDA, N, ML, MU, IPVT, B, JOB)
C***BEGIN PROLOGUE  DGBSL
C***PURPOSE  Solve the real band system A*X=B or TRANS(A)*X=B using
C            the factors computed by DGBCO or DGBFA.
C***CATEGORY  D2A2
C***TYPE      DOUBLE PRECISION (SGBSL-S, DGBSL-D, CGBSL-C)
C***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
C***AUTHOR  Moler, C. B., (U. of New Mexico)
C***DESCRIPTION
C
C     DGBSL solves the double precision band system
C     A * X = B  or  TRANS(A) * X = B
C     using the factors computed by DGBCO or DGBFA.
C
C     On Entry
C
C        ABD     DOUBLE PRECISION(LDA, N)
C                the output from DGBCO or DGBFA.
C
C        LDA     INTEGER
C                the leading dimension of the array  ABD .
C
C        N       INTEGER
C                the order of the original matrix.
C
C        ML      INTEGER
C                number of diagonals below the main diagonal.
C
C        MU      INTEGER
C                number of diagonals above the main diagonal.
C
C        IPVT    INTEGER(N)
C                the pivot vector from DGBCO or DGBFA.
C
C        B       DOUBLE PRECISION(N)
C                the right hand side vector.
C
C        JOB     INTEGER
C                = 0         to solve  A*X = B ,
C                = nonzero   to solve  TRANS(A)*X = B , where
C                            TRANS(A)  is the transpose.
C
C     On Return
C
C        B       the solution vector  X .
C
C     Error Condition
C
C        A division by zero will occur if the input factor contains a
C        zero on the diagonal.  Technically this indicates singularity
C        but it is often caused by improper arguments or improper
C        setting of LDA .  It will not occur if the subroutines are
C        called correctly and if DGBCO has set RCOND .GT. 0.0
C        or DGBFA has set INFO .EQ. 0 .
C
C     To compute  INVERSE(A) * C  where  C  is a matrix
C     with  P  columns
C           CALL DGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
C           IF (RCOND is too small) GO TO ...
C           DO 10 J = 1, P
C              CALL DGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
C        10 CONTINUE
C
C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
C***ROUTINES CALLED  DAXPY, DDOT
C***REVISION HISTORY  (YYMMDD)
C   780814  DATE WRITTEN
C   890531  Changed all specific intrinsics to generic.  (WRB)
C   890831  Modified array declarations.  (WRB)
C   890831  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900326  Removed duplicate information from DESCRIPTION section.
C           (WRB)
C   920501  Reformatted the REFERENCES section.  (WRB)
C***END PROLOGUE  DGBSL
      INTEGER LDA,N,ML,MU,IPVT(*),JOB
      DOUBLE PRECISION ABD(LDA,*),B(*)
C
      DOUBLE PRECISION DDOT,T
      INTEGER K,KB,L,LA,LB,LM,M,NM1
C***FIRST EXECUTABLE STATEMENT  DGBSL
      M = MU + ML + 1
      NM1 = N - 1
      IF (JOB .NE. 0) GO TO 50
C
C        JOB = 0 , SOLVE  A * X = B
C        FIRST SOLVE L*Y = B
C
         IF (ML .EQ. 0) GO TO 30
         IF (NM1 .LT. 1) GO TO 30
            DO 20 K = 1, NM1
               LM = MIN(ML,N-K)
               L = IPVT(K)
               T = B(L)
               IF (L .EQ. K) GO TO 10
                  B(L) = B(K)
                  B(K) = T
   10          CONTINUE
               CALL DAXPY(LM,T,ABD(M+1,K),1,B(K+1),1)
   20       CONTINUE
   30    CONTINUE
C
C        NOW SOLVE  U*X = Y
C
         DO 40 KB = 1, N
            K = N + 1 - KB
            B(K) = B(K)/ABD(M,K)
            LM = MIN(K,M) - 1
            LA = M - LM
            LB = K - LM
            T = -B(K)
            CALL DAXPY(LM,T,ABD(LA,K),1,B(LB),1)
   40    CONTINUE
      GO TO 100
   50 CONTINUE
C
C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
C        FIRST SOLVE  TRANS(U)*Y = B
C
         DO 60 K = 1, N
            LM = MIN(K,M) - 1
            LA = M - LM
            LB = K - LM
            T = DDOT(LM,ABD(LA,K),1,B(LB),1)
            B(K) = (B(K) - T)/ABD(M,K)
   60    CONTINUE
C
C        NOW SOLVE TRANS(L)*X = Y
C
         IF (ML .EQ. 0) GO TO 90
         IF (NM1 .LT. 1) GO TO 90
            DO 80 KB = 1, NM1
               K = N - KB
               LM = MIN(ML,N-K)
               B(K) = B(K) + DDOT(LM,ABD(M+1,K),1,B(K+1),1)
               L = IPVT(K)
               IF (L .EQ. K) GO TO 70
                  T = B(L)
                  B(L) = B(K)
                  B(K) = T
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
  100 CONTINUE
      RETURN
      END

c *DECK DAXPY
c       SUBROUTINE DAXPY (N, DA, DX, INCX, DY, INCY)
c C***BEGIN PROLOGUE  DAXPY
c C***PURPOSE  Compute a constant times a vector plus a vector.
c C***CATEGORY  D1A7
c C***TYPE      DOUBLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C)
c C***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR
c C***AUTHOR  Lawson, C. L., (JPL)
c C           Hanson, R. J., (SNLA)
c C           Kincaid, D. R., (U. of Texas)
c C           Krogh, F. T., (JPL)
c C***DESCRIPTION
c C
c C                B L A S  Subprogram
c C    Description of Parameters
c C
c C     --Input--
c C        N  number of elements in input vector(s)
c C       DA  double precision scalar multiplier
c C       DX  double precision vector with N elements
c C     INCX  storage spacing between elements of DX
c C       DY  double precision vector with N elements
c C     INCY  storage spacing between elements of DY
c C
c C     --Output--
c C       DY  double precision result (unchanged if N .LE. 0)
c C
c C     Overwrite double precision DY with double precision DA*DX + DY.
c C     For I = 0 to N-1, replace  DY(LY+I*INCY) with DA*DX(LX+I*INCX) +
c C       DY(LY+I*INCY),
c C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
c C     defined in a similar way using INCY.
c C
c C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
c C                 Krogh, Basic linear algebra subprograms for Fortran
c C                 usage, Algorithm No. 539, Transactions on Mathematical
c C                 Software 5, 3 (September 1979), pp. 308-323.
c C***ROUTINES CALLED  (NONE)
c C***REVISION HISTORY  (YYMMDD)
c C   791001  DATE WRITTEN
c C   890831  Modified array declarations.  (WRB)
c C   890831  REVISION DATE from Version 3.2
c C   891214  Prologue converted to Version 4.0 format.  (BAB)
c C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
c C   920501  Reformatted the REFERENCES section.  (WRB)
c C***END PROLOGUE  DAXPY
c       DOUBLE PRECISION DX(*), DY(*), DA
c C***FIRST EXECUTABLE STATEMENT  DAXPY
c       IF (N.LE.0 .OR. DA.EQ.0.0D0) RETURN
c       IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
c C
c C     Code for unequal or nonpositive increments.
c C
c     5 IX = 1
c       IY = 1
c       IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
c       IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
c       DO 10 I = 1,N
c         DY(IY) = DY(IY) + DA*DX(IX)
c         IX = IX + INCX
c         IY = IY + INCY
c    10 CONTINUE
c       RETURN
c C
c C     Code for both increments equal to 1.
c C
c C     Clean-up loop so remaining vector length is a multiple of 4.
c C
c    20 M = MOD(N,4)
c       IF (M .EQ. 0) GO TO 40
c       DO 30 I = 1,M
c         DY(I) = DY(I) + DA*DX(I)
c    30 CONTINUE
c       IF (N .LT. 4) RETURN
c    40 MP1 = M + 1
c       DO 50 I = MP1,N,4
c         DY(I) = DY(I) + DA*DX(I)
c         DY(I+1) = DY(I+1) + DA*DX(I+1)
c         DY(I+2) = DY(I+2) + DA*DX(I+2)
c         DY(I+3) = DY(I+3) + DA*DX(I+3)
c    50 CONTINUE
c       RETURN
c C
c C     Code for equal, positive, non-unit increments.
c C
c    60 NS = N*INCX
c       DO 70 I = 1,NS,INCX
c         DY(I) = DA*DX(I) + DY(I)
c    70 CONTINUE
c       RETURN
c       END
c *DECK DCOPY
c       SUBROUTINE DCOPY (N, DX, INCX, DY, INCY)
c C***BEGIN PROLOGUE  DCOPY
c C***PURPOSE  Copy a vector.
c C***CATEGORY  D1A5
c C***TYPE      DOUBLE PRECISION (SCOPY-S, DCOPY-D, CCOPY-C, ICOPY-I)
c C***KEYWORDS  BLAS, COPY, LINEAR ALGEBRA, VECTOR
c C***AUTHOR  Lawson, C. L., (JPL)
c C           Hanson, R. J., (SNLA)
c C           Kincaid, D. R., (U. of Texas)
c C           Krogh, F. T., (JPL)
c C***DESCRIPTION
c C
c C                B L A S  Subprogram
c C    Description of Parameters
c C
c C     --Input--
c C        N  number of elements in input vector(s)
c C       DX  double precision vector with N elements
c C     INCX  storage spacing between elements of DX
c C       DY  double precision vector with N elements
c C     INCY  storage spacing between elements of DY
c C
c C     --Output--
c C       DY  copy of vector DX (unchanged if N .LE. 0)
c C
c C     Copy double precision DX to double precision DY.
c C     For I = 0 to N-1, copy DX(LX+I*INCX) to DY(LY+I*INCY),
c C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
c C     defined in a similar way using INCY.
c C
c C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
c C                 Krogh, Basic linear algebra subprograms for Fortran
c C                 usage, Algorithm No. 539, Transactions on Mathematical
c C                 Software 5, 3 (September 1979), pp. 308-323.
c C***ROUTINES CALLED  (NONE)
c C***REVISION HISTORY  (YYMMDD)
c C   791001  DATE WRITTEN
c C   890831  Modified array declarations.  (WRB)
c C   890831  REVISION DATE from Version 3.2
c C   891214  Prologue converted to Version 4.0 format.  (BAB)
c C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
c C   920501  Reformatted the REFERENCES section.  (WRB)
c C***END PROLOGUE  DCOPY
c       DOUBLE PRECISION DX(*), DY(*)
c C***FIRST EXECUTABLE STATEMENT  DCOPY
c       IF (N .LE. 0) RETURN
c       IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
c C
c C     Code for unequal or nonpositive increments.
c C
c     5 IX = 1
c       IY = 1
c       IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
c       IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
c       DO 10 I = 1,N
c         DY(IY) = DX(IX)
c         IX = IX + INCX
c         IY = IY + INCY
c    10 CONTINUE
c       RETURN
c C
c C     Code for both increments equal to 1.
c C
c C     Clean-up loop so remaining vector length is a multiple of 7.
c C
c    20 M = MOD(N,7)
c       IF (M .EQ. 0) GO TO 40
c       DO 30 I = 1,M
c         DY(I) = DX(I)
c    30 CONTINUE
c       IF (N .LT. 7) RETURN
c    40 MP1 = M + 1
c       DO 50 I = MP1,N,7
c         DY(I) = DX(I)
c         DY(I+1) = DX(I+1)
c         DY(I+2) = DX(I+2)
c         DY(I+3) = DX(I+3)
c         DY(I+4) = DX(I+4)
c         DY(I+5) = DX(I+5)
c         DY(I+6) = DX(I+6)
c    50 CONTINUE
c       RETURN
c C
c C     Code for equal, positive, non-unit increments.
c C
c    60 NS = N*INCX
c       DO 70 I = 1,NS,INCX
c         DY(I) = DX(I)
c    70 CONTINUE
c       RETURN
c       END
c *DECK DDOT
c       DOUBLE PRECISION FUNCTION DDOT (N, DX, INCX, DY, INCY)
c C***BEGIN PROLOGUE  DDOT
c C***PURPOSE  Compute the inner product of two vectors.
c C***CATEGORY  D1A4
c C***TYPE      DOUBLE PRECISION (SDOT-S, DDOT-D, CDOTU-C)
c C***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR
c C***AUTHOR  Lawson, C. L., (JPL)
c C           Hanson, R. J., (SNLA)
c C           Kincaid, D. R., (U. of Texas)
c C           Krogh, F. T., (JPL)
c C***DESCRIPTION
c C
c C                B L A S  Subprogram
c C    Description of Parameters
c C
c C     --Input--
c C        N  number of elements in input vector(s)
c C       DX  double precision vector with N elements
c C     INCX  storage spacing between elements of DX
c C       DY  double precision vector with N elements
c C     INCY  storage spacing between elements of DY
c C
c C     --Output--
c C     DDOT  double precision dot product (zero if N .LE. 0)
c C
c C     Returns the dot product of double precision DX and DY.
c C     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY),
c C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
c C     defined in a similar way using INCY.
c C
c C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
c C                 Krogh, Basic linear algebra subprograms for Fortran
c C                 usage, Algorithm No. 539, Transactions on Mathematical
c C                 Software 5, 3 (September 1979), pp. 308-323.
c C***ROUTINES CALLED  (NONE)
c C***REVISION HISTORY  (YYMMDD)
c C   791001  DATE WRITTEN
c C   890831  Modified array declarations.  (WRB)
c C   890831  REVISION DATE from Version 3.2
c C   891214  Prologue converted to Version 4.0 format.  (BAB)
c C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
c C   920501  Reformatted the REFERENCES section.  (WRB)
c C***END PROLOGUE  DDOT
c       DOUBLE PRECISION DX(*), DY(*)
c C***FIRST EXECUTABLE STATEMENT  DDOT
c       DDOT = 0.0D0
c       IF (N .LE. 0) RETURN
c       IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
c C
c C     Code for unequal or nonpositive increments.
c C
c     5 IX = 1
c       IY = 1
c       IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
c       IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
c       DO 10 I = 1,N
c         DDOT = DDOT + DX(IX)*DY(IY)
c         IX = IX + INCX
c         IY = IY + INCY
c    10 CONTINUE
c       RETURN
c C
c C     Code for both increments equal to 1.
c C
c C     Clean-up loop so remaining vector length is a multiple of 5.
c C
c    20 M = MOD(N,5)
c       IF (M .EQ. 0) GO TO 40
c       DO 30 I = 1,M
c          DDOT = DDOT + DX(I)*DY(I)
c    30 CONTINUE
c       IF (N .LT. 5) RETURN
c    40 MP1 = M + 1
c       DO 50 I = MP1,N,5
c       DDOT = DDOT + DX(I)*DY(I) + DX(I+1)*DY(I+1) + DX(I+2)*DY(I+2) +
c      1              DX(I+3)*DY(I+3) + DX(I+4)*DY(I+4)
c    50 CONTINUE
c       RETURN
c C
c C     Code for equal, positive, non-unit increments.
c C
c    60 NS = N*INCX
c       DO 70 I = 1,NS,INCX
c         DDOT = DDOT + DX(I)*DY(I)
c    70 CONTINUE
c       RETURN
c       END
c *DECK DNRM2
c       DOUBLE PRECISION FUNCTION DNRM2 (N, DX, INCX)
c C***BEGIN PROLOGUE  DNRM2
c C***PURPOSE  Compute the Euclidean length (L2 norm) of a vector.
c C***CATEGORY  D1A3B
c C***TYPE      DOUBLE PRECISION (SNRM2-S, DNRM2-D, SCNRM2-C)
c C***KEYWORDS  BLAS, EUCLIDEAN LENGTH, EUCLIDEAN NORM, L2,
c C             LINEAR ALGEBRA, UNITARY, VECTOR
c C***AUTHOR  Lawson, C. L., (JPL)
c C           Hanson, R. J., (SNLA)
c C           Kincaid, D. R., (U. of Texas)
c C           Krogh, F. T., (JPL)
c C***DESCRIPTION
c C
c C                B L A S  Subprogram
c C    Description of parameters
c C
c C     --Input--
c C        N  number of elements in input vector(s)
c C       DX  double precision vector with N elements
c C     INCX  storage spacing between elements of DX
c C
c C     --Output--
c C    DNRM2  double precision result (zero if N .LE. 0)
c C
c C     Euclidean norm of the N-vector stored in DX with storage
c C     increment INCX.
c C     If N .LE. 0, return with result = 0.
c C     If N .GE. 1, then INCX must be .GE. 1
c C
c C     Four phase method using two built-in constants that are
c C     hopefully applicable to all machines.
c C         CUTLO = maximum of  SQRT(U/EPS)  over all known machines.
c C         CUTHI = minimum of  SQRT(V)      over all known machines.
c C     where
c C         EPS = smallest no. such that EPS + 1. .GT. 1.
c C         U   = smallest positive no.   (underflow limit)
c C         V   = largest  no.            (overflow  limit)
c C
c C     Brief outline of algorithm.
c C
c C     Phase 1 scans zero components.
c C     move to phase 2 when a component is nonzero and .LE. CUTLO
c C     move to phase 3 when a component is .GT. CUTLO
c C     move to phase 4 when a component is .GE. CUTHI/M
c C     where M = N for X() real and M = 2*N for complex.
c C
c C     Values for CUTLO and CUTHI.
c C     From the environmental parameters listed in the IMSL converter
c C     document the limiting values are as follows:
c C     CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
c C                   Univac and DEC at 2**(-103)
c C                   Thus CUTLO = 2**(-51) = 4.44089E-16
c C     CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
c C                   Thus CUTHI = 2**(63.5) = 1.30438E19
c C     CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
c C                   Thus CUTLO = 2**(-33.5) = 8.23181D-11
c C     CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
c C     DATA CUTLO, CUTHI /8.232D-11,  1.304D19/
c C     DATA CUTLO, CUTHI /4.441E-16,  1.304E19/
c C
c C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
c C                 Krogh, Basic linear algebra subprograms for Fortran
c C                 usage, Algorithm No. 539, Transactions on Mathematical
c C                 Software 5, 3 (September 1979), pp. 308-323.
c C***ROUTINES CALLED  (NONE)
c C***REVISION HISTORY  (YYMMDD)
c C   791001  DATE WRITTEN
c C   890531  Changed all specific intrinsics to generic.  (WRB)
c C   890831  Modified array declarations.  (WRB)
c C   890831  REVISION DATE from Version 3.2
c C   891214  Prologue converted to Version 4.0 format.  (BAB)
c C   920501  Reformatted the REFERENCES section.  (WRB)
c C***END PROLOGUE  DNRM2
c       INTEGER NEXT
c       DOUBLE PRECISION DX(*), CUTLO, CUTHI, HITEST, SUM, XMAX, ZERO,
c      +                 ONE
c       SAVE CUTLO, CUTHI, ZERO, ONE
c       DATA ZERO, ONE /0.0D0, 1.0D0/
c C
c       DATA CUTLO, CUTHI /8.232D-11,  1.304D19/
c C***FIRST EXECUTABLE STATEMENT  DNRM2
c       IF (N .GT. 0) GO TO 10
c          DNRM2  = ZERO
c          GO TO 300
c C
c    10 ASSIGN 30 TO NEXT
c       SUM = ZERO
c       NN = N * INCX
c C
c C                                                 BEGIN MAIN LOOP
c C
c       I = 1
c    20    GO TO NEXT,(30, 50, 70, 110)
c    30 IF (ABS(DX(I)) .GT. CUTLO) GO TO 85
c       ASSIGN 50 TO NEXT
c       XMAX = ZERO
c C
c C                        PHASE 1.  SUM IS ZERO
c C
c    50 IF (DX(I) .EQ. ZERO) GO TO 200
c       IF (ABS(DX(I)) .GT. CUTLO) GO TO 85
c C
c C                                PREPARE FOR PHASE 2.
c C
c       ASSIGN 70 TO NEXT
c       GO TO 105
c C
c C                                PREPARE FOR PHASE 4.
c C
c   100 I = J
c       ASSIGN 110 TO NEXT
c       SUM = (SUM / DX(I)) / DX(I)
c   105 XMAX = ABS(DX(I))
c       GO TO 115
c C
c C                   PHASE 2.  SUM IS SMALL.
c C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
c C
c    70 IF (ABS(DX(I)) .GT. CUTLO) GO TO 75
c C
c C                     COMMON CODE FOR PHASES 2 AND 4.
c C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
c C
c   110 IF (ABS(DX(I)) .LE. XMAX) GO TO 115
c          SUM = ONE + SUM * (XMAX / DX(I))**2
c          XMAX = ABS(DX(I))
c          GO TO 200
c C
c   115 SUM = SUM + (DX(I)/XMAX)**2
c       GO TO 200
c C
c C                  PREPARE FOR PHASE 3.
c C
c    75 SUM = (SUM * XMAX) * XMAX
c C
c C     FOR REAL OR D.P. SET HITEST = CUTHI/N
c C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
c C
c    85 HITEST = CUTHI / N
c C
c C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
c C
c       DO 95 J = I,NN,INCX
c       IF (ABS(DX(J)) .GE. HITEST) GO TO 100
c    95    SUM = SUM + DX(J)**2
c       DNRM2 = SQRT(SUM)
c       GO TO 300
c C
c   200 CONTINUE
c       I = I + INCX
c       IF (I .LE. NN) GO TO 20
c C
c C              END OF MAIN LOOP.
c C
c C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
c C
c       DNRM2 = XMAX * SQRT(SUM)
c   300 CONTINUE
c       RETURN
c       END
c *DECK DSCAL
c       SUBROUTINE DSCAL (N, DA, DX, INCX)
c C***BEGIN PROLOGUE  DSCAL
c C***PURPOSE  Multiply a vector by a constant.
c C***CATEGORY  D1A6
c C***TYPE      DOUBLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C)
c C***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR
c C***AUTHOR  Lawson, C. L., (JPL)
c C           Hanson, R. J., (SNLA)
c C           Kincaid, D. R., (U. of Texas)
c C           Krogh, F. T., (JPL)
c C***DESCRIPTION
c C
c C                B L A S  Subprogram
c C    Description of Parameters
c C
c C     --Input--
c C        N  number of elements in input vector(s)
c C       DA  double precision scale factor
c C       DX  double precision vector with N elements
c C     INCX  storage spacing between elements of DX
c C
c C     --Output--
c C       DX  double precision result (unchanged if N.LE.0)
c C
c C     Replace double precision DX by double precision DA*DX.
c C     For I = 0 to N-1, replace DX(IX+I*INCX) with  DA * DX(IX+I*INCX),
c C     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
c C
c C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
c C                 Krogh, Basic linear algebra subprograms for Fortran
c C                 usage, Algorithm No. 539, Transactions on Mathematical
c C                 Software 5, 3 (September 1979), pp. 308-323.
c C***ROUTINES CALLED  (NONE)
c C***REVISION HISTORY  (YYMMDD)
c C   791001  DATE WRITTEN
c C   890831  Modified array declarations.  (WRB)
c C   890831  REVISION DATE from Version 3.2
c C   891214  Prologue converted to Version 4.0 format.  (BAB)
c C   900821  Modified to correct problem with a negative increment.
c C           (WRB)
c C   920501  Reformatted the REFERENCES section.  (WRB)
c C***END PROLOGUE  DSCAL
c       DOUBLE PRECISION DA, DX(*)
c       INTEGER I, INCX, IX, M, MP1, N
c C***FIRST EXECUTABLE STATEMENT  DSCAL
c       IF (N .LE. 0) RETURN
c       IF (INCX .EQ. 1) GOTO 20
c C
c C     Code for increment not equal to 1.
c C
c       IX = 1
c       IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
c       DO 10 I = 1,N
c         DX(IX) = DA*DX(IX)
c         IX = IX + INCX
c    10 CONTINUE
c       RETURN
c C
c C     Code for increment equal to 1.
c C
c C     Clean-up loop so remaining vector length is a multiple of 5.
c C
c    20 M = MOD(N,5)
c       IF (M .EQ. 0) GOTO 40
c       DO 30 I = 1,M
c         DX(I) = DA*DX(I)
c    30 CONTINUE
c       IF (N .LT. 5) RETURN
c    40 MP1 = M + 1
c       DO 50 I = MP1,N,5
c         DX(I) = DA*DX(I)
c         DX(I+1) = DA*DX(I+1)
c         DX(I+2) = DA*DX(I+2)
c         DX(I+3) = DA*DX(I+3)
c         DX(I+4) = DA*DX(I+4)
c    50 CONTINUE
c       RETURN
c       END
c *DECK IDAMAX
c       INTEGER FUNCTION IDAMAX (N, DX, INCX)
c C***BEGIN PROLOGUE  IDAMAX
c C***PURPOSE  Find the smallest index of that component of a vector
c C            having the maximum magnitude.
c C***CATEGORY  D1A2
c C***TYPE      DOUBLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C)
c C***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR
c C***AUTHOR  Lawson, C. L., (JPL)
c C           Hanson, R. J., (SNLA)
c C           Kincaid, D. R., (U. of Texas)
c C           Krogh, F. T., (JPL)
c C***DESCRIPTION
c C
c C                B L A S  Subprogram
c C    Description of Parameters
c C
c C     --Input--
c C        N  number of elements in input vector(s)
c C       DX  double precision vector with N elements
c C     INCX  storage spacing between elements of DX
c C
c C     --Output--
c C   IDAMAX  smallest index (zero if N .LE. 0)
c C
c C     Find smallest index of maximum magnitude of double precision DX.
c C     IDAMAX = first I, I = 1 to N, to maximize ABS(DX(IX+(I-1)*INCX)),
c C     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
c C
c C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
c C                 Krogh, Basic linear algebra subprograms for Fortran
c C                 usage, Algorithm No. 539, Transactions on Mathematical
c C                 Software 5, 3 (September 1979), pp. 308-323.
c C***ROUTINES CALLED  (NONE)
c C***REVISION HISTORY  (YYMMDD)
c C   791001  DATE WRITTEN
c C   890531  Changed all specific intrinsics to generic.  (WRB)
c C   890531  REVISION DATE from Version 3.2
c C   891214  Prologue converted to Version 4.0 format.  (BAB)
c C   900821  Modified to correct problem with a negative increment.
c C           (WRB)
c C   920501  Reformatted the REFERENCES section.  (WRB)
c C***END PROLOGUE  IDAMAX
c       DOUBLE PRECISION DX(*), DMAX, XMAG
c       INTEGER I, INCX, IX, N
c C***FIRST EXECUTABLE STATEMENT  IDAMAX
c       IDAMAX = 0
c       IF (N .LE. 0) RETURN
c       IDAMAX = 1
c       IF (N .EQ. 1) RETURN
c C
c       IF (INCX .EQ. 1) GOTO 20
c C
c C     Code for increments not equal to 1.
c C
c       IX = 1
c       IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
c       DMAX = ABS(DX(IX))
c       IX = IX + INCX
c       DO 10 I = 2,N
c         XMAG = ABS(DX(IX))
c         IF (XMAG .GT. DMAX) THEN
c           IDAMAX = I
c           DMAX = XMAG
c         ENDIF
c         IX = IX + INCX
c    10 CONTINUE
c       RETURN
c C
c C     Code for increments equal to 1.
c C
c    20 DMAX = ABS(DX(1))
c       DO 30 I = 2,N
c         XMAG = ABS(DX(I))
c         IF (XMAG .GT. DMAX) THEN
c           IDAMAX = I
c           DMAX = XMAG
c         ENDIF
c    30 CONTINUE
c       RETURN
c       END
*DECK XERRWD
      SUBROUTINE XERRWD (MSG, NMES, NERR, LEVEL, NI, I1, I2, NR, R1, R2)
C***BEGIN PROLOGUE  XERRWD
C***SUBSIDIARY
C***PURPOSE  Write error message with values.
C***CATEGORY  R3C
C***TYPE      DOUBLE PRECISION (XERRWV-S, XERRWD-D)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  Subroutines XERRWD, XSETF, XSETUN, and the function routine IXSAV,
C  as given here, constitute a simplified version of the SLATEC error
C  handling package.
C
C  All arguments are input arguments.
C
C  MSG    = The message (character array).
C  NMES   = The length of MSG (number of characters).
C  NERR   = The error number (not used).
C  LEVEL  = The error level..
C           0 or 1 means recoverable (control returns to caller).
C           2 means fatal (run is aborted--see note below).
C  NI     = Number of integers (0, 1, or 2) to be printed with message.
C  I1,I2  = Integers to be printed, depending on NI.
C  NR     = Number of reals (0, 1, or 2) to be printed with message.
C  R1,R2  = Reals to be printed, depending on NR.
C
C  Note..  this routine is machine-dependent and specialized for use
C  in limited context, in the following ways..
C  1. The argument MSG is assumed to be of type CHARACTER, and
C     the message is printed with a format of (1X,A).
C  2. The message is assumed to take only one line.
C     Multi-line messages are generated by repeated calls.
C  3. If LEVEL = 2, control passes to the statement   STOP
C     to abort the run.  This statement may be machine-dependent.
C  4. R1 and R2 are assumed to be in double precision and are printed
C     in D21.13 format.
C
C***ROUTINES CALLED  IXSAV
C***REVISION HISTORY  (YYMMDD)
C   920831  DATE WRITTEN
C   921118  Replaced MFLGSV/LUNSAV by IXSAV. (ACH)
C   930329  Modified prologue to SLATEC format. (FNF)
C   930407  Changed MSG from CHARACTER*1 array to variable. (FNF)
C   930922  Minor cosmetic change. (FNF)
C***END PROLOGUE  XERRWD
C
C*Internal Notes:
C
C For a different default logical unit number, IXSAV (or a subsidiary
C routine that it calls) will need to be modified.
C For a different run-abort command, change the statement following
C statement 100 at the end.
C-----------------------------------------------------------------------
C Subroutines called by XERRWD.. None
C Function routine called by XERRWD.. IXSAV
C-----------------------------------------------------------------------
C**End
C
C  Declare arguments.
C
      DOUBLE PRECISION R1, R2
      INTEGER NMES, NERR, LEVEL, NI, I1, I2, NR
      CHARACTER*(*) MSG
C
C  Declare local variables.
C
      INTEGER LUNIT, IXSAV, MESFLG
C
C  Get logical unit number and message print flag.
C
C***FIRST EXECUTABLE STATEMENT  XERRWD
      LUNIT = IXSAV (1, 0, .FALSE.)
      MESFLG = IXSAV (2, 0, .FALSE.)
      IF (MESFLG .EQ. 0) GO TO 100
C
C  Write the message.
C
      WRITE (LUNIT,10)  MSG
 10   FORMAT(1X,A)
      IF (NI .EQ. 1) WRITE (LUNIT, 20) I1
 20   FORMAT(6X,'In above message,  I1 =',I10)
      IF (NI .EQ. 2) WRITE (LUNIT, 30) I1,I2
 30   FORMAT(6X,'In above message,  I1 =',I10,3X,'I2 =',I10)
      IF (NR .EQ. 1) WRITE (LUNIT, 40) R1
 40   FORMAT(6X,'In above message,  R1 =',D21.13)
      IF (NR .EQ. 2) WRITE (LUNIT, 50) R1,R2
 50   FORMAT(6X,'In above,  R1 =',D21.13,3X,'R2 =',D21.13)
C
C  Abort the run if LEVEL = 2.
C
 100  IF (LEVEL .NE. 2) RETURN
      STOP
C----------------------- End of Subroutine XERRWD ----------------------
      END
*DECK XSETF
      SUBROUTINE XSETF (MFLAG)
C***BEGIN PROLOGUE  XSETF
C***PURPOSE  Reset the error print control flag.
C***CATEGORY  R3A
C***TYPE      ALL (XSETF-A)
C***KEYWORDS  ERROR CONTROL
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C   XSETF sets the error print control flag to MFLAG:
C      MFLAG=1 means print all messages (the default).
C      MFLAG=0 means no printing.
C
C***SEE ALSO  XERRWD, XERRWV
C***REFERENCES  (NONE)
C***ROUTINES CALLED  IXSAV
C***REVISION HISTORY  (YYMMDD)
C   921118  DATE WRITTEN
C   930329  Added SLATEC format prologue. (FNF)
C   930407  Corrected SEE ALSO section. (FNF)
C   930922  Made user-callable, and other cosmetic changes. (FNF)
C***END PROLOGUE  XSETF
C
C Subroutines called by XSETF.. None
C Function routine called by XSETF.. IXSAV
C-----------------------------------------------------------------------
C**End
      INTEGER MFLAG, JUNK, IXSAV
C
C***FIRST EXECUTABLE STATEMENT  XSETF
      IF (MFLAG .EQ. 0 .OR. MFLAG .EQ. 1) JUNK = IXSAV (2,MFLAG,.TRUE.)
      RETURN
C----------------------- End of Subroutine XSETF -----------------------
      END
*DECK XSETUN
      SUBROUTINE XSETUN (LUN)
C***BEGIN PROLOGUE  XSETUN
C***PURPOSE  Reset the logical unit number for error messages.
C***CATEGORY  R3B
C***TYPE      ALL (XSETUN-A)
C***KEYWORDS  ERROR CONTROL
C***DESCRIPTION
C
C   XSETUN sets the logical unit number for error messages to LUN.
C
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***SEE ALSO  XERRWD, XERRWV
C***REFERENCES  (NONE)
C***ROUTINES CALLED  IXSAV
C***REVISION HISTORY  (YYMMDD)
C   921118  DATE WRITTEN
C   930329  Added SLATEC format prologue. (FNF)
C   930407  Corrected SEE ALSO section. (FNF)
C   930922  Made user-callable, and other cosmetic changes. (FNF)
C***END PROLOGUE  XSETUN
C
C Subroutines called by XSETUN.. None
C Function routine called by XSETUN.. IXSAV
C-----------------------------------------------------------------------
C**End
      INTEGER LUN, JUNK, IXSAV
C
C***FIRST EXECUTABLE STATEMENT  XSETUN
      IF (LUN .GT. 0) JUNK = IXSAV (1,LUN,.TRUE.)
      RETURN
C----------------------- End of Subroutine XSETUN ----------------------
      END
*DECK IXSAV
      INTEGER FUNCTION IXSAV (IPAR, IVALUE, ISET)
C***BEGIN PROLOGUE  IXSAV
C***SUBSIDIARY
C***PURPOSE  Save and recall error message control parameters.
C***CATEGORY  R3C
C***TYPE      ALL (IXSAV-A)
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C
C  IXSAV saves and recalls one of two error message parameters:
C    LUNIT, the logical unit number to which messages are printed, and
C    MESFLG, the message print flag.
C  This is a modification of the SLATEC library routine J4SAVE.
C
C  Saved local variables..
C   LUNIT  = Logical unit number for messages.  The default is obtained
C            by a call to IUMACH (may be machine-dependent).
C   MESFLG = Print control flag..
C            1 means print all messages (the default).
C            0 means no printing.
C
C  On input..
C    IPAR   = Parameter indicator (1 for LUNIT, 2 for MESFLG).
C    IVALUE = The value to be set for the parameter, if ISET = .TRUE.
C    ISET   = Logical flag to indicate whether to read or write.
C             If ISET = .TRUE., the parameter will be given
C             the value IVALUE.  If ISET = .FALSE., the parameter
C             will be unchanged, and IVALUE is a dummy argument.
C
C  On return..
C    IXSAV = The (old) value of the parameter.
C
C***SEE ALSO  XERRWD, XERRWV
C***ROUTINES CALLED  IUMACH
C***REVISION HISTORY  (YYMMDD)
C   921118  DATE WRITTEN
C   930329  Modified prologue to SLATEC format. (FNF)
C   930915  Added IUMACH call to get default output unit.  (ACH)
C   930922  Minor cosmetic changes. (FNF)
C   010425  Type declaration for IUMACH added. (ACH)
C***END PROLOGUE  IXSAV
C
C Subroutines called by IXSAV.. None
C Function routine called by IXSAV.. IUMACH
C-----------------------------------------------------------------------
C**End
      LOGICAL ISET
      INTEGER IPAR, IVALUE
C-----------------------------------------------------------------------
      INTEGER IUMACH, LUNIT, MESFLG
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this routine.
C-----------------------------------------------------------------------
      SAVE LUNIT, MESFLG
      DATA LUNIT/-1/, MESFLG/1/
C
C***FIRST EXECUTABLE STATEMENT  IXSAV
      IF (IPAR .EQ. 1) THEN
        IF (LUNIT .EQ. -1) LUNIT = IUMACH()
        IXSAV = LUNIT
        IF (ISET) LUNIT = IVALUE
        ENDIF
C
      IF (IPAR .EQ. 2) THEN
        IXSAV = MESFLG
        IF (ISET) MESFLG = IVALUE
        ENDIF
C
      RETURN
C----------------------- End of Function IXSAV -------------------------
      END
*DECK IUMACH
      INTEGER FUNCTION IUMACH()
C***BEGIN PROLOGUE  IUMACH
C***PURPOSE  Provide standard output unit number.
C***CATEGORY  R1
C***TYPE      INTEGER (IUMACH-I)
C***KEYWORDS  MACHINE CONSTANTS
C***AUTHOR  Hindmarsh, Alan C., (LLNL)
C***DESCRIPTION
C *Usage:
C        INTEGER  LOUT, IUMACH
C        LOUT = IUMACH()
C
C *Function Return Values:
C     LOUT : the standard logical unit for Fortran output.
C
C***REFERENCES  (NONE)
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   930915  DATE WRITTEN
C   930922  Made user-callable, and other cosmetic changes. (FNF)
C***END PROLOGUE  IUMACH
C
C*Internal Notes:
C  The built-in value of 6 is standard on a wide range of Fortran
C  systems.  This may be machine-dependent.
C**End
C***FIRST EXECUTABLE STATEMENT  IUMACH
      IUMACH = 6
C
      RETURN
C----------------------- End of Function IUMACH ------------------------
      END
