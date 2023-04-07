      SUBROUTINE ABQMAIN
C====================================================================
C This program must be compiled and linked with the command:
C     abaqus make job=fprin
C Run the program using the command:
C     abaqus fprin
C====================================================================
C
C  Purpose: 
C 
C    This program computes the principal stresses and strains and their
C    directions from stress and strain values stored in an ABAQUS 
C    results file (.fil).
C
C  Input File names: `FNAME.fil', where FNAME is the root file name of 
C                               the input file.
C
C  Output File name:  pvalue.dat
C
C====================================================================
C
C  Variables used by this program and ABAQUS subroutine SPRIND :
C
C   NDI    -- Number of direct components in stress/strain tensor.
C   NSHR   -- Number of shear components in stress/strain tensor.
C   NDIP1  -- NDI + 1
C   ARRAY  -- Real array containing values read from results file 
C              (.fil). Equivalenced to JRRAY.
C   JRRAY  -- Integer array containing values read from results file 
C              (.fil). Equivalenced to ARRAY.
C   FNAME  -- Root file name of input file (w/o .fil extension).
C   NRU    -- Number of results files (.fil) to be read.
C   LRUNIT -- Array containing unit number and format of results files:
C               LRUNIT(1,*) --> Unit number of input file.
C               LRUNIT(2,*) --> Format of input file.
C   LOUTF  -- Format of output file:
C               0 --> Standard ASCII format.
C               1 --> ABAQUS results file ASCII format.
C               2 --> ABAQUS results file binary format.
C   JUNIT  -- Unit number of file to be opened.
C   JRCD   -- Error check return code.
C               .EQ. 0 --> No errors.
C               .NE. 0 --> Errors detected.
C   KEY    -- Current record key identifier.
C   JELNUM -- Current element number.
C   INTPN  -- Integration point number.
C   LSTR   -- Indicates type of principal value (stress/strain) and
C               ordering used:
C                 For calculation of principal value (stress/strain):
C                   1 -->  stress.
C                   2 -->  strain. 
C                 For calculation of directions:
C                   1 -->  stress.
C                   2 -->  strain.
C   S      -- Array containing stress tensor.
C   PS     -- Array containing principal stresses.
C   ANPS   -- Array containing directions of principal stresses.
C   E      -- Array containing strain tensor.
C   PE     -- Array containing principal strains.
C   ANPE   -- Array containing directions of principal strains.
C
C====================================================================
C
C  The use of ABA_PARAM.INC eliminates the need to have different  
C  versions of the code for single and double precision.  
C  ABA_PARAM.INC defines an appropriate IMPLICIT REAL statement 
C  and sets the value of NPRECD to 1 or 2, depending on whether 
C  the machine uses single or double precision.
C
C====================================================================
C
      INCLUDE 'aba_param.inc'
      DIMENSION  ARRAY(513), JRRAY(NPRECD,513), LRUNIT(2,1)
      EQUIVALENCE (ARRAY(1), JRRAY(1,1))
C
C====================================================================
      DIMENSION S(6), E(6), PS(3), PE(3), ANPS(3,3), ANPE(3,3)
      CHARACTER  FNAME*80
C
C====================================================================
C  Get the name of the results file.
C
C====================================================================
      WRITE(6,*) 'Enter the name of the input file (w/o .fil):'
      READ(5,'(A)') FNAME
C
C====================================================================
C  Open the output file.
C
C====================================================================
      OPEN(UNIT=9,FILE='pvalue.dat',STATUS='NEW')
C
      NRU = 1
      LOUTF = 0
      LRUNIT(1,1) = 8
      LRUNIT(2,1) = 2
C
      CALL INITPF(FNAME,NRU,LRUNIT,LOUTF)
C
      JUNIT = 8
C
      CALL DBRNU(JUNIT)
C
C====================================================================
C  Read records from the results (.fil) file and process the data.  
C  Cover a maximum of 10 million records in the file. 
C
C====================================================================
      DO 1000 K100 = 1, 100
      DO 1000 K1 = 1, 99999
         CALL DBFILE(0,ARRAY,JRCD)
         IF (JRCD .NE. 0) GO TO 1001
         KEY = JRRAY(1,2)
C
C====================================================================
C  Get the heading (title) record.
C
C====================================================================
         IF (KEY .EQ. 1922) THEN
            WRITE(9,1100) (ARRAY(IXX),IXX=3,12)
 1100       FORMAT(1X,10A8)
C
C====================================================================
C  Get the current step and increment number.
C
C====================================================================
         ELSE IF (KEY .EQ. 2000) THEN
            WRITE(9,1200) JRRAY(1,8), JRRAY(1,9)
 1200       FORMAT(1X,'** STEP ',I2,'    INCREMENT ',I3)
C
C====================================================================
C  Get the element and integration point numbers, JELNUM and INTPN, 
C  and the location of INTPN (0--at int.pt., 1--at centroid, 
C  4--nodal average) and the number of direct and shear components 
C  in the analysis.
C
C====================================================================
         ELSE IF (KEY .EQ. 1) THEN
            JELNUM = JRRAY(1,3)
            INTPN  = JRRAY(1,4)
            LOCATE = JRRAY(1,6)
            NDI    = JRRAY(1,8)
            NSHR   = JRRAY(1,9)
            NDIP1  = NDI + 1
            IF(LOCATE.LE.1) THEN
              WRITE(9,1201) JELNUM, INTPN ,NDI,NSHR
 1201         FORMAT(2X,'ELEMENT NUMBER = ',I8,5X,
     1                  'INT. PT. NUMBER = ',I2,5X,
     2                  'NDI/HSHR = ',2I2)
            ELSEIF(LOCATE.EQ.4) THEN
              WRITE(9,1191) JELNUM, NDI,NSHR
 1191         FORMAT(2X,'NODE NUMBER = ',I8,5X,
     1                  'NDI/HSHR = ',2I2)
            END IF
C
C====================================================================
C  Get the stress tensor. 
C
C====================================================================
         ELSE  IF (KEY .EQ. 11) THEN
            WRITE(9,1202)
 1202       FORMAT(3X,'STRESSES:')
C
            DO 10 IXX = 1, NDI
               S(IXX) = ARRAY(IXX+2)
   10       CONTINUE
            WRITE(9,1203) (S(IZZ), IZZ = 1, NDI)
 1203       FORMAT(4X,'S11 = ',E12.5,'  S22 = ',E12.5,'  S33 = ',E12.5)
            DO 20 IYY = NDI + 1, NSHR + NDI
               S(IYY) = ARRAY(IYY+2)             
   20       CONTINUE
            WRITE(9,1204) (S(IZZ), IZZ = NDI + 1, NSHR + NDI)
 1204       FORMAT(4X,'S12 = ',E12.5,'  S13 = ',E12.5,'  S23 = ',E12.5)
C
C
C====================================================================
C  Calculate the principal stresses and corresponding principal 
C  directions in unsorted order.
C====================================================================
            LSTR = 1
            CALL SPRIND(S,PS,ANPS,LSTR,NDI,NSHR)
            WRITE(9,1205) PS(1), ANPS(1,1), ANPS(1,2), ANPS(1,3)
 1205       FORMAT(4X,'PS1 = ',E12.5,/,
     1             5X,'PD11 =',F8.3,2X,'PD12 =',F8.3,2X,'PD13 =',F8.3)
            WRITE(9,1206) PS(2), ANPS(2,1), ANPS(2,2), ANPS(2,3)
 1206       FORMAT(4X,'PS2 = ',E12.5,/,
     1             5X,'PD21 =',F8.3,2X,'PD22 =',F8.3,2X,'PD23 =',F8.3)
            WRITE(9,1207) PS(3), ANPS(3,1), ANPS(3,2), ANPS(3,3)
 1207       FORMAT(4X,'PS3 = ',E12.5,/,
     1             5X,'PD31 =',F8.3,2X,'PD32 =',F8.3,2X,'PD33 =',F8.3)
C
C
C====================================================================
C  Get the strain tensor. 
C
C====================================================================
         ELSE  IF (KEY .EQ. 21) THEN
            WRITE(9,2202)
 2202       FORMAT(3X,'STRAINS:')
C
            DO 30 IXX = 1, NDI
               E(IXX) = ARRAY(IXX+2)
   30       CONTINUE
            WRITE(9,2203) (E(IZZ), IZZ = 1, NDI)
 2203       FORMAT(4X,'E11 = ',E12.5,'  E22 = ',E12.5,'  E33 = ',E12.5)
            DO 40 IYY = NDI + 1, NSHR + NDI
               E(IYY) = ARRAY(IYY+2)             
   40       CONTINUE
            WRITE(9,2204) (E(IZZ), IZZ = NDI + 1, NSHR + NDI)
 2204       FORMAT(4X,'E12 = ',E12.5,'  E13 = ',E12.5,'  E23 = ',E12.5)
C
C
C====================================================================
C  Calculate the principal strains and corresponding principal 
C  directions in unsorted order.
C====================================================================
            LSTR = 2
            CALL SPRIND(E,PE,ANPE,LSTR,NDI,NSHR)
            WRITE(9,2205) PE(1), ANPE(1,1), ANPE(1,2), ANPE(1,3)
 2205       FORMAT(4X,'PE1 = ',E12.5,/,
     1             5X,'PD11 =',F8.3,2X,'PD12 =',F8.3,2X,'PD13 =',F8.3)
            WRITE(9,2206) PE(2), ANPE(2,1), ANPE(2,2), ANPE(2,3)
 2206       FORMAT(4X,'PE2 = ',E12.5,/,
     1             5X,'PD21 =',F8.3,2X,'PD22 =',F8.3,2X,'PD23 =',F8.3)
            WRITE(9,2207) PE(3), ANPE(3,1), ANPE(3,2), ANPE(3,3)
 2207       FORMAT(4X,'PE3 = ',E12.5,/,
     1             5X,'PD31 =',F8.3,2X,'PD32 =',F8.3,2X,'PD33 =',F8.3)
C
         END IF
C
 1000 CONTINUE
 1001 CONTINUE
C
      CLOSE (UNIT=9)
C
      RETURN
      END