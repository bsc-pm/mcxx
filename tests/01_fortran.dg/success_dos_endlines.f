! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      PROGRAM DOS_ENDINGS
          PRINT *, "ONCE UPON"
     &," A TIME THERE WAS A SMALL OS CALLED DOS"
9001    FORMAT (8X,'DATE',3X,'IREC SRCID',6X,'DISTDOM',4X,'MAXCONC',2X,&
     &           'NUMCONT  O3CONC',6X,'O3MOLES',5X,'NOXMOLES',4X,       &
     &           'BHORIZ',6X,'BVERT',4X,'PLUMEVOL',3X,'PercentNO2')
      END PROGRAM DOS_ENDINGS
