! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
MODULE ParameterDefinitionen
  INTEGER, PARAMETER :: real_normal = SELECTED_REAL_KIND(10)
  INTEGER, PARAMETER :: real_long   = MAX(real_normal, SELECTED_REAL_KIND(15))
  INTEGER, PARAMETER :: int_normal  = SELECTED_INT_KIND(8)
  INTEGER, PARAMETER :: int_short   = SELECTED_INT_KIND(4)
  INTEGER, PARAMETER :: int_long    = SELECTED_INT_KIND(12)
  INTEGER, PARAMETER :: char_normal = 256
  INTEGER, PARAMETER :: char_short  = 16
  INTEGER, PARAMETER :: char_long   = 1024
  REAL(KIND=real_normal), PARAMETER :: pi = REAL(3.14159265358979D0,KIND=real_normal)
  REAL(KIND=real_long),   PARAMETER :: PI_long = REAL(3.141592653589793238D0,KIND=real_long)
  REAL(KIND=real_normal), PARAMETER :: zweiPi = REAL(2.0D0, KIND=real_normal)*pi
  REAL(KIND=real_normal), PARAMETER :: piHalbe = pi/REAL(2.0D0, KIND=real_normal)
  REAL(KIND=real_normal), PARAMETER :: grad2RadKonst = pi/REAL(180.0D0, KIND=real_normal)
  REAL(KIND=real_normal), PARAMETER :: Rad2GradKonst = REAL(180.0D0, KIND=real_normal)/pi
  REAL(KIND=real_normal), PARAMETER :: euler = REAL(2.718281828459D0,KIND=real_normal)
  REAL(KIND=real_normal), PARAMETER :: realNull = REAL(0.0D0,KIND=real_normal)
  REAL(KIND=real_normal), PARAMETER :: realEins = REAL(1.0D0,KIND=real_normal)
  REAL(KIND=real_normal), PARAMETER :: infinity = 0.1D0*HUGE(realEins)
  INTEGER, PARAMETER :: maxPath = 260
  REAL(KIND=real_normal), PARAMETER :: grenze_klein =  2.0D0*10.0D0**(-PRECISION(realeins))
  REAL(KIND=real_normal), PARAMETER :: grenze_normal = 10.0D0*10.0D0**(-PRECISION(realeins))
  REAL(KIND=real_normal), PARAMETER :: grenze_gross = 100.0D0*10.0D0**(-PRECISION(realeins))
  REAL(KIND=real_normal), PARAMETER :: pointDistance = 5.0*10E-4
  INTEGER, PARAMETER :: ALL_LOG   = -2147483647
  INTEGER, PARAMETER :: TRACE_LOG = 5000
  INTEGER, PARAMETER :: DEBUG_LOG = 10000
  INTEGER, PARAMETER :: INFO_LOG  = 20000
  INTEGER, PARAMETER :: WARN_LOG  = 30000
  INTEGER, PARAMETER :: ERROR_LOG = 40000
  INTEGER, PARAMETER :: FATAL_LOG = 50000
  INTEGER, PARAMETER :: OFF_LOG   = 2147483647
  INTEGER, PARAMETER :: konstDefault = 0, konstDegrees = 1, konstRadians = 2
  CHARACTER(LEN=1), PARAMETER :: PATHSEPAR='/'
  INTEGER :: current_anzahl_OMP_Threads = 1
  INTEGER :: buildBSPTreeListOMP_modus = 7
  INTEGER :: buildBSPTreeListOMP_menge = 3
  LOGICAL :: buildBSPTreeListOMP_zeitmessung = .TRUE.
  INTEGER :: boxLoopSchleifeOMP_modus = 7
  INTEGER :: boxLoopSchleifeOMP_menge = 4
  LOGICAL :: boxLoopSchleifeOMP_zeitmessung = .TRUE.
  INTEGER :: balancierungsTiefe = 0
  INTEGER :: taskAbschneidTiefe = 1000000
  LOGICAL :: arrayEnbettungPolyListe = .TRUE.
  LOGICAL :: subtractTriStrip_zeitmessung = .TRUE.
END MODULE ParameterDefinitionen
