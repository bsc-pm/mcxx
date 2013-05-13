! <testinfo>
! test_generator=config/mercurium-fortran
! </testinfo>
      PROGRAM MAIN
          integer proc_num_threads(10)
               if (mz .gt. 0) then
                  cur_size = proc_zone_size(ip) / proc_num_threads(ip)
                  if (cur_size.gt.max_size .and. (max_threads.le.0
     & 	              .or. proc_num_threads(ip).lt.max_threads)) then
                     max_size = cur_size
                     imx = ip
     	          endif
     	       else if (proc_num_threads(ip) .gt. 1) then
                  cur_size = proc_zone_size(ip) / 
     &	             	     (proc_num_threads(ip)-1)
     	          if (max_size.eq.0 .or. cur_size.lt.max_size) then
     	             max_size = cur_size
     	             imx = ip
     	          endif
     	       endif
      END PROGRAM MAIN
