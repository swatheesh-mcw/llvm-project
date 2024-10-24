! RUN: %flang_fc1 -fopenmp %s
! Compilation error of Fixed Source Form Conditional Compilation Sentinels #89560
! CHECK:      print *,'pass'
! CHECK:    program main
! CHECK:    k01=-1
! CHECK:c$ x
! CHECK:    print *,'pass'
! CHECK:    end
    program main
    k01=-1
c$ x
    print *,'pass'
    end