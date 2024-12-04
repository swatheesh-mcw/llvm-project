!RUN: not %flang_fc1 -fopenmp %s 2>&1 | FileCheck %s

subroutine main1()
  use omp_lib
  integer(omp_interop_kind) :: obj
  !CHECK: error: Each interop-var may be speciﬁed for at most one action-clause of each interop construct.
  !$omp interop init(targetsync,target: obj) use(obj)
  print *,'pass'
end

subroutine main2()
  use omp_lib
  integer(omp_interop_kind) :: obj
  !CHECK: error: Each interop-type may be speciﬁed at most once.
  !$omp interop init(targetsync,target,targetsync: obj)
  print *,'pass'
end

subroutine main3()
  use omp_lib
  integer(omp_interop_kind) :: obj
  !CHECK: error: A depend clause can only appear on the directive if the interop-type includes targetsync
  !$omp interop init(target: obj) depend(inout: obj)
  print *,'pass'
end