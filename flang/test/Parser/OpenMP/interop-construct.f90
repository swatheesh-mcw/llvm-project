! RUN: %flang_fc1 -fdebug-unparse -fopenmp %s | FileCheck --ignore-case --check-prefix="UNPARSE" %s 
! RUN: %flang_fc1 -fdebug-dump-parse-tree-no-sema -fopenmp %s | FileCheck --check-prefix="PARSE-TREE" %s

subroutine main()
  !$omp interop device(1)
  print *,'pass'
end

!UNPARSE: SUBROUTINE main
!UNPARSE: !$OMP INTEROP  DEVICE(1_4)
!UNPARSE:  PRINT *, "pass"
!UNPARSE: END SUBROUTINE

!PARSE-TREE: | SubroutineStmt
!PARSE-TREE: | | Name = 'main'
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPStandaloneConstruct -> OpenMPInteropConstruct
!PARSE-TREE: | | | Verbatim
!PARSE-TREE: | | | OmpClauseList -> OmpClause -> Device -> OmpDeviceClause
!PARSE-TREE: | | | | Scalar -> Integer -> Expr -> LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> PrintStmt
!PARSE-TREE: | | | Format -> Star
!PARSE-TREE: | | | OutputItem -> Expr -> LiteralConstant -> CharLiteralConstant
!PARSE-TREE: | | | | string = 'pass'
!PARSE-TREE: | EndSubroutineStmt -> 

subroutine main2()
  use omp_lib
  integer(omp_interop_kind) :: obj1, obj2, obj3
  !$omp interop init(targetsync: obj) use(obj1) destroy(obj3)
  print *,'pass'
end

!UNPARSE: SUBROUTINE main2
!UNPARSE:  USE :: omp_lib
!UNPARSE:  INTEGER(KIND=8_4) obj1, obj2, obj3
!UNPARSE: !$OMP INTEROP INIT(TARGETSYNC: obj) USE(obj1) DESTROY(obj3)
!UNPARSE:  PRINT *, "pass"
!UNPARSE: END SUBROUTINE

!PARSE-TREE: | SubroutineStmt
!PARSE-TREE: | | Name = 'main2'
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | UseStmt
!PARSE-TREE: | | | Name = 'omp_lib'
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | | DeclarationConstruct -> SpecificationConstruct -> TypeDeclarationStmt
!PARSE-TREE: | | | DeclarationTypeSpec -> IntrinsicTypeSpec -> IntegerTypeSpec -> KindSelector -> Scalar -> Integer -> Constant -> Expr -> Designator -> DataRef -> Name = 'omp_interop_kind'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'obj1'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'obj2'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'obj3'
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPStandaloneConstruct -> OpenMPInteropConstruct
!PARSE-TREE: | | | Verbatim
!PARSE-TREE: | | | OmpClauseList -> OmpClause -> Init -> OmpInitClause
!PARSE-TREE: | | | | InteropTypes -> InteropType -> Kind = TargetSync
!PARSE-TREE: | | | | InteropVar -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | | OmpClause -> Use -> OmpUseClause -> OmpObject -> Designator -> DataRef -> Name = 'obj1'
!PARSE-TREE: | | | OmpClause -> Destroy -> OmpDestroyClause -> OmpObject -> Designator -> DataRef -> Name = 'obj3'
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> PrintStmt
!PARSE-TREE: | | | Format -> Star
!PARSE-TREE: | | | OutputItem -> Expr -> LiteralConstant -> CharLiteralConstant
!PARSE-TREE: | | | | string = 'pass'
!PARSE-TREE: | EndSubroutineStmt ->

subroutine main3()
  use omp_lib
  Integer(omp_interop_kind) :: obj
  !$omp interop init(targetsync: obj) depend(inout: obj)
  print *,'pass'
end

!UNPARSE: SUBROUTINE main3
!UNPARSE:  USE :: omp_lib
!UNPARSE:  INTEGER(KIND=8_4) obj
!UNPARSE: !$OMP INTEROP INIT(TARGETSYNC: obj) DEPEND(INOUT:obj)
!UNPARSE:  PRINT *, "pass"
!UNPARSE: END SUBROUTINE


!PARSE-TREE: | SubroutineStmt
!PARSE-TREE: | | Name = 'main3'
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | UseStmt
!PARSE-TREE: | | | Name = 'omp_lib'
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | | DeclarationConstruct -> SpecificationConstruct -> TypeDeclarationStmt
!PARSE-TREE: | | | DeclarationTypeSpec -> IntrinsicTypeSpec -> IntegerTypeSpec -> KindSelector -> Scalar -> Integer -> Constant -> Expr -> Designator -> DataRef -> Name = 'omp_interop_kind'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'obj'
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPStandaloneConstruct -> OpenMPInteropConstruct
!PARSE-TREE: | | | Verbatim
!PARSE-TREE: | | | OmpClauseList -> OmpClause -> Init -> OmpInitClause
!PARSE-TREE: | | | | InteropTypes -> InteropType -> Kind = TargetSync
!PARSE-TREE: | | | | InteropVar -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | | OmpClause -> Depend -> OmpDependClause -> TaskDep
!PARSE-TREE: | | | | OmpTaskDependenceType -> Type = Inout
!PARSE-TREE: | | | | OmpObjectList -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> PrintStmt
!PARSE-TREE: | | | Format -> Star
!PARSE-TREE: | | | OutputItem -> Expr -> LiteralConstant -> CharLiteralConstant
!PARSE-TREE: | | | | string = 'pass'
!PARSE-TREE: | EndSubroutineStmt ->

subroutine main4()
  use omp_lib
  integer(omp_interop_kind) :: obj
  !$omp interop init(prefer_type(10),targetsync,target: obj)
  print *,'pass'
end

!UNPARSE: SUBROUTINE main4
!UNPARSE:  USE :: omp_lib
!UNPARSE:  INTEGER(KIND=8_4) obj
!UNPARSE: !$OMP INTEROP INIT(PREFER_TYPE(10_4),TARGETSYNC,TARGET: obj)
!UNPARSE:  PRINT *, "pass"
!UNPARSE: END SUBROUTINE

!PARSE-TREE: | SubroutineStmt
!PARSE-TREE: | | Name = 'main4'
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | UseStmt
!PARSE-TREE: | | | Name = 'omp_lib'
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | | DeclarationConstruct -> SpecificationConstruct -> TypeDeclarationStmt
!PARSE-TREE: | | | DeclarationTypeSpec -> IntrinsicTypeSpec -> IntegerTypeSpec -> KindSelector -> Scalar -> Integer -> Constant -> Expr -> Designator -> DataRef -> Name = 'omp_interop_kind'
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'obj'
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPStandaloneConstruct -> OpenMPInteropConstruct
!PARSE-TREE: | | | Verbatim
!PARSE-TREE: | | | OmpClauseList -> OmpClause -> Init -> OmpInitClause
!PARSE-TREE: | | | | InteropPreferenceList -> Integer -> Constant -> Expr -> LiteralConstant -> IntLiteralConstant = '10'
!PARSE-TREE: | | | | InteropTypes -> InteropType -> Kind = TargetSync
!PARSE-TREE: | | | | InteropType -> Kind = Target
!PARSE-TREE: | | | | InteropVar -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> PrintStmt
!PARSE-TREE: | | | Format -> Star
!PARSE-TREE: | | | OutputItem -> Expr -> LiteralConstant -> CharLiteralConstant
!PARSE-TREE: | | | | string = 'pass'
!PARSE-TREE: | EndSubroutineStmt -> 
