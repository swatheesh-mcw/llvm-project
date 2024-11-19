! RUN: %flang_fc1 -fdebug-unparse -fopenmp %s | FileCheck --ignore-case --check-prefix="UNPARSE" %s 
! RUN: %flang_fc1 -fdebug-dump-parse-tree-no-sema -fopenmp %s | FileCheck --check-prefix="PARSE-TREE" %s

subroutine main()
  !$omp interop device(1)
  print *,'pass'
end

!UNPARSE: SUBROUTINE main
!UNPARSE:  INTEROP  DEVICE(1_4)
!UNPARSE:  PRINT *, "pass"
!UNPARSE: END SUBROUTINE

!PARSE-TREE: | SubroutineStmt
!PARSE-TREE: | | Name = 'main'
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPInteropConstruct
!PARSE-TREE: | | | Verbatim
!PARSE-TREE: | | | OmpClauseList -> OmpClause -> Device -> OmpDeviceClause
!PARSE-TREE: | | | | Scalar -> Integer -> Expr -> LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> PrintStmt
!PARSE-TREE: | | | Format -> Star
!PARSE-TREE: | | | OutputItem -> Expr -> LiteralConstant -> CharLiteralConstant
!PARSE-TREE: | | | | string = 'pass'
!PARSE-TREE: | EndSubroutineStmt -> 


subroutine main2()
  integer,parameter :: obj = 1
  !$omp interop init(targetsync: obj) use(obj) destroy(obj)
  print *,'pass'
end

!UNPARSE: SUBROUTINE main2
!UNPARSE:  INTEGER, PARAMETER :: obj = 1_4
!UNPARSE:  INTEROP  INIT(TARGETSYNC:obj) USE(obj) DESTROY(obj)
!UNPARSE:  PRINT *, "pass"
!UNPARSE: END SUBROUTINE

!PARSE-TREE: | SubroutineStmt
!PARSE-TREE: | | Name = 'main2'
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | | DeclarationConstruct -> SpecificationConstruct -> TypeDeclarationStmt
!PARSE-TREE: | | | DeclarationTypeSpec -> IntrinsicTypeSpec -> IntegerTypeSpec -> 
!PARSE-TREE: | | | AttrSpec -> Parameter
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'obj'
!PARSE-TREE: | | | | Initialization -> Constant -> Expr -> LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPInteropConstruct
!PARSE-TREE: | | | Verbatim
!PARSE-TREE: | | | OmpClauseList -> OmpClause -> Init -> OmpInitClause
!PARSE-TREE: | | | | InteropTypes -> InteropType -> Kind = TargetSync
!PARSE-TREE: | | | | InteropVar -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | | OmpClause -> Use -> OmpUseClause -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | | OmpClause -> Destroy -> OmpDestroyClause -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> PrintStmt
!PARSE-TREE: | | | Format -> Star
!PARSE-TREE: | | | OutputItem -> Expr -> LiteralConstant -> CharLiteralConstant
!PARSE-TREE: | | | | string = 'pass'
!PARSE-TREE: | EndSubroutineStmt -> 

subroutine main3()
  Integer,parameter :: obj = 1
  !$omp interop init(targetsync: obj) depend(inout: obj)
  print *,'pass'
end

!UNPARSE: SUBROUTINE main3
!UNPARSE:  INTEGER, PARAMETER :: obj = 1_4
!UNPARSE:  INTEROP  INIT(TARGETSYNC:obj) DEPEND((INOUT:obj))
!UNPARSE:  PRINT *, "pass"
!UNPARSE: END SUBROUTINE

!PARSE-TREE: | SubroutineStmt
!PARSE-TREE: | | Name = 'main3'
!PARSE-TREE: | SpecificationPart
!PARSE-TREE: | | ImplicitPart -> 
!PARSE-TREE: | | DeclarationConstruct -> SpecificationConstruct -> TypeDeclarationStmt
!PARSE-TREE: | | | DeclarationTypeSpec -> IntrinsicTypeSpec -> IntegerTypeSpec -> 
!PARSE-TREE: | | | AttrSpec -> Parameter
!PARSE-TREE: | | | EntityDecl
!PARSE-TREE: | | | | Name = 'obj'
!PARSE-TREE: | | | | Initialization -> Constant -> Expr -> LiteralConstant -> IntLiteralConstant = '1'
!PARSE-TREE: | ExecutionPart -> Block
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> OpenMPConstruct -> OpenMPInteropConstruct
!PARSE-TREE: | | | Verbatim
!PARSE-TREE: | | | OmpClauseList -> OmpClause -> Init -> OmpInitClause
!PARSE-TREE: | | | | InteropTypes -> InteropType -> Kind = TargetSync
!PARSE-TREE: | | | | InteropVar -> OmpObject -> Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | | OmpClause -> Depend -> OmpDependClause -> InOut
!PARSE-TREE: | | | | OmpDependenceType -> Type = Inout
!PARSE-TREE: | | | | Designator -> DataRef -> Name = 'obj'
!PARSE-TREE: | | ExecutionPartConstruct -> ExecutableConstruct -> ActionStmt -> PrintStmt
!PARSE-TREE: | | | Format -> Star
!PARSE-TREE: | | | OutputItem -> Expr -> LiteralConstant -> CharLiteralConstant
!PARSE-TREE: | | | | string = 'pass'
!PARSE-TREE: | EndSubroutineStmt -> 
