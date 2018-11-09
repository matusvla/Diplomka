! module cmdlineloader
! (c) Vladislav Matus
! last edit: 09. 11. 2018      

      module cmdlineloader
        implicit none        
        integer, parameter :: MATRIXTYPE_MAXLEN = 100 !length of string matrixpath         
      contains

!--------------------------------------------------------------------  
! subroutine getCmdlineArgs
! (c) Vladislav Matus
! last edit: 09. 11. 2018  
!
! Purpose: 
!   Load command line arguments       
!   
! Input: none
!   
! Output:
!   matrixtype
!   
! Allocations: none
!

      subroutine getCmdlineArgs(matrixtype)
        implicit none
!
! parameters
!
      character*(MATRIXTYPE_MAXLEN) matrixtype
!
! internals
!              

!
! start of getCmdlineArgs
!	      
      call GETARG(1, matrixtype)
!
! end of getCmdlineArgs
!  
      end subroutine getCmdlineArgs
!--------------------------------------------------------------------    
    end module cmdlineloader
