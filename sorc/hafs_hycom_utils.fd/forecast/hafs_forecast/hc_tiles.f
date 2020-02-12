      SUBROUTINE ASSEMBLE_cc(FG,FL)

      USE mod_xc
      
      USE CMP_COMM, ONLY: component_master_rank_local

      implicit none

      real, intent(out) :: FG(itdm,jtdm)
      real, intent(in)  :: FL(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)

      real a
C

      a=vland
      vland=-1.E30

      call xcaget(FG,FL,component_master_rank_local+1)

      vland=a

      return
      END
C
C***********************************************************************
C
      SUBROUTINE DISASSEMBLE_cc(FG,FL)

      USE mod_xc
      
      USE CMP_COMM, ONLY: component_master_rank_local

      implicit none

      real, intent(inout) :: FG(itdm,jtdm)
                     !<- somehow they want it to be broadcastable
      real, intent(out)   :: FL(1-nbdy:idm+nbdy,1-nbdy:jdm+nbdy)
C

      call xcaput(FG,FL,component_master_rank_local+1)

      return
      END
