! module metisinterface
! (c) Vladislav Matus
! source of inspiration: http://glaros.dtc.umn.edu/gkhome/node/877 
! last edit: 22. 07. 2018

module metis_interface
use,intrinsic::ISO_C_BINDING

integer(C_INT)::metis_ne,metis_nn
integer(C_INT)::ncommon,objval
integer(C_INT)::nparts
integer(C_INT),allocatable,dimension(:)::eptr,eind,permM,iperm
integer(C_INT),allocatable,dimension(:)::epart,npart
type(C_PTR)::vwgt,vsize,twgts
integer(C_INT)::opts(0:40)

interface
  integer(C_INT) function METIS_SetDefaultOptions(options) bind(C,name="METIS_SetDefaultOptions")
    use,intrinsic::ISO_C_BINDING
    implicit none
    integer(C_INT)::options(0:40)
  end function METIS_SetDefaultOptions
end interface

interface
  integer(C_INT) function METIS_NodeND(nvtxs,xadj,adjncy,vwgt,opts,permM,iperm) bind(C,name="METIS_NodeND")
    use,intrinsic::ISO_C_BINDING
    implicit none
    integer(C_INT)::nvtxs
    integer(C_INT),dimension(*)::xadj,adjncy,permM,iperm
    type(C_PTR),value::vwgt
    integer(C_INT)::opts(0:40)
  end function METIS_NodeND
end interface

interface
  integer(C_INT) function METIS_PartGraphRecursive &
    (nvtxs,ncon,xadj,adjncy,vwgt,vsize,adjwgt,nparts,tpwgts,ubvec,options,objval,part) &
    bind(C,name="METIS_PartGraphRecursive")
    use,intrinsic::ISO_C_BINDING
    implicit none
    integer(C_INT)::nvtxs,ncon,nparts,objval
    integer(C_INT),dimension(*)::xadj,adjncy,part
    type(C_PTR),value::vwgt,vsize,adjwgt,tpwgts,ubvec
    integer(C_INT)::options(0:40)
  end function METIS_PartGraphRecursive
end interface

!METIS_ComputeVertexSeparator needs graph with C++ numbering (from 0) without loops
interface
  integer(C_INT) function METIS_ComputeVertexSeparator &
    (nvtxs,xadj,adjncy,vwgt,options,r_sepsize,part) &
    bind(C,name="METIS_ComputeVertexSeparator")
    use,intrinsic::ISO_C_BINDING
    implicit none
    integer(C_INT)::nvtxs,r_sepsize
    integer(C_INT),dimension(*)::xadj,adjncy,part
    type(C_PTR),value::vwgt
    integer(C_INT)::options(0:40)
  end function METIS_ComputeVertexSeparator
end interface

end module metis_interface