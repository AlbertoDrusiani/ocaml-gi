type allocation_op =
  | AllocationOpUnknown
  | AllocationOp of string
      


type allocation_info = {
  allocCalloc: allocation_op;
  allocCopy: allocation_op;
  allocFree: allocation_op;
}


let unknownAllocationInfo =
  { allocCalloc = AllocationOpUnknown;
    allocCopy = AllocationOpUnknown;
    allocFree = AllocationOpUnknown;
  }
