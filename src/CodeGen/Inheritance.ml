(*open GIR.BasicTypes
open GIR.Property


class 

let removeDuplicates verbose inheritables =
  let filterTainted xs =
    match xs with
    | [ (_, (_, name, prop)) ] -> [(name, prop)]

  in let filterDups m (name, prop) =
    match StringMap.find_opt prop.iName*)

(*let fullInterfacePropertyList n i =
  fullInterfaceInheritableList n i removeDuplicates true*)