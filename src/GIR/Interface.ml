(*

type interface ={ 
    ifTypeInit: string option;
    ifCType: string option;
   (* ifDocumentation: documentation;*)
    ifPrerequisites: name list ref;
    ifProperties: property list ref;
    ifSignals: signal list ref;
    ifMethods: method_ml list ref;
   (* ifAllocationInfo: allocation_info;*)
    ifDeprecated: bool;
    }

*)
