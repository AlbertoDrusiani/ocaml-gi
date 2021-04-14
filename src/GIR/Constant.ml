open Type
open Parser
open BasicTypes
open Documentation
open Deprecation

type constant = {
    constantType: type_ml;
    constantValue: string;
    constantCType: string;
    constantDocumentation: documentation;
    constantDeprecated: deprecation_info option;
}


let parseConstant el ns =
  prerr_endline ("Inizio il parse Constant");
  let name = parseName ns el in
  name, 
  {constantType = parseType el ns;
   constantValue = getAttr "value" el;
   constantCType = parseCType el;
   constantDocumentation = parseDocumentation el;
   constantDeprecated = parseDeprecation el;
  }

