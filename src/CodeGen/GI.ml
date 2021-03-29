





(*type library = {
    name: string;
    version: string;
    overridesFile: string;
}

(*per ora non considero gli overrides*)
let genLibraryCode name version =
    let gir, girDeps = loadGIRInfo name in
*)
