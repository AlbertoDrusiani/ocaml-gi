

open CG.GI

let glibLib = {
    name = "GLib";
    version = "2.0";
    overridesFile = Some "overrides/GLib.overrides";
}


let gobjectLib = {
    name = "GObject";
    version = "2.0";
    overridesFile = Some "overrides/GObject.overrides";
}


let gioLib = {
    name = "Gio";
    version = "2.0";
    overridesFile = Some "overrides/Gio.overrides";
}


let atkLib = {
    name = "Atk";
    version = "1.0";
    overridesFile = Some "overrides/Atk.overrides";
}


let cairoLib = {
    name = "cairo";
    version = "1.0";
    overridesFile = Some "overrides/cairo.overrides";
}


let pangoLib = {
    name = "Pango";
    version = "1.0";
    overridesFile = Some "overrides/Pango.overrides";
}


let gdkPixbufLib = {
    name = "GdkPixbuf";
    version = "2.0";
    overridesFile = Some "overrides/GdkPixbuf.overrides";
}


let gdkLib = {
    name = "Gdk";
    version = "3.0";
    overridesFile = Some "overrides/Gdk.overrides";
}


let gtkLib = {
    name = "Gtk";
    version = "3.0";
    overridesFile = Some "overrides/Gtk.overrides";
}



let gtkSourceLib = {
    name = "GtkSource";
    version = "3.0";
    overridesFile = Some "overrides/GtkSource.overrides";
}


let main =
  (*let _ = raise (Failure ("prova")) in*)
  genBindings true gobjectLib;;


main
