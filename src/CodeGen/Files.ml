(*open API*)
open GIR.BasicTypes

let noCType =
  NameSet.of_list [{namespace = "Gtk"; name = "HeaderBarAccessible"}]



let noCheckMacro =
  NameSet.of_list [{namespace = "Pango"; name = "Coverage"}]


let buggedIfaces =
  NameSet.of_list [
      {namespace = "Gtk"; name = "TreeModel"};
      {namespace = "Gtk"; name = "CellLayout"};
      {namespace = "Gtk"; name = "StyleProvider"};
      ]

let excludeFiles =
  NameSet.of_list [
      {namespace = "Gtk";       name = "HeaderBarAccessible"};
      {namespace = "Gtk";       name = "EntryIconAccessible"};
      {namespace = "Gtk";       name = "CellAccessibleParent"};
      {namespace = "Gtk";       name = "TreeViewAccessible"};
      {namespace = "Gtk";       name = "Widget"};
      {namespace = "Gtk";       name = "FileChooserButton"};
      {namespace = "Pango";     name = "Engine"};
      {namespace = "Pango";     name = "EngineShape"};
      {namespace = "Pango";     name = "EngineLang"};
      {namespace = "Pango";     name = "FontsetSimple"};
      {namespace = "GtkSource"; name = "Gutter"};
      {namespace = "GtkSource"; name = "View"};
      {namespace = "GtkSource"; name = "Map"};
      {namespace = "GtkSource"; name = "Mark"};
      {namespace = "GtkSource"; name = "LanguageManager"};
      {namespace = "GtkSource"; name = "StyleSchemeManager"};
      {namespace = "Gio";       name = "ApplicationCommandLine"};
      {namespace = "Gio";       name = "ListStore"};
      {namespace = "Gio";       name = "MenuLinkIter"};
      {namespace = "Gio";       name = "SimpleProxyResolver"};
      {namespace = "Atk";       name = "Component"};
      {namespace = "Atk";       name = "Image"};
      {namespace = "Atk";       name = "Text"};
      {namespace = "Atk";       name = "EditableText"};
      {namespace = "Atk";       name = "Table"};
      {namespace = "Gdk";       name = "Window"};
  ]