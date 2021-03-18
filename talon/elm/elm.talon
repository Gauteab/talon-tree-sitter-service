mode: user.elm
mode: command 
and code.language: elm
-

# Type Declaration
state type: "type "
[state] type alias: "type alias "

# Type Insertion
of type: " : "
of type <user.elm_type>: 
    " : "
    "{elm_type}" 
to [type] <user.elm_type>: " -> {elm_type} "
[type] <user.elm_type> to: " {elm_type} -> "
type <user.elm_type>: "{elm_type} "

