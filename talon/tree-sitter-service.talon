
# Insertion
call {user.symbol_function}: user.code_insert_function(symbol_function, "")
symbol <user.symbol_identifier>: insert(symbol_identifier)


# Navigation
go imports: user.symbol_navigate("", "import")
go to <user.symbol_identifier>: user.symbol_navigate("{symbol_identifier}", "identifier")
go type {user.symbol_type}: user.symbol_navigate("{symbol_type}", "type")
go fun {user.symbol_function}: user.symbol_navigate("{symbol_function}", "function")

go to next: user.go_to_next_result("forward")
go to last: user.go_to_next_result("backward")

# Editing
select fun {user.symbol_function}: user.symbol_select("{symbol_function}", "function")
select type {user.symbol_type}: user.symbol_select("{symbol_type}", "type")
delete fun {user.symbol_function}: 
    user.symbol_select("{symbol_function}", "function")
    "d"
delete type {user.symbol_type}: 
    user.symbol_select("{symbol_type}", "type")
    "d"

# TODO: implement server-side
select function: user.symbol_select_parent("value_declaration")


# Miscellaneous
print cursor: user.print_cursor()
print symbols: user.print_symbols()
clear symbols: user.clear_symbols()
