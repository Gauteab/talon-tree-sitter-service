from talon import actions, Context, Module
import http.client
from typing import Dict
import json

module = Module()
context = Context()

PORT = 8080
BASE_URL = f"localhost:{PORT}"

def request_GET(url: str):
    connection = http.client.HTTPConnection(BASE_URL)
    connection.request("GET", url)
    response = connection.getresponse()
    print(f"Status: {response.status} ({response.reason})")
    result = json.loads(response.read()) if response.status == 200 else None
    connection.close()
    return result

def document_query(file: str, target: str, type_: str):
    line, column = actions.user.editor_get_cursor_position()
    file = actions.user.editor_get_file_path()
    return request_GET(f"/document-query?file&target={target}&type={type_}&file={file}&line={line}&column={column}")

def next_result(direction):
    return request_GET(f"/cycle-result?direction={direction}")

def navigate_to_node_start(node):
    actions.user.editor_go_to_position(node["startPosition"]["row"]+1, node["startPosition"]["column"])

@module.action_class
class editor_actions:

    def editor_get_cursor_position() -> int:
        "get the position of the cursor. e.g: (12, 13)"

    def editor_get_file_path() -> str:
        "get the full file path for the file being currently edited"

    def editor_go_to_position(row: int, column: int):
        "navigate to the given position in the editor"

    def editor_select_range(line1: int, column1: int, line2: int, column2: int):
        "marks the given range in the editor"

@module.action_class
class Actions:

    def symbol_select_parent(target: str):
        "select the closest parent from the node under the cursor with the given type"
        line, column = actions.user.editor_get_cursor_position()
        file = actions.user.editor_get_file_path()
        node = request_GET(f"/select-parent?target={target}&file={file}&line={line}&column={column}")
        navigate_to_node_start(node)

    def go_to_next_result(direction: str = "forward"):
        "navigate to the next match for the previous query"
        node = next_result(direction)
        navigate_to_node_start(node)

    def symbol_select(target: str, type_: str):
        "select a node in the file"
        file = actions.user.editor_get_file_path()
        node = document_query(file, target, type_)
        start = node["startPosition"]
        end = node["endPosition"]
        actions.user.editor_select_range(start["row"]+1, start["column"], end["row"]+1, end["column"])

    def symbol_navigate(target: str, type_: str):
        "move the cursor to a node in the file"
        file = actions.user.editor_get_file_path()
        node = document_query(file, target, type_)
        navigate_to_node_start(node)

    def update_symbols(dictionary: Dict[str, Dict[str,str]]) -> str:
        "update the symbols"
        captures = []
        for (name, v) in dictionary.items():
            list_name = f"symbol_{name}"
            captures.append(f"{{user.{list_name}}}")
            module.list(list_name, name)
            context.lists[f"self.{list_name}"] = v

        # create capture on the fly
        @module.capture(rule=f'({("|".join(captures))})')
        def symbol_identifier(m) -> str: return m

    def print_cursor():
        "print the current cursor position"
        cursor = actions.user.editor_get_cursor_position()
        print(cursor)

    def print_symbols():
        "Print the symbols"
        print(context.lists)

    def clear_symbols():
        "Clears the Symbols"
        print("clearing symbols")
        context.lists = {}

# Notes:
# update context from repl
# >>> user.thesis.context.lists["self.test"]={"test":"!"}
# get index (offset by 1): :echo wordcount().cursor_chars 
