from talon import actions, Context, Module, fs, app
from typing import Dict
import json
import requests
import os
import sys

module = Module()
context = Context()

PORT = 8080
BASE_URL = f"http://localhost:{PORT}"

def request_GET(url: str):
    url = BASE_URL + url
    print(url)
    response = requests.get(url, timeout=(0.05, 3.05))
    response.raise_for_status()
    return json.loads(response.text)

def document_query(target: str, type_: str):
    line, column = actions.user.editor_get_cursor_position()
    return request_GET(f"/document-query?target={target}&type={type_}&line={line}&column={column}")

def next_result(direction):
    return request_GET(f"/cycle-result?direction={direction}")

def navigate_to_node_start(node):
    actions.user.editor_go_to_position(node["startPosition"]["row"]+1, node["startPosition"]["column"])

@module.action_class
class editor_actions:

    def editor_get_cursor_position() -> int:
        "get the position of the cursor. e.g: (12, 13)"

    def editor_go_to_position(row: int, column: int):
        "navigate to the given position in the editor"

    def editor_select_range(line1: int, column1: int, line2: int, column2: int):
        "marks the given range in the editor"

@module.action_class
class Actions:

    def symbol_select_parent(target: str):
        "select the closest parent from the node under the cursor with the given type"
        line, column = actions.user.editor_get_cursor_position()
        node = request_GET(f"/select-parent?target={target}&line={line}&column={column}")
        navigate_to_node_start(node)

    def go_to_next_result(direction: str = "forward"):
        "navigate to the next match for the previous query"
        node = next_result(direction)
        navigate_to_node_start(node)

    def symbol_select(target: str, type_: str):
        "select a node in the file"
        node = document_query(target, type_)
        start = node["startPosition"]
        end = node["endPosition"]
        actions.user.editor_select_range(start["row"]+1, start["column"], end["row"]+1, end["column"])

    def symbol_navigate(target: str, type_: str):
        "move the cursor to a node in the file"
        node = document_query(target, type_)
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
        # @module.capture(rule=f'({("|".join(captures))})')
        # def symbol_identifier(m) -> str: return m

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

#
# Windows workaround for IPC
# Much slower than Unix implementation
#

WINDOWS_IPC_PATH = "C:\\talon-tss-ipc"

def windows_ipc(path, flags):
    print("IPC recieved command from: ", path)
    if flags.exists:
        with open(path) as f:
            eval(f.read())
        
def setup_windows_ipc():
    print("platform: ", sys.platform)
    if sys.platform != "win32": return
    try:
        os.mkdir(WINDOWS_IPC_PATH)
    except Exception as e:
        print(e)
    fs.watch(WINDOWS_IPC_PATH, windows_ipc)

setup_windows_ipc()

# Notes:
# update context from repl
# >>> user.thesis.context.lists["self.test"]={"test":"!"}
# get index (offset by 1): :echo wordcount().cursor_chars 
