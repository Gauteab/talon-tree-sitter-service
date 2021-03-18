
import re

from talon import Context, Module, actions, settings, ctrl

module = Module()
context = Context()
context.matches = r"""
mode: user.elm
mode: command
and code.language: elm
"""

module.list("elm_type", "elm types")
context.lists["user.elm_type"] = {
    # Basic
    "car": "Char",
    "character": "Char",
    "boolean": "Bool",
    "string": "String",
    "float": "Float",
    "integer": "Int",
    "hint": "Int",
    # Special
    "number": "number",
    "comparable": "comparable",
    "appendable": "appendable",
    "unit": "()",
    # Complex
    "list": "List",
    "array": "Array",
    "maybe": "Maybe",
    "CMD": "Cmd",
    "sub": "Sub",
    "task": "Task",
    "dictionary": "Dict",
    "set": "Set",
    "result": "Result",
}

@module.capture(rule="({self.elm_type}|{user.symbol_type})+")
def elm_type(m) -> str: return m

@context.action_class("user")
class code_actions:
    def code_insert_function(text: str, selection: str):
        actions.insert(text + " ")

