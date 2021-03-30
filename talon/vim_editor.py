
from talon import Context, actions
import os

context = Context()
context.matches = "title: /VIM/i"

vim_normal_mode = actions.user.vim_normal_mode
# # Provide simple vim_normal_mode if the user does not already use vim
# try:
#     actions.user.vim_normal_mode()
# except KeyError:
#     print("VIM: using simple insert command")
#     def vim_normal_mode(cmd: str):
#         actions.key("escape")
#         actions.sleep(0.2)
#         actions.insert(cmd)
# except Exception:
#     print("VIM: using fidget")
#     vim_normal_mode = actions.user.vim_normal_mode

@context.action_class("user")
class editor_actions:

    def editor_get_cursor_position():
        title = actions.win.title()
        return eval(title.split(" | ")[-2])

    def editor_go_to_position(row: int, column: int):
        horizontal_movement = f"{column}l" if column > 0 else ""
        vim_normal_mode(f"{row}G0{horizontal_movement}")

    def editor_select_range(line1: int, column1: int, line2: int, column2: int):
        actions.user.editor_go_to_position(line1, column1)
        vertical_movement = f"{line2-line1}j" if line2-line1 > 0 else ""
        horizontal_movement = f"{column2}l" if column2 > 0 else ""
        actions.insert(f"v{vertical_movement}0{horizontal_movement}")


