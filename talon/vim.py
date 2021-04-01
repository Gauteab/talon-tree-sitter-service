import time
from talon import Context, Module, actions, settings, ui

mod = Module()
ctx = Context()
ctx.matches = "title: /VIM/i"

mod.list("vim_verbs", "verbs")
ctx.lists["user.vim_verbs"] = {
    "delete": "d",
    "change": "c",
    "comment": "gc",
    "upper": "gU",
    "lower": "gu",
}

mod.list("vim_targets", "targets")
ctx.lists["user.vim_targets"] = {
    "par": "ip",
    "down par": "}",
    "up par": "{",
    "back": "b",
    "back word": "b",
    "big back": "B",
    "end": "e",
    "big end": "E",
    "word": "aw",
    "big word": "aW",
    "back end": "ge",
    "back big end": "gE",
    "right": "l",
    "left": "h",
    "down": "j",
    "up": "k",
    "next": "n",
    "next reversed": "N",
    "previous": "N",
    "column zero": "0",
    "column": "|",
    "start of line": "^",
    "end of line": "$",
    "search under cursor": "*",
    "search under cursor reversed": "#",
    "again": ";",
    "again reversed": ",",
    "down sentence": ")",
    "sentence": ")",
    "up sentence": "(",
    "start of next section": "]]",
    "start of previous section": "[[",
    "end of next section": "][",
    "end of previous section": "[]",
    "matching": "%",
    "cursor home": "H",
    "cursor middle": "M",
    "cursor last": "L",
    "start of file": "gg",
    "top of file": "gg",
    "end of file": "G",
    "line": "line"
}

@mod.action_class
class Actions:
    def vim_do_action(verb:str, target:str, number:int = None):
        ""
        target = target if target != "line" else verb[-1]
        cmd = f"{verb}{number if number else ''}{target}"
        actions.user.vim_normal_mode_np(cmd)

@ctx.action_class("win")
class win_actions:
    def filename():
        title = actions.win.title()
        result = title.split("|")[-1]
        return result

    def file_ext():
        ext = "." + actions.win.filename().split(".")[-1]
        # print(ext)
        return ext


mod.setting( "vim_preserve_insert_mode", type=int, default=1, desc="If normal mode actions are called from insert mode, stay in insert",)
mod.setting( "vim_adjust_modes", type=int, default=1, desc="User wants talon to automatically adjust modes for commands",)
mod.setting( "vim_notify_mode_changes", type=int, default=0, desc="Notify user about vim mode changes as they occur",)
mod.setting( "vim_escape_terminal_mode", type=int, default=0, desc="When set won't limit what motions and commands will pop out of terminal mode",)
mod.setting( "vim_cancel_queued_commands", type=int, default=1, desc="Press escape before issuing commands, to cancel previously queued command that might have been in error",)
mod.setting( "vim_cancel_queued_commands_timeout", type=float, default=0.05, desc="How long to wait in seconds before issuing the real command after canceling",)
mod.setting( "vim_mode_change_timeout", type=float, default=0.3, desc="It how long to wait before issuing commands after a mode change",)
mod.setting( "vim_mode_switch_moves_cursor", type=int, default=0, desc="Preserving insert mode will automatically move the cursor. Setting this to 0 can override that.",)
mod.setting( "vim_use_rpc", type=int, default=0, desc="Whether or not to use RPC if it is available. Useful for testing or avoiding bugs",)
mod.setting( "vim_debug", type=int, default=0, desc="Debugging used for development",)


# These are actions you can call from vim.talon via `user.method_name()` in
# order to modify modes, run commands in specific modes, etc
@mod.action_class
class FidgetActions:
    def vim_set_normal_mode():
        """set normal mode"""
        v = VimMode()
        v.set_normal_mode(auto=False)

    def vim_set_normal_mode_exterm():
        """set normal mode and don't preserve the previous mode"""
        v = VimMode()
        v.set_normal_mode_exterm()

    def vim_set_normal_mode_np():
        """set normal mode and don't preserve the previous mode"""
        v = VimMode()
        v.set_normal_mode_np(auto=False)

    def vim_set_visual_mode():
        """set visual mode"""
        v = VimMode()
        v.set_visual_mode()

    def vim_set_visual_line_mode():
        """set visual line mode"""
        v = VimMode()
        v.set_visual_line_mode()

    def vim_set_visual_block_mode():
        """set visual block mode"""
        v = VimMode()
        v.set_visual_block_mode()

    def vim_set_insert_mode():
        """set insert mode"""
        v = VimMode()
        v.set_insert_mode()

    def vim_set_terminal_mode():
        """set terminal mode"""
        v = VimMode()
        v.set_terminal_mode()

    def vim_set_command_mode():
        """set visual mode"""
        v = VimMode()
        v.set_command_mode()

    def vim_set_command_mode_exterm():
        """set visual mode"""
        v = VimMode()
        v.set_command_mode_exterm()

    def vim_insert_mode(cmd: str):
        """run a given list of commands in normal mode, preserve mode"""
        v = VimMode()
        v.set_insert_mode()
        actions.insert(cmd)

    def vim_insert_mode_key(cmd: str):
        """run a given list of commands in normal mode, preserve mode"""
        v = VimMode()
        v.set_insert_mode()
        actions.key(cmd)

    def vim_insert_mode_np(cmd: str):
        """run a given list of commands in normal mode, don't preserve"""
        v = VimMode()
        v.set_insert_mode_np()
        actions.insert(cmd)

    def vim_normal_mode(cmd: str):
        """run a given list of commands in normal mode, preserve INSERT"""
        v = VimMode()
        v.set_normal_mode()
        actions.insert(cmd)

    def vim_normal_mode_np(cmd: str):
        """run a given list of commands in normal mode, don't preserve
        INSERT"""
        v = VimMode()
        v.set_normal_mode_np()
        actions.insert(cmd)

    def vim_normal_mode_exterm(cmd: str):
        """run a given list of commands in normal mode, don't preserve INSERT,
        escape from terminal mode"""
        v = VimMode()
        v.set_normal_mode_exterm()
        actions.insert(cmd)

    def vim_normal_mode_exterm_preserve(cmd: str):
        """run a given list of commands in normal mode, escape from terminal
        mode, but return to terminal mode after. Special case for settings"""
        v = VimMode()
        v.set_normal_mode_exterm()
        actions.insert(cmd)

    def vim_normal_mode_key_np(cmd: str):
        """press a given key in normal mode"""
        v = VimMode()
        v.set_normal_mode_np()
        actions.key(cmd)

    def vim_normal_mode_key(cmd: str):
        """press a given key in normal mode"""
        v = VimMode()
        v.set_normal_mode()
        actions.key(cmd)

    def vim_normal_mode_exterm_key(cmd: str):
        """press a given key in normal mode, and escape terminal"""
        v = VimMode()
        v.set_normal_mode_exterm()
        actions.key(cmd)

    def vim_normal_mode_keys(keys: str):
        """press a given list of keys in normal mode"""
        v = VimMode()
        v.set_normal_mode()
        for key in keys.split(" "):
            # print(key)
            actions.key(key)

    def vim_normal_mode_exterm_keys(keys: str, term_return: str = "False"):
        """press a given list of keys in normal mode"""
        v = VimMode()
        v.set_normal_mode_exterm()
        for key in keys.split(" "):
            # print(key)
            actions.key(key)
        if term_return == "True":
            v.set_insert_mode()

    def vim_visual_mode(cmd: str):
        """run a given list of commands in visual mode"""
        v = VimMode()
        v.set_visual_mode()
        actions.insert(cmd)

    # technically right now they run in in normal mode, but these calls will
    # ensure that any queued commands are removed
    def vim_command_mode(cmd: str):
        """run a given list of commands in command mode, preserve INSERT"""
        v = VimMode()
        v.set_command_mode()
        if cmd[0] == ':':
            actions.user.paste(cmd[1:])
        else:
            actions.user.paste(cmd)
        # pasting a newline doesn't apply it
        if cmd[-1] == '\n':
            actions.key("enter")

    # technically right now they run in in normal mode, but these calls will
    # ensure that any queued commands are removed
    def vim_command_mode_exterm(cmd: str):
        """run a given list of commands in command mode, preserve INSERT"""
        v = VimMode()
        v.set_command_mode_exterm()
        if cmd[0] == ':':
            actions.user.paste(cmd[1:])
        else:
            actions.user.paste(cmd)
        # pasting a newline doesn't apply it
        if cmd[-1] == '\n':
            actions.key("enter")

    # Sometimes the .talon file won't know what mode to run something in, just
    # that it needs to be a mode that supports motions like normal and visual.
    def vim_any_motion_mode(cmd: str):
        """run a given list of commands in normal mode"""
        v = VimMode()
        v.set_any_motion_mode()
        actions.insert(cmd)


    # Sometimes the .talon file won't know what mode to run something in, just
    # that it needs to be a mode that supports motions like normal and visual.
    def vim_any_motion_mode_exterm(cmd: str):
        """run a given list of commands in some motion mode"""
        v = VimMode()
        v.set_any_motion_mode_exterm()
        actions.insert(cmd)

    def vim_any_motion_mode_key(cmd: str):
        """run a given list of commands in normal mode"""
        v = VimMode()
        v.set_any_motion_mode()
        actions.key(cmd)

    def vim_any_motion_mode_exterm_key(cmd: str):
        """run a given list of commands in normal mode"""
        v = VimMode()
        v.set_any_motion_mode_exterm()
        actions.key(cmd)


class NeoVimRPC:
    """For setting/pulling the modes using RPC"""

    def __init__(self):
        self.init_ok = False
        self.nvim = None

        if settings.get("user.vim_use_rpc") == 0:
            return

        self.rpc_path = self.get_active_rpc()
        if self.rpc_path is not None:
            try:
                self.nvim = pynvim.attach("socket", path=self.rpc_path)
            except RuntimeError:
                return
            self.init_ok = True
        else:
            return

    def get_active_rpc(self):
        title = ui.active_window().title
        if "RPC" in title:
            named_pipe = title.split("RPC:")[1].split(" ")[0]
            return named_pipe
        return None

    def get_active_mode(self):
        mode = self.nvim.request("nvim_get_mode")
        return mode


class VimNonRpc:
    """For pulling the modes out of the title string, if RPC isn't
    available. Is generally slower.."""

    pass


class VimMode:
    # TODO: make this an Enum
    # mode ids represent generic statusline mode() values. see :help mode()
    NORMAL = 1
    VISUAL = 2
    VISUAL_LINE = 3
    VISUAL_BLOCK = 4
    INSERT = 5
    TERMINAL = 6
    COMMAND = 7
    REPLACE = 8
    VREPLACE = 9

    # XXX - not really necessary here, but just used to sanity check for now
    vim_modes = {
        "n": "Normal",
        "no": "N Operator Pending",
        "v": "Visual",
        "V": "V Line",
        "^V": "V-Block",
        "s": "Select",
        "S": "S·Line",
        "i": "Insert",
        "R": "Replace",
        "Rv": "V·Replace",
        "c": "Command",
        "cv": "Vim Ex",
        "ce": "Ex",
        "r": "Prompt",
        "rm": "More",
        "r?": "Confirm",
        "!": "Shell",
        "t": "Terminal",
    }

    def __init__(self):
        # list of all vim instances talon is aware of
        self.vim_instances = []
        self.current_rpc = None
        self.nvrpc = NeoVimRPC()
        self.current_mode = self.get_active_mode()
        self.canceled_timeout = settings.get("user.vim_cancel_queued_commands_timeout")
        self.wait_mode_timeout = settings.get("user.vim_mode_change_timeout")

    def dprint(self, s):
        if settings.get("user.vim_debug"):
            print(s)

    def is_normal_mode(self):
        return self.current_mode == "n"

    def is_visual_mode(self):
        return self.current_mode in ["v", "V", "^V"]

    def is_insert_mode(self):
        return self.current_mode == "i"

    def is_terminal_mode(self):
        return self.current_mode == "t"

    def is_command_mode(self):
        return self.current_mode == "c"

    def is_replace_mode(self):
        return self.current_mode in ["R", "Rv"]

    def get_active_mode(self):
        if self.nvrpc.init_ok is True:
            mode = self.nvrpc.get_active_mode()["mode"]
            self.dprint(mode)
            # XXX -
            self.current_mode = mode
        else:
            title = ui.active_window().title
            mode = None
            if "MODE:" in title:
                mode = title.split("MODE:")[1].split(" ")[0]
                self.dprint(mode)
                if mode not in self.vim_modes.keys():
                    return None
                self.current_mode = mode

        return mode

    def current_mode_id(self):
        if self.is_normal_mode():
            return self.NORMAL
        elif self.is_visual_mode():
            return self.VISUAL
        elif self.is_insert_mode():
            return self.INSERT
        elif self.is_terminal_mode():
            return self.TERMINAL
        elif self.is_command_mode():
            return self.COMMAND

    def set_normal_mode(self, auto=True):
        self.adjust_mode(self.NORMAL, auto=auto)

    def set_normal_mode_exterm(self):
        self.adjust_mode(self.NORMAL, escape_terminal=True)

    # XXX - revisit auto, maybe have separate method version or something
    def set_normal_mode_np(self, auto=True):
        self.adjust_mode(self.NORMAL, no_preserve=True, auto=auto)

    def set_visual_mode(self):
        self.adjust_mode(self.VISUAL)

    def set_visual_mode_np(self):
        self.adjust_mode(self.VISUAL, no_preserve=True)

    def set_visual_line_mode(self):
        self.adjust_mode(self.VISUAL_LINE)

    def set_visual_block_mode(self):
        self.adjust_mode(self.VISUAL_BLOCK)

    def set_insert_mode(self):
        self.adjust_mode(self.INSERT)

    def set_terminal_mode(self):
        self.adjust_mode(self.TERMINAL)

    def set_command_mode(self):
        self.adjust_mode(self.COMMAND)

    def set_command_mode_exterm(self):
        self.adjust_mode(self.COMMAND, escape_terminal=True)

    def set_replace_mode(self):
        self.adjust_mode(self.REPLACE)

    def set_visual_replace_mode(self):
        self.adjust_mode(self.VREPLACE)

    def set_any_motion_mode(self):
        self.adjust_mode([self.NORMAL, self.VISUAL])

    def set_any_motion_mode_exterm(self):
        self.adjust_mode([self.NORMAL, self.VISUAL], escape_terminal=True)

    def set_any_motion_mode_np(self):
        self.adjust_mode(self.NORMAL, no_preserve=True)

    def adjust_mode(
        self, valid_mode_ids, no_preserve=False, escape_terminal=False, auto=True
    ):
        if auto is True and settings.get("user.vim_adjust_modes") == 0:
            return

        self.get_active_mode()
        cur = self.current_mode_id()
        if type(valid_mode_ids) != list:
            valid_mode_ids = [valid_mode_ids]
        self.dprint(f"from {cur} to {valid_mode_ids}")
        if cur not in valid_mode_ids:
            # Just favor the first mode match
            self.set_mode(
                valid_mode_ids[0],
                no_preserve=no_preserve,
                escape_terminal=escape_terminal,
            )
            # Trigger / untrigger mode-related talon grammars
            self.set_mode_tag(valid_mode_ids[0])


    # Often I will say `delete line` and it will trigger `@delete` and `@nine`.
    # This then keys 9. I then say `undo` to fix the bad delete, which does 9
    # undos. Chaos ensues... this seeks to fix that
    def cancel_queued_commands(self):
        if (
            settings.get("user.vim_cancel_queued_commands") == 1
            and self.is_normal_mode()
        ):
            # print("escaping queued cmd")
            actions.key("escape")
            timeout = settings.get("user.vim_cancel_queued_commands_timeout")
            time.sleep(timeout)

    def wait_mode_change(self, wanted):
        # XXX - try to force a redraw?
        if self.nvrpc.init_ok:
            while wanted != self.nvrpc.get_active_mode()["mode"][0]:
                #print("%s vs %s" % (wanted, self.nvrpc.get_active_mode()["mode"]))
                time.sleep(0.005)
        else:
            time.sleep(self.wait_mode_timeout)

    @classmethod
    # We don't want unnecessarily only call this from set_mode() is the user
    # might change the mode of vim manually or speaking keys, but we still want
    # the context specific grammars to match.
    # TODO: present to figure out if this makes sense present addition to
    # win.title matching I already do. I think it does make sense for cases of
    # overriding certain default actions like home/end
    def set_mode_tag(self, mode):
        global mode_tag_list
        global ctx

        print(ctx.tags)

    # NOTE: querying certain modes is broken (^V mode undetected)
    # Setting mode with RPC is impossible, which makes sense because it would
    # break things like macro recording/replaying. So we use keyboard
    # combinations
    def set_mode(self, wanted_mode, no_preserve=False, escape_terminal=False):
        current_mode = self.get_active_mode()

        if current_mode == wanted_mode or (
            self.is_terminal_mode() and wanted_mode == self.INSERT
        ):
            return

        self.dprint("Setting mode to {}".format(wanted_mode))
        # enter normal mode where necessary
        if self.is_terminal_mode():
            if (
                settings.get("user.vim_escape_terminal_mode") is True
                or escape_terminal is True
            ):
                # print("escaping")
                # break out of terminal mode
                actions.key("ctrl-\\")
                actions.key("ctrl-n")
                self.wait_mode_change("n")
            else:
                # Imagine you have a vim terminal and inside you're running a
                # terminal that is using vim mode rather than emacs mode. This
                # means you will want to be able to use some amount of vim
                # commands to edit the shells command line itself without
                # actually being inside the encapsulating vim instance.
                # The use of escape here tries to compensate for those
                # scenerios, where you won't break into the encapsulating vim
                # instance. Needs to be tested. If you don't like this, you can
                # set vim_escape_terminal_mode to 1
                actions.key("escape")
                # NOTE: do not wait on mode change here, as we
                # cannot detect it for the inner thing
        elif self.is_insert_mode():
            if (
                wanted_mode == self.NORMAL
                and no_preserve is False
                and settings.get("user.vim_preserve_insert_mode") >= 1
            ):
                if settings.get("user.vim_mode_switch_moves_cursor") == 0:
                    actions.key("ctrl-\\")  # don't move the cursor on mode switch
                actions.key("ctrl-o")
            else:
                # Presses right because entering normal mode via escape puts
                # the cursor back one position, otherwise misaligns on words.
                # Exception is `2 delete big-back` from INSERT mode.
                actions.key("right")
                actions.key("escape")
            self.wait_mode_change("n")
        elif self.is_visual_mode() or self.is_command_mode() or self.is_replace_mode():
            actions.key("escape")
            self.wait_mode_change("n")
        elif self.is_normal_mode() and wanted_mode == self.COMMAND:
            # We explicitly escape even if normal mode, to cancel any queued
            # commands that might affect our command. For instance, accidental
            # number queueing followed by :w, etc
            actions.key("escape")
            time.sleep(self.canceled_timeout)
            self.wait_mode_change("n")

        # switch to explicit mode if necessary. we will be normal mode here
        if wanted_mode == self.INSERT:
            actions.key("i")
        # or just let the original 'mode' command run from this point
        elif wanted_mode == self.VISUAL:
            # first we cancel queued normal commands that might mess with 'v'
            # ex: normal mode press 5, then press v to switch to visual
            actions.key("escape")
            actions.key("v")
        elif wanted_mode == self.VISUAL_LINE:
            # first we cancel queued normal commands that might mess with 'v'
            # ex: normal mode press 5, then press v to switch to visual
            actions.key("escape")
            actions.key("V")
        elif wanted_mode == self.VISUAL_BLOCK:
            # first we cancel queued normal commands that might mess with 'v'
            # ex: normal mode press 5, then press v to switch to visual
            actions.key("escape")
            actions.key("ctrl-v")
        elif wanted_mode == self.COMMAND:
            actions.key(":")
            self.wait_mode_change("c")
        elif wanted_mode == self.REPLACE:
            actions.key("R")
        elif wanted_mode == self.VREPLACE:
            actions.key("g R")

        # Here we assume we are now in some normalized state:
        # need to make the notify command configurable
        if settings.get("user.vim_notify_mode_changes") >= 1:
            self.notify_mode_change(wanted_mode)
            ...

    def notify_mode_change(self, mode):
        """Function to be customized by talon user to determine how they want
        notifications on mode changes"""
        pass
