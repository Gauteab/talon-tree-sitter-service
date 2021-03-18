title: /VIM/i

-
norm: key(ctrl-c)
complete: key(ctrl-n)

visual block: user.vim_set_visual_block_mode()

{user.vim_verbs} {user.vim_targets}: user.vim_do_action(vim_verbs, vim_targets)
{user.vim_verbs} <number> {user.vim_targets}: user.vim_do_action(vim_verbs, vim_targets, number)

duplicate line: user.vim_normal_mode("yyp")
replace <user.text>:
    user.vim_normal_mode("ciw")
    insert(text)

jump down: user.vim_normal_mode_key_np("ctrl-d")
jump up: user.vim_normal_mode_key_np("ctrl-u")
scroll bottom: key(ctrl-c G)
scroll top: key(ctrl-c g g)
forward: key(ctrl-c f)
backward: key(ctrl-c F)
scan <user.word>:
    key(ctrl-c /)
    insert(user.formatted_text(word, "snake"))
    key(enter)
scan: key(ctrl-c /)
scan back <user.word>:
    key(ctrl-c ?)
    insert(user.formatted_text(word, "snake"))
    key(enter)
scan back: key(ctrl-c ?)

find file: " g"
find symbol: " rg"
buffers: 
    key(ctrl-c)
    ":Buffers\n"

pain (vertical|vert): key(ctrl-c : v s enter)
pain (horizontal|whore): key(ctrl-c : s p enter)
pain right: key(ctrl-c ctrl-w l)
pain left: key(ctrl-c ctrl-w h)
pain up: key(ctrl-c ctrl-w k)
pain down: key(ctrl-c ctrl-w j)

set wrap: ":set wrap\n"
[set] no wrap: ":set nowrap\n"
no highlight: ":noh\n"

run it: key(f9)
save and quit: key(ctrl-c : w q enter)
save it: key(ctrl-c : w enter)
quit it: key(ctrl-c : q enter)
quit everything: key(ctrl-c : q a enter)
do edit: key(ctrl-c : e space)
do set: key(ctrl-c : s e t space)
do echo: key(ctrl-c : e c h o space)

next air: key(ctrl-n)
last air: key(ctrl-b)

spell on: ":set spell spelllang=en_us\n"
spell off: ":set nospell\n"
spell next: key(] s)
spell last: key([ s)
spell fix: key(z =)

mark [down] preview: ":MarkdownPreview\n"
nerd tree: user.vim_normal_mode(":NERDTree\n")

substitute: ":s/"
substitute all: ":%s/"

cock fix: user.vim_normal_mode(":CocFix\n")
# do hover: user.vim_normal_mode(":call CocAction('doHover')\n")
do hover: user.vim_normal_mode(" ch")
add extension: 
    user.vim_normal_mode("ggO")
    "lang\t"

source config: user.vim_normal_mode(":source $MYVIMRC\n")

