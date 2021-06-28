title: /VIM/i

-
norm: key(ctrl-c)
complete: key(ctrl-n)

visual block: user.vim_normal_mode_key("ctrl-v")

change that: user.vim_normal_mode("ciw")
delete that: user.vim_normal_mode("daw")
{user.vim_verbs} {user.vim_targets}: user.vim_do_action(vim_verbs, vim_targets)
{user.vim_verbs} <number> {user.vim_targets}: user.vim_do_action(vim_verbs, vim_targets, number)

duplicate line: user.vim_normal_mode("yyp")
replace <user.text>: user.vim_normal_mode("ciw{text}")
insert start: user.vim_normal_mode("I")
insert end: user.vim_normal_mode("A")
insert after: user.vim_normal_mode("ea")
[insert] after {user.vim_targets}: user.vim_normal_mode("{vim_targets}a")
insert before: user.vim_normal_mode("bi")
[insert] before {user.vim_targets}: user.vim_normal_mode("{vim_targets}i")

leap: user.vim_normal_mode("\n")

jump down: user.vim_normal_mode_key("ctrl-d")
jump up: user.vim_normal_mode_key("ctrl-u")
regret: user.vim_normal_mode("u")
re do: user.vim_normal_mode_key("ctrl-r")
scroll bottom: key(ctrl-c G)
scroll top: key(ctrl-c g g)
forward: key(ctrl-c f)
backward: key(ctrl-c F)
scan: user.vim_normal_mode("/")
scan <user.word>: user.vim_normal_mode("/{word}\n")
scan back: user.vim_normal_mode("?")
scan back <user.word>: user.vim_normal_mode("?{word}\n")
push: user.vim_normal_mode("o")
push up: user.vim_normal_mode("O")

find file: user.vim_normal_mode(":Files\n")
find file <user.word>: user.vim_normal_mode(":Files\n{word}")
find symbol: user.vim_normal_mode(":Rg\n")
buffers: user.vim_normal_mode(":Buffers\n")
buff last: user.vim_normal_mode(" b")

pain (vertical|vert): user.vim_normal_mode(":vs\n")
pain (horizontal|whore): user.vim_normal_mode(":sp\n")
pain right: user.vim_normal_mode_key("ctrl-w l")
pain left: user.vim_normal_mode_key("ctrl-w h")
pain up: user.vim_normal_mode_key("ctrl-w k")
pain down: user.vim_normal_mode_key("ctrl-w j")

set wrap: user.vim_normal_mode(":set wrap\n")
[set] no wrap: user.vim_normal_mode(":set nowrap\n")
no highlight: user.vim_normal_mode(":noh\n")

run it: user.vim_normal_mode_key("f9")
save and quit: user.vim_normal_mode(":wq\n")
save it: user.vim_normal_mode(":w\n")
quit it: user.vim_normal_mode(":q\n")
quit everything: user.vim_normal_mode(":qa\n")
do edit: user.vim_normal_mode(":e ")
do set: user.vim_normal_mode(":set ")
do echo: user.vim_normal_mode(":echo ")

next air: user.vim_normal_mode_key("ctrl-n")
last air: user.vim_normal_mode_key("ctrl-b")

spell on: user.vim_normal_mode(":set spell spelllang=en_us\n")
spell off: user.vim_normal_mode(":set nospell\n")
spell next: user.vim_normal_mode_key("] s")
spell last: user.vim_normal_mode_key("[ s")
spell fix: user.vim_normal_mode_key("z =")

mark [down] preview: user.vim_normal_mode(":MarkdownPreview\n")
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

