
# elm-tree-sitter range bug with comments

if a value declaration ending with a case expression is followed by a comment
that is not the final node in the file, the end position of that declaration
will be that of the comment.

example: 
```elm
update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1

-- VIEW

x=5
```

# let expressions break when there are extra spaces

```elm
test =
    let x = 5  in
    x
```

reporoduce by removing `x=5` and observe
how the end position of *update* changes.

I have only reproduced this with case expressions.

# issues

type signatures and value decorations are separate nodes
and can therefore not be captured in the same query.
I would like to be able to select a function along with its signature, but this seems to require some extra work.

it is convenient that elm does not allow shadowing decorations.
this makes it possible to use queries for navigating between declarations.
otherwise you would have to be able to distinguish between the different
declarations with the same name.

-- which part of the system should be responsible for making edits?

## thoughts

this stuff can make it easier to use a new editor because the structural navigation commands should be the same
