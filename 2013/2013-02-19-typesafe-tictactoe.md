# A Game of Type-Safe Tic-Tac-Toe

I held a functional programming workshop recently where the topic was
implementing a browser based Tic-Tac-Toe on top of a Haskell web server. This is
a rundown of the resulting game, where I demonstrate some of the ways to
leverage Haskell's type system to gain stronger compile time guarantees.

While many of these techniques are slightly overkill for such as simple game,
they will hopefully give you some ideas that you can take advantage of in
more complicated Haskell projects.


## A Kind of Piece

The fundamental elements of Tic-Tac-Toe are the game pieces, which can be either
Xs or Os. This maps naturally to the simple, algebraic data-type

    data Piece = X | O deriving Eq


However, we can encode several interesting properties on the type-level by
promoting the `Piece` type to a type-kind and thus, `X` and `O` into types. This
is done by the language extension [`DataKinds`](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/kind-polymorphism-and-promotion.html#promotion) which was
introduced in GHC 7.4.1.

We can use the types `X` and `O` as [phantom types](http://www.haskell.org/haskellwiki/Phantom_type)
to tag other types. For example, instead of just having a `Player` type, we can
have `Player X` and `Player O` and when those players make moves on the game
board, the moves can be tagged as `Move X` and `Move O`. This adds additional
compile-time guarantees to our program, because we can formulate the APIs so
that it's impossible for the `Player X` to make a `Move O` due to a programming
error if the program type-checks.

Sometimes we need to reify the `Piece` type into `Piece` value, so define the
following type-class

    class ReifyPiece (p :: Piece) where
        reifyPiece :: f p -> Piece

    instance ReifyPiece X where
        reifyPiece _ = X

    instance ReifyPiece O where
        reifyPiece _ = O

The `p :: Piece` in the type-class declaration is a [kind signature](http://www.haskell.org/ghc/docs/latest/html/users_guide/other-type-extensions.html#kinding) for the
type variable `p`. This restricts the type of `p` to those types that have the
kind `Piece`, i.e. the types `X` and `O`.

Since data-types that are promoted from constructors cannot have any values,
`X` and `O` are, in practice, limited to appearing in phantom types.
Therefore the `reifyPiece` function takes in a value of some parametric type
`f` that has `p` as a type-parameter. Now we can, for example, call `reifyPiece`
with any value of type `Move X` and get back the `Piece` value `X`.


## The Order of Play

Using the `Piece` kind, we can even encode in the types the fact that `Player X`
and `Player O` must take alternating turns when playing the game. For this, we
need a [type family](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/type-families.html)
for switching from `X` to `O` and back.

    type family Other (p :: Piece) :: Piece
    type instance Other X = O
    type instance Other O = X


The type family `Other` is the type-level equivalent of the function

    other :: Piece -> Piece
    other X = O
    other O = X

Even though writing type-level functions is a bit more limited and verbose the
relationship to value-level functions should be quite evident.


Now, to encode the current state of the game, we use the `Game` data type

    data Game (turn :: Piece) = Game Board (GameStatus turn)

The `turn` type-variable indicates whether it's currently X's or O's turn to
play. `GameStatus` is a [GADT](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/data-type-extensions.html#gadt) (generalized algebraic data type) which indicates whether the game
still expects a new turn or if it has ended in a victory or draw.

    type ProcessMove turn = Move turn -> Maybe (Game (Other turn))

    data GameStatus (turn :: Piece) where
        Turn  :: ProcessMove turn -> GameStatus turn
        Draw  :: GameStatus turn
        Win   :: Piece -> GameStatus turn

The `ProcessMove` type alias describes a function that takes a move from the
player whose turn it currently is and returns a new `Game` value for the
opponent's turn (or `Nothing` if the move was invalid). Since the return value
has the phantom type `Other turn` this causes type parameter to switch between
`X` and `O`.


`Board` and `Position` are opaque types that we interface with using the
following functions.

    newBoard :: Board
    putPiece :: Piece -> Position -> Board -> Board
    isFull   :: Board -> Bool
    (!)      :: Board -> Position -> Maybe Piece


In addition, we have the value `lanes` which lists all the possible lanes that
can be filled up to win teh game (i.e. rows, columns and diagonals) and the
accessor `movePos` which returns the position where a move was played.

    lanes    :: [[Position]]
    movePos  :: Move turn -> Position


The only function we expose from the game logic module is

    newGame :: Game X
    newGame = Game newBoard $ Turn $ makeMove newBoard

The type of `newGame` encodes the property that `X` always goes first in a
newly created game and the `makeMove` function alternates between accepting
`Move X` and `Move O` values to produce the next turn of the game.

    makeMove :: CyclicTurn turn => Board -> ProcessMove turn
    makeMove board move
        | Nothing <- board ! pos = Game board' <$> (gameOver <|> nextTurn)
        | otherwise              = Nothing
        where
            piece  = reifyPiece move
            pos    = movePos move
            board' = putPiece piece pos board

            gameOver = victory <|> draw
            nextTurn = Just $ Turn $ makeMove board'

            draw     = maybeIf (isFull board') Draw
            victory  = msum $ map check lanes

            check lane = maybeIf allMatch $ Win piece where
                allMatch = all (\p -> board' ! p == Just piece) lane

The `CyclicTurn` constraint is to help GHC realize that any `Move turn` can
be reified to a `Piece` since `Move X` is an instance of `ReifyPiece` as is
`Move O` and for all types `t` of the `Piece` kind `Other (Other t)` is equal to
`t`.

    type CyclicTurn turn =
        ( ReifyPiece turn
        , ReifyPiece (Other turn)
        , Other (Other turn) ~ turn
        )

Let's go through the `makeMove` function part by part.

    makeMove board move
        | Nothing <- board ! pos = Game board' <$> (gameOver <|> nextTurn)
        | otherwise              = Nothing

The guard verifies that the move is valid (i.e. the position on the board is
empty) with a [pattern guard](http://research.microsoft.com/en-us/um/people/simonpj/Haskell/guards.html).
We do not have to check that the given position is within game board bounds
because `Position` is an opaque data type for which constructing invalid values
is impossible.

If the move is valid, we return a `Game` value with the updated board (`board'`)
and a new `GameStatus` which is either `gameOver` or `nextTurn`.

            gameOver = victory <|> draw
            nextTurn = Just $ Turn $ makeMove board'

`gameOver` happens with either `victory` or `draw` which both have the type
`Maybe (GameStatus turn)`. `nextTurn` is always a `Just`, so the first guard in
the function can never return a `Nothing`. `nextTurn` calls `makeMove`
recursively and because the return type of `makeMove` is `Maybe (Game (Other turn))`
type-inference will handle switching the `X` to `O` and `O` to `X` in the
recursive call.

            draw     = maybeIf (isFull board') Draw
            victory  = msum $ map check lanes

            check lane = maybeIf allMatch $ Win piece where
                allMatch = all (\p -> board' ! p == Just piece) lane

Checking for draw or victory is straightforward with the `Board` API we are
given. `maybeIf` is a small utility function for returning a `Maybe` value
based on a boolean predicate.

    maybeIf :: Bool -> a -> Maybe a
    maybeIf p a = if p then Just a else Nothing


## Interfacing with the Unsafe World of JavaScript

The actual game implementation runs as a web server to which browsers connect
using [WebSockets](http://en.wikipedia.org/wiki/WebSocket). There's a thin
abstraction layer on top of raw WebSockets which introduces two concepts:
notifications and requests. Notifications are fire-and-forget sort of messages
while requests require an answer from the other end of the connection.

The messaging layer uses JSON for serialization and, naturally, when the
Haskell program receives a new message and deserializes it, there is no static
type information available. If we look at requests which are initiated by the
server, one way to determine the response type is to let the caller decide
it via return value polymorphism.

    request :: (ToJSON request, FromJSON response) => request -> IO response

The `response` type parameter will usually get inferred from the calling context,
but this can sometimes result in hard to find bugs if, due to faulty implementation,
the reponse type ends up being inferred as something other than we expected.
Another weakness in this approach is that when using polymorphic functions, the
reponse type might be left ambiguous, forcing us to add inline type annotations
at the function call site.

Because of the above reasons, the messaging layer requires requests to have
the kind (* -> *) so that every request type contains the type of the expected
response. I.e.

    request :: (Request request, FromJSON response) => request response -> IO response

Now we can use a GADT to define the different requests in our communication
protocol. For example:

    data ServerRequest response where
        AskName    :: ServerRequest String
        AskMove    :: ServerRequest (Move t)
        AskNewGame :: ServerRequest Bool


The `Request` type-class is defined as

    class Request r where
        reqToJSON   :: r a -> Value
        reqFromJSON :: Value -> Result (Some r)

Since we cannot know the correct phantom type for a request that we deserialize
from some arbitrary JSON string, we need to erase the phantom type (which indicates
the expected type for the response) using the helper GADT `Some`.

    data Some t where
        Some :: (FromJSON x, ToJSON x) => t x -> Some t

Now erasing the phantom type might seem like it undermines the whole extra
type-safety, but once again, it's the properties of GADTs that come to our
rescue. Handlers for incoming requests are registered with the function

    onRequest
        :: Request req
        => Connection
        -> (forall resp. => req resp -> IO resp)
        -> STM ()

By using a [rank-2 type](http://www.haskell.org/ghc/docs/7.4.1/html/users_guide/other-type-extensions.html#universal-quantification), we can provide a request handler function that
can return various response types based on the GADT constructor. For example,
handling the `ServerRequest` defined above might look something like

    onRequest conn $ \req -> case req of
        AskName    -> return "John Doe"
        AskNewGame -> return True

As you can see, the different branches of the case expression can return
different types based on the GADT constructor that was matched. And best of all,
if we tried to return the wrong type, for example

    AskName -> return False

we would get a compile time type error.


## Taking Sides

As I mentioned earlier, the `Piece` kind is used to tag the types `Move` and
`Player` as well. However, the way the game works is that when players connect
to the server they are put to a queue and at this point they are not yet playing
with either X's or O's. The `Player` type is actually defined like this

    data User (piece :: Maybe Piece) = User
        { userName :: String
        , userConn :: Connection
        }

    type NewPlayer    = User Nothing
    type Player piece = User (Just piece)

Since the `DataKinds` extension promotes _all_ types into kinds, this includes
standard library types such as `Maybe`. That means that we can use the `Maybe`
kind to encode optional phantom types.


The user module exports the following functions:

    newUser     :: String -> Connection -> NewPlayer
    assignSides :: NewPlayer -> NewPlayer -> Random (Player X, Player O)
    stripSide   :: Player t -> NewPlayer


For receiving moves from the players, we use the function

    requestMove :: Player piece -> IO (Future (Move piece))

which ensures that the move has the same `Piece` tag that the issuing player
has.


## Playing the Game

When there are at least two players queued up on the server, the server pairs
them up, assigns random sides with the `assignSides` function and starts a
thread which runs the actual game:

    type Cyclic a b = (a ~ Other b, b ~ Other a)

    playGame :: TChan NewPlayer -> (Player X, Player O) -> IO ()
    playGame queue (px, po) = start >> play >> both requeue where

        start = atomically $ do
            notify (userConn px) $ FoundOpponent $ userName po
            notify (userConn po) $ FoundOpponent $ userName px

        play = playTurn px po newGame

        playTurn :: Cyclic t t' => Player t -> Player t' -> Game t -> IO ()
        playTurn p p' (Game b st) = sendBoard >> foldGameStatus turn draw win st where
            turn f = loop where
                loop        = requestMove p >>= resolveMove
                resolveMove = join . atomically . foldFuture disconnect nextTurn
                disconnect  = atomically $ notifyResult p' WonGame
                nextTurn m  = maybe loop (playTurn p' p) (f m)

            draw = atomically $ both $ \u -> notifyResult u DrawGame

            win _ = atomically $ do
                notifyResult p  LostGame
                notifyResult p' WonGame

            sendBoard = atomically $ both $ \u -> notify (userConn u) (GameBoard b)

        notifyResult u = notify (userConn u) . GameOver

        both :: Monad m => (forall t. Player t -> m ()) -> m ()
        both op = op px >> op po

        requeue :: Player t -> IO ()
        requeue p = void $ forkIO $ do
            yes <- request (userConn p) AskNewGame
            when yes $ atomically $ writeTChan queue $ stripSide p

While I'm not going to go into too much detail here, the part that is relevant for this
type-safety article is the way that `playTurn` takes in `Player X` and
`Player O` and then swaps them around in the recursive call in `nextTurn`. This
way the first parameter always contains the player whose turn it is, and the tag
`X` or `O` has to match the type of move `Game` is expecting for this turn. This
way, were we to, for example, forget to swap the two players, we would get a
compile time type-error, preventing the same player from getting to play all the
turns.

The full source code for the game can be browsed at [GitHub](https://github.com/leonidas/lambda-webdev/tree/type-safe).


<hr/>
Sami Hangaslammi <[sami.hangaslammi@leonidasoy.fi](mailto://sami.hangaslammi@leonidasoy.fi)>

Leonidas Oy <[http://leonidasoy.fi](http://leonidasoy.fi)>
