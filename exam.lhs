\documentclass[acmlarge,12pt]{acmart}
\geometry{a4paper,margin=25mm,includehead=false,includefoot=false}

% newtxmath obviates and conflicts with amssymb and stmaryrd,
% so pretend those packages are already loaded
\expandafter\def\csname ver@@amssymb.sty\endcsname{}
\expandafter\def\csname ver@@stmaryrd.sty\endcsname{}
%include preamble.lhs
%format ... = "\dots "
\begin{comment}
\begin{code}
main = return ()
\end{code}
\end{comment}
\renewcommand{\plus}{\mathbin{+\!\!+}}
\renewcommand{\bind}{\mathbin{>\!\!>\mkern-6.7mu=}}
\renewcommand{\rbind}{\mathbin{=\mkern-6.7mu<\!\!<}}% suggested by Neil Mitchell
\renewcommand{\sequ}{\mathbin{>\!\!>}}

\begin{document}
\thispagestyle{empty}
\pagestyle{empty}
\textbf{Monad and side effects}
\begin{enumerate}
\item
Recall that the |Monad| type class is built-in already in Haskell:
\begin{spec}
class Monad m where
  return  :: a -> m a
  (>>=)   :: m a -> (a -> m b) -> m b
\end{spec}
(Actually, the |Monad| type class in Haskell has the superclasses |Functor| and
|Applicative| today, but we ignore them for simplicity here.)

The list monad is also built-in.  However, please define it again below.
In other words, define the functions |return :: a -> [a]| and |(>>=) :: [a] -> (a -> [b]) -> [b]| below.
\begin{spec}
instance Monad [] where
  ??? = ???
  ...
\end{spec}
\vfill

Hint: The definition should only be a few lines long.
You can use the following built-in functions for lists, but you don't have to:
\begin{spec}
concatMap  :: (a -> [b]) -> [a] -> [b]
concat     :: [[a]] -> [a]
map        :: (a -> b) -> [a] -> [b]
(++)       :: [a] -> [a] -> [a]
\end{spec}

\item
Define a function named |cases| that takes a string and returns all the ways to
change each character in the given string to either uppercase or lowercase.
\begin{spec}
cases :: String -> [String]
\end{spec}
For example, the call |cases "Cat"| should compute this list of strings:
\[|cases "Cat" = ["CAT","CAt","CaT","Cat","cAT","cAt","caT","cat"]|\]
Your definition \textbf{must} use the built-in function |traverse|, like this:
\begin{spec}
cases = traverse ???
\end{spec}
\vfill

Hint: The definition should only be a few lines long. Recall that |traverse| has this type:
\begin{spec}
traverse :: (Monad m) => (a -> m b) -> [a] -> m [b]
\end{spec}
The type |String| is same as |[Char]|:
\begin{spec}
type String = [Char]
\end{spec}
Use these built-in functions:
\begin{spec}
toUpper, toLower :: Char -> Char
\end{spec}

\item
Define a function
\begin{spec}
editLine :: String -> IO String
\end{spec}
that prints out the given string and then reads a line entered by the user.
For example, the call |editLine "cat"| should print ``\texttt{cat}'' to the screen
and then wait for the user to enter a line. If the user enters ``\texttt{dog}'',
then the result of the call should be the new string |"dog"|.

\newpage
Hint: Use these built-in functions:
\begin{spec}
putStrLn  :: String -> IO ()
getLine   :: IO String
\end{spec}

\item
Recall internally labeled binary trees:
\begin{spec}
data ITree a = Null | Node a (ITree a) (ITree a)
\end{spec}
In a tree of type |ITree a|, each label has type~|a|.
Define a function
\begin{spec}
traverseITree :: (Monad m) => (a -> m b) -> ITree a -> m (ITree b)
\end{spec}
that applies the given function to each label in the given tree,
to produce a new tree, with side effects.
\vfill

\item
Using |editLine| and |traverseITree| above, define a function
\begin{spec}
editITree :: ITree String -> IO (ITree String)
\end{spec}
that prints out each string in the given tree and replaces it with a line
entered by the user before proceeding to the next string.
For example, the call
\[|Node "cat" Null (Node "dog" Null Null)|\]
should print ``\texttt{cat}'', then read a line from the user, then
print ``\texttt{dog}'', then read another line from the user.
If the user enters ``\texttt{rat}'' and ``\texttt{fox}'', then
the result of the call should be the new tree
\[|Node "rat" Null (Node "fox" Null Null)|\]
\vspace{0pt plus.5fill}

\item
Using |traverseITree| above, define a function
\begin{spec}
relabelITree :: ITree a -> ITree Int
\end{spec}
that changes the labels in the given tree to successive integers, starting with~1.
For example, the call
\[|Node "cat" Null (Node "dog" Null Null)|\]
should produce the result
\[|Node 1 Null (Node 2 Null Null)|\]
\vfill
Hint: You can use the following built-in state monad, but you don't have to:
\begin{spec}
instance Monad (State s) where ...
runState  :: State s a -> (s -> (a, s))
state     :: (s -> (a, s)) -> State s a
\end{spec}
\end{enumerate}
\end{document}
