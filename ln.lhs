\documentclass[sigplan,landscape,nonacm]{acmart}
\geometry{a4paper,margin=3pc,includeheadfoot}
\citestyle{acmauthoryear}

\usepackage[T1]{CJKutf8}

% newtxmath obviates and conflicts with amssymb and stmaryrd,
% so pretend those packages are already loaded
\expandafter\def\csname ver@@amssymb.sty\endcsname{}
\expandafter\def\csname ver@@stmaryrd.sty\endcsname{}
%include preamble.lhs
\begin{comment}
\begin{code}
main = return ()
\end{code}
\end{comment}
\renewcommand{\plus}{\mathbin{+\!\!+}}
\renewcommand{\bind}{\mathbin{>\!\!>\mkern-6.7mu=}}
\renewcommand{\rbind}{\mathbin{=\mkern-6.7mu<\!\!<}}% suggested by Neil Mitchell
\renewcommand{\sequ}{\mathbin{>\!\!>}}
%format nabla f  = "\nabla " f
%format `bullet` = "\bullet "
\arrayhs
\raggedbottom

\usepackage{comment}
\excludecomment{solution}
\excludecomment{challenge}
\includecomment{nonchallenge}

\usepackage{tikz}

\usepackage{mdwlist}

\usepackage{parskip}
\raggedright

\csname tagsleft@@true\endcsname

\begin{document}
\fancyhf{}
\fancyfoot[C]{\thepage}
\begin{CJK}{UTF8}{bsmi}
\CJKnospace

\title{Monad and side effects}
\author{單中杰}
\authorsaddresses{}
\maketitle

Theme: To get what you want, say what you mean.\\
So, if you want to do something, say what doing it means.

\section{Warm up}

Sub-theme: For modular reuse, abstract from similarities over differences.

Property-based testing \citep{claessen-quickcheck}

\section*{Side effects}

Basically, a side effect is something that a piece of code does besides turning input arguments into return values.

Use each side effect to write some programs, including an interpreter for a simple language.
We want to reuse the same interpreter code for a variety of side effects.

\section{State (the OG side effect)}
\begin{itemize*}
\item |state -> (value, state)|
\item Local vs global state (example: union-find)
\end{itemize*}

\section{Exception, Maybe}
\begin{itemize*}
\item |error + value|
\item Early exit from loop/recursion
\end{itemize*}

\section{Nondeterminism}
\begin{itemize*}
\item Set/multiset/list/pointed-set/distribution of values
\item Backtracking/tabling search
\end{itemize*}

\section{Monads}

A type constructor |M| with two operations: \citep{wadler-monads}\nopagebreak
\begin{spec}
return  :: forall a.    a -> M a                  -- unit, eta
(>>=)   :: forall a b.  M a -> (a -> M b) -> M b  -- bind, star
\end{spec}
Monad laws:\nopagebreak
\begin{spec}
return a >>= k           = k a
m >>= return             = m
m >>= (\a -> k a >>= l)  = (m >>= k) >>= l
\end{spec}
Alternative definition with three operations:\nopagebreak
\begin{spec}
return  :: forall a.    a -> M a                  -- unit, eta
fmap    :: forall a b.  (a -> b) -> M a -> M b    -- functoriality
join    :: forall a.    M (M a) -> M a            -- mu
\end{spec}

\begin{comment}
\begin{spec}
fmap id                  = id
fmap (f . g)             = fmap f . fmap g
fmap f . return          = return . f          
fmap f . join            = join . fmap (fmap f)
join (return m)          = m
join (fmap return m)     = m
join . join              = join . fmap join
\end{spec}
\end{comment}

\section{Type classes}

Dictionaries of methods (even binary methods) \citep{wadler-ad-hoc}
\begin{itemize*}
\item Passed implicitly
\item Constructed automatically during type inference
\end{itemize*}

\subsection{Eq}

Default method implementations
\begin{spec}
class Eq a where
  (==)  :: a -> a -> Bool
  (/=)  :: a -> a -> Bool

  x /= y  =  not  (x == y)
  x == y  =  not  (x /= y)
\end{spec}
\begin{spec}
instance Eq Int   where -- ...

instance Eq Char  where -- ...
\end{spec}
Instance contexts (constraints)\nopagebreak
\begin{spec}
instance (Eq a, Eq b) => Eq (a,b) where
  (a1,b1) == (a2,b2) = a1 == a2 && b1 == b2

instance (Eq a) => Eq [a] where
  []      ==  []      =  True
  (x:xs)  ==  (y:ys)  =  x == y && xs == ys
  _       ==  _       =  False
\end{spec}

Type-signature contexts (constraints)
\begin{spec}
elem :: (Eq a) => a -> [a] -> Bool
\end{spec}

\subsection{Ord}

Class contexts (superclasses)
\begin{spec}
class (Eq a) => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool
  x < y   =  not  (x == y)  &&       (x <= y)
  x > y   =  not  (x == y)  &&  not  (x <= y)
  x >= y  =       (x == y)  ||  not  (x <= y)
 -- ...
\end{spec}

\subsection{Show}

\leavevmode
\begin{spec}
class Show a where
  show :: a -> String
 -- ...
\end{spec}

\subsection{Monad}

Refined class hierarchy
\begin{spec}
class Functor m where
  fmap    :: (a -> b) -> m a -> m b

class (Functor m) => Applicative m where
  pure    :: a -> m a
  (<*>)   :: m (a -> b) -> m a -> m b

class (Applicative m) => Monad m where
  return  :: a -> m a
  (>>=)   :: m a -> (a -> m b) -> m b
\end{spec}
Easy superclass implementations\nopagebreak
\begin{spec}
newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where fmap = liftM

instance Applicative (State s) where pure = return; (<*>) = ap

instance Monad (State s) where
  return a  = State (\s ->  (a, s))
  m >>= k   = State (\s ->  let (a, s') = runState m s
                            in runState (k a) s')
\end{spec}
|IO| is an abstract |Monad| \citep{peyton-jones-tackling}

\section{Imperative programming}

Input, Output
    \begin{itemize*}
    \item |input -> value|
    \item |(value, output)|
    \item Interactivity: inputs and outputs that depend on each other
    \end{itemize*}
Control
    \begin{itemize*}
    \item GO TO
    \item Continuations
    \item Measures?! Everything?!?! \citep{filinski-representing}
    \end{itemize*}

\subsection{Do notation}

Imperative intuition. For example, monad laws:
\begin{spec}
do { x <- return a; k x }            =  k a
do { x <- m; return x }              =  m
do { a <- m; do { b <- k a; l b } }  =  do { b <- do { a <- m; k a }; l b }
\end{spec}

\subsection{Polymorphism across monads}

Action combinators, useful for all monads
\begin{spec}
traverse     :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM_        :: (Monad m) => (a -> m b) -> [a] -> m ()

sequence     :: (Monad m) => [m a] -> m [a]
sequence_    :: (Monad m) => [m a] -> m ()

import Control.Monad

replicateM   :: (Monad m) => Int -> m a -> m [a]
replicateM_  :: (Monad m) => Int -> m a -> m ()

filterM      :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
foldM        :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
\end{spec}
Generalized to containers other than lists:\\
small but powerful API for container implementation to provide

\newpage
\section{Combining side effects}

\subsection{State and IO}

Make state dependencies explicit and contained,\\
in just another monad with a different dictionary
\begin{spec}
import Control.Monad.Trans.State

StateT s IO a                  = s -> IO (a, s)
\end{spec}

\subsection{State and exception}

Different combinations have different meanings
\begin{spec}
import Control.Monad.Trans.Maybe

StateT s Maybe a               = s -> Maybe (a, s)
MaybeT (State s) a             = s -> (Maybe a, s)
StateT s (MaybeT (State t)) a  = s -> t -> (Maybe (a, s), t)
\end{spec}
Stack of memory regions

\subsection{State and nondeterminism}

Different combinations have different meanings
\begin{spec}
import Control.Monad.Trans.List

StateT s [] a                  = s -> [(a, s)]
ListT (State s) a              = s -> ([a], s)
\end{spec}
Lazy evaluation in state \citep{fischer-purely-jfp}

\subsection{Monad transformers}
\citep{liang-interpreter}
\begin{spec}
import Control.Monad.Trans.Class
class MonadTrans t where
  lift :: (Monad m) => m a -> t m a
\end{spec}
Monad transformer laws
\begin{spec}
lift (return a)  =  return a
lift (m >>= k)   =  lift m >>= (lift . k)
\end{spec}
Operations need to be lifted too

\section{Parsing}

\citep{hutton-monadic-jfp}
\begin{spec}
StateT String [] a             = String -> [(a, String)]
\end{spec}
Everything follows from a few operations for |String| and nondeterminism:
\begin{spec}
instance Monad Parser
empty   :: Parser a
(<|>)   :: Parser a -> Parser a -> Parser a
item    :: Parser Char
\end{spec}
Efficiency concerns are tricky to reason about, because the data type does not express them
\begin{spec}
empty <|> p                =  p
p <|> empty                =  p
p <|> (q <|> r)            =  (p <|> q) <|> r
empty >>= f                =  empty
p >>= \_ -> empty          =  empty
(p <|> q) >>= f            =  (p >>= f) <|> (q >>= f)
p >>= (\a -> f a <|> g a)  =  (p >>= f) <|> (p >>= g)
\end{spec}

\section{Probability}

\citep{ramsey-stochastic}
\begin{spec}
WriterT (Product Double) [] a     = [(a, Product Double)]
\end{spec}
Everything follows from a few operations for |Product Double| and nondeterminism:
\begin{spec}
instance Monad Dist
empty   :: Dist a
(<|>)   :: Dist a -> Dist a -> Dist a
factor  :: Double -> Dist ()
\end{spec}

\newpage
\section{Neural nets}

\begin{align*}
\tag*{Examples}
    \boldsymbol{x}_i &\mapsto \boldsymbol{z}_i
\\
\tag*{Parameters}
    \boldsymbol{\theta} &
\\
\tag*{Loss}
    L(\boldsymbol{\theta}) &= \sum_i \bigl\lVert f(\mathbf{x}_i;\boldsymbol{\theta}) - \mathbf{z}_i \bigr\rVert^2
\\
\tag*{Update}
    \boldsymbol{\theta}^{(t+1)} &= \boldsymbol{\theta}^{(t)} 
    - \alpha \cdot \nabla L(\boldsymbol{\theta}^{(t)})
    + \beta \cdot (\boldsymbol{\theta}^{(t)} - \boldsymbol{\theta}^{(t-1)})
\end{align*}

\subsection{Linear regression}

\begin{align*}
\tag*{Examples}
     0 &\mapsto 26 \\
    10 &\mapsto 31 \\
    20 &\mapsto 40
\\
\tag*{Parameters}
    \boldsymbol{\theta} &= (\theta_0,\theta_1)
\\
\tag*{Line}
    f(x;\theta_0,\theta_1) &= \theta_0 + \theta_1 x
\end{align*}

\subsection{Perceptron}

\begin{align*}
\tag*{Examples}
    (-.9,-.9) &\mapsto +.9 \\
    (-.9,+.9) &\mapsto +.9 \\
    (+.9,-.9) &\mapsto +.9 \\
    (+.9,+.9) &\mapsto -.9
\\
\tag*{Parameters}
    \boldsymbol{\theta} &= (\theta_0,\theta_1,\theta_2)
\\
\tag*{Neuron}
    f(x,y;\theta_0,\theta_1,\theta_2) &= \sigma(\theta_0 + \theta_1 x + \theta_2 y)
\\
\tag*{Sigmoid}
    \sigma(r) &= \frac{2}{1 + e^{-r}} - 1
\end{align*}
\begin{center}
\begin{tikzpicture}[domain=-5:5]
    \draw [-{stealth}] (-5.2,0) -- (5.2,0) node[right] {$r$};
    \draw [-{stealth}] (0,-1.2) -- (0,1.2) node[above] {$\sigma(r)$};
    \draw [thick] plot (\x,{2/(1+exp(-\x))-1});
\end{tikzpicture}
\end{center}

\subsection{Network}

\begin{align*}
\tag*{Examples}
    (-.9,-.9) &\mapsto -.9 \\
    (-.9,+.9) &\mapsto +.9 \\
    (+.9,-.9) &\mapsto +.9 \\
    (+.9,+.9) &\mapsto -.9
\\
\tag*{Parameters}
    \boldsymbol{\theta} &= (\mathbf{a},\mathbf{b},\mathbf{c}) \\
    \mathbf{a} &= (a_0,a_1,a_2) \\
    \mathbf{b} &= (b_0,b_1,b_2) \\
    \mathbf{c} &= (c_0,c_1,c_2)
\\
\tag*{Network}
    f(x,y;\mathbf{a,b,c}) &= g\bigl(g(x,y;\mathbf{a}),g(x,y;\mathbf{b});\mathbf{c}\bigr)
\\
\tag*{Neuron}
    g(x,y;a_0,a_1,a_2) &= \sigma(a_0 + a_1 x + a_2 y)
\end{align*}
\begin{center}
\begin{tikzpicture}[x=4pc, y=3pc, neuron/.style={draw,circle}, every edge/.style={{stealth}-,draw}]
    \node          (y) at (0,1) {y};
    \node          (x) at (0,0) {x};
    \node [neuron] (b) at (1,1) {b} edge (x) edge (y);
    \node [neuron] (a) at (1,0) {a} edge (x) edge (y);
    \node [neuron] (c) at (2,.5) {c} edge (a) edge (b);
\end{tikzpicture}
\end{center}

\section{Automatic differentiation}

\citep{krawiec-provably}\nopagebreak
\begin{spec}
runIdentity . eval e   :: M.Map Name Double            -> Double
runIdentity . eval2 e  :: M.Map Name (Double, Double)  -> (Double, Double)

(runIdentity . eval2 e)(env)
==  (  (runIdentity . eval e)(fmap fst env)
    ,  nabla(runIdentity . eval e)(fmap fst env) `bullet` (fmap snd env)  )
  where  (`bullet`) :: M.Map Name Double -> M.Map Name Double -> Double
         um `bullet` vm = M.foldr (+) 0 (M.intersectionWith (*) um vm)

runIdentity . eval3 e  :: M.Map Name (Double, Delta)   -> (Double, Delta)

(runIdentity . eval3 e)(env)
==  (  (runIdentity . eval e)(fmap fst env)
    ,  nabla(runIdentity . eval e)(fmap fst env) `bullet` (fmap snd env)  )
  where  (`bullet`) :: M.Map Name Double -> M.Map Name Delta -> Delta
         um `bullet` vm = M.foldr dAdd M.empty (M.intersectionWith dScale um vm)

runDelta . eval4 e     :: M.Map Name (Double, Delta)   -> (Double, Delta)

(runDelta . eval4 e)(env)
==  (  (runIdentity . eval e)(fmap fst env)
    ,  nabla(runIdentity . eval e)(fmap fst env) `bullet` (fmap snd env)  )
  where  (`bullet`) :: M.Map Name Double -> M.Map Name Delta -> Delta
         um `bullet` vm = M.foldr DAdd Zero (M.intersectionWith DScale um vm)
\end{spec}

\newpage
\renewcommand{\bibliofont}{\normalsize}
\bibliographystyle{ACM-Reference-Format}
\bibliography{ccshan}

\clearpage
\end{CJK}
\end{document}
