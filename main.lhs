\documentclass[acmsmall,nonacm,timestamp]{acmart}
\citestyle{acmauthoryear}

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
\arrayhs
\raggedbottom

\usepackage{comment}
\excludecomment{solution}

\usepackage{tikz}

\begin{document}

\title{Monad and side effects}
\author{Chung-chieh Shan}
\authorsaddresses{}
\maketitle

Theme: To get what you want, say what you mean.
So, if you want to do something, say what doing it means.

\section{Warm up}

\subsection{Pure recursion}
\texttt{Tree-1.hs}

For modular reuse, abstract from similarities over differences: |sumTree| vs |productTree|

\subsection{Interpreter}
\texttt{Arith-1.hs}

Challenge: local variable binding
\texttt{Arith-2.hs}

\section{State}

Certain programming tasks make us intuitively reach for side effects.

Basically, a side effect is something that a piece of code does besides turning input arguments into return values.

\subsection{Accumulator passing}

\texttt{TreeState-1.hs}

\begin{spec}
result := 0
sumTree (Leaf n)        =  result := result + n;
                           result
sumTree (Branch t1 t2)  =  sumTree t1;
                           sumTree t2
\end{spec}

\subsection{State threading}

\texttt{TreeState-2.hs}

\begin{spec}
next := 0
relabel (Leaf _)        =  next := next + 1;
                           Leaf next
relabel (Branch t1 t2)  =  Branch (relabel t1) (relabel t2)
\end{spec}

\begin{spec}
seen := S.empty
unique (Leaf n)        =  if S.member n seen then False
                          else seen := S.insert n seen; True
unique (Branch t1 t2)  =  unique t1 && unique t2
\end{spec}

\noindent\includegraphics[width=\textwidth]{say-it}

\subsection{Local vs global state}
\texttt{UnionFind-1.hs}

\noindent
\begin{minipage}[t]{22em}
\begin{spec}
testState :: State
testState = M.fromList  [  (Key 100, Root 0 "A")
                        ,  (Key 101, Link (Key 104))
                        ,  (Key 102, Root 1 "C")
                        ,  (Key 103, Link (Key 102))
                        ,  (Key 104, Root 1 "E")
                        ,  (Key 105, Root 0 "F")  ]
\end{spec}
\end{minipage}
\begin{tikzpicture}[>=stealth,baseline=2cm]
    \node (100) at (0,1) {100};
    \node (101) at (1,0) {101};
    \node (102) at (2,1) {102};
    \node (103) at (2,0) {103};
    \node (104) at (1,1) {104};
    \node (105) at (3,1) {105};
    \draw [->] (101) -- (104);
    \draw [->] (103) -- (102);
    \draw (-.7,-.5) rectangle (3.7,1.5);
\end{tikzpicture}

\noindent
\begin{minipage}[t]{22em}
\begin{spec}
testState' :: State
testState'  = M.fromList   [  (Key 100, Root 0 "A")
                           ,  (Key 101, Link (Key 104))
                           ,  (Key 102, Link (Key 104))
                           ,  (Key 103, Link (Key 102))
                           ,  (Key 104, Root 2 "E")
                           ,  (Key 105, Link (Key 108))
                           ,  (Key 106, Link (Key 108))
                           ,  (Key 107, Link (Key 106))
                           ,  (Key 108, Root 2 "I")  ]
\end{spec}
\end{minipage}
\begin{tikzpicture}[>=stealth,baseline=2cm]
    \node (100) at (0,1) {100};
    \node (101) at (1,0) {101};
    \node (102) at (2,1) {102};
    \node (103) at (2,0) {103};
    \node (104) at (1,1) {104};
    \node (105) at (3,0) {105};
    \node (106) at (4,1) {106};
    \node (107) at (4,0) {107};
    \node (108) at (3,1) {108};
    \draw [->] (101) -- (104);
    \draw [->] (102) -- (104);
    \draw [->] (103) -- (102);
    \draw [->] (105) -- (108);
    \draw [->] (106) -- (108);
    \draw [->] (107) -- (106);
    \draw (-.7,-.5) rectangle (4.7,1.5);
\end{tikzpicture}

Pointers, references, file system

\subsection{Interpreter}
\texttt{ArithState-1.hs}

Challenge: Memory allocation
\texttt{ArithState-2.hs}
\begin{spec}
data Expr  =  Lit Int | Add Expr Expr | Mul Expr Expr
           |  New Expr | Get Expr | Put Expr Expr
type State = [Int]
\end{spec}
How can this be useful?

\section{Exception}

\subsection{Tree}
\texttt{TreeMaybe-1.hs}

\subsection{Interpreter}
\texttt{ArithMaybe-1.hs}

\section{Nondeterminism}

\subsection{Tree}
\texttt{TreeNondet-1.hs}
\begin{spec}
blackjack' :: Tree -> Int -> [Int]
blackjack' (Leaf n)        total  =  if total + n > 21 then total
                                     else amb [total, total + n]
blackjack' (Branch t1 t2)  total  =  blackjack' t2 (blackjack' t1 total)
\end{spec}

\subsection{SEND + MORE = MONEY}
\texttt{Crypta-1.hs}

Loop bodies are continuation functions.

Prepone checking to avoid futile generation.

\texttt{Crypta-2.hs}

Use state to remember letters whose digits have been chosen.

Generalize to TO + GO = OUT.

\subsection{Interpreter}
\texttt{ArithNondet-1.hs}

\section{Monads}

Abstract from $\Conid{Expr}$ interpreter. \citep{wadler-monads}

\section{Type classes}

Examples: $\Conid{Eq}$, $\Conid{Ord}$, $\Conid{Show}$. \citep{wadler-ad-hoc}

$\Conid{Monad}$ class, inheriting from $\Conid{Applicative}$, inheriting from $\Conid{Functor}$.

\texttt{ArithMonad-1.hs}
\texttt{ArithMonad-2.hs}
\texttt{ArithMonad-3.hs}

\section{Imperative programming}
\texttt{ArithIO-1.hs}

How (and in what monad) to interpret |Input| and |Output| is an open-ended question.

\begin{quote}
    A value of type |IO a| is an ``action'' that, when performed, may do some
    input/output, before delivering a value of type~|a|.

    |type IO a = World -> (a, World)|
\end{quote}

Execution by \emph{monad laws} and labeled transitions (\citep[Figure~3]{peyton-jones-tackling})

Translating impure programs to monadic form

\subsection{Do notation}
\texttt{ArithDo-1.hs}
\texttt{ArithDo-2.hs}
\texttt{ArithDo-3.hs}
\texttt{ArithDo-4.hs}

\subsection{Polymorphism across monads}
\texttt{Traverse-1.hs}

\section{Combining side effects}

\subsection{State and IO}
\input{StateIO}

\subsection{State and exception}
\input{StateMaybe}

\subsection{State and nondeterminism}
\input{StateNondet}
\citep{fischer-purely-jfp}

\subsection{Monad transformers}
\citep{liang-interpreter}
\begin{spec}
StateT s Maybe a               = s -> Maybe (a, s)
MaybeT (State s) a             = s -> (Maybe a, s)
StateT s (MaybeT (State t)) a  = s -> t -> (Maybe (a, s), t)

StateT s [] a                  = s -> [(a, s)]
ListT (State s) a              = s -> ([a], s) -- for profiling search?
\end{spec}
Lifting operations is ad hoc

\section{Parsing}
\citep{hutton-monadic-jfp}
\begin{spec}
type Parser = StateT String []
\end{spec}
Prove monad laws

Disprove left distributivity for $+\!\!+\!\!+$

|apply expr " 1 - 2 * 3 + 4 "| with $(+\!\!+\!\!+) = (\langle\vert\rangle)$

\section{Probability}
\citep{ramsey-stochastic}
\begin{spec}
type Dist = WriterT (Product Double) []
\end{spec}

\section{Neural nets}

\section{Automatic differentiation}
\citep{krawiec-provably}
\input{Diff}

\bibliographystyle{mcbride}
\bibliography{ccshan}

\end{document}
