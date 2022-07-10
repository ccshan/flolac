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

\subsection{Challenge: local variable binding}
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
\input{UnionFind}
Pointers, references, file system

\subsection{Interpreter}
\texttt{ArithState-1.hs}

Challenge: Memory allocation
\begin{spec}
data Expr  =  Lit Int | Add Expr Expr | Mul Expr Expr
           |  New Expr | Get Expr | Put Expr Expr
\end{spec}
How can this be useful?

\section{Exception}

\subsection{Tree}
\input{TreeMaybe}

\subsection{Interpreter}
\input{ArithMaybe}

\section{Nondeterminism}

\subsection{Tree}
\input{TreeNondet}

\subsection{SEND + MORE = MONEY}
\input{SendMoreMoney}

\subsection{Interpreter}
\input{ArithNondet}

\section{Monads}

Abstract from $\Conid{Expr}$ interpreter. \citep{wadler-monads}

\section{Type classes}

Examples: $\Conid{Eq}$, $\Conid{Ord}$, $\Conid{Show}$. \citep{wadler-ad-hoc}

$\Conid{Monad}$ class, inheriting from $\Conid{Applicative}$, inheriting from $\Conid{Functor}$.

\input{ArithMonad}

\section{Imperative programming}
\input{ArithIO}

\begin{quote}
    A value of type |IO a| is an ``action'' that, when performed, may do some
    input/output, before delivering a value of type~|a|.

    |type IO a = World -> (a, World)|
\end{quote}

Execution by \emph{monad laws} and labeled transitions (\citep[Figure~3]{peyton-jones-tackling})

Translating impure programs to monadic form

\subsection{Do notation}
\input{ArithDo}

\subsection{Polymorphism across monads}
\input{Traverse}

\begin{solution}
\begin{spec}
m >>= k   = join (fmap k m)

fmap f m  = m >>= \a -> return (f a)
join m    = m >>= id
\end{spec}
\end{solution}

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
