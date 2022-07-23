\def~{\nobreakspace{}} % work around Debian bug 487974 and 534458 in texi2dvi
\pagenumbering{arabic}
\documentclass[aspectratio=169,professionalfonts]{beamer}
\usetheme{metropolis}
\geometry{papersize={400bp,225bp}}
\usepackage{microtype}
\usepackage[T1]{fontenc}
\usepackage[libertine]{newtxmath}
\usepackage[tt=false]{libertine}
\setmonofont[StylisticSet=3]{inconsolata}
\usepackage{xeCJK}
\setCJKmainfont[BoldFont={Noto Serif CJK TC Bold}]{Noto Serif CJK TC}
\setCJKsansfont[BoldFont={Noto Sans CJK TC Bold}]{Noto Sans CJK TC}
\setCJKmonofont[BoldFont={Noto Sans Mono CJK TC Bold}]{Noto Sans Mono CJK TC}
\usepackage{xeCJKfntef}

\usepackage{calc}
\usepackage{comment}

\usepackage{natbib}
\citestyle{acmauthoryear}
\bibliographystyle{ACM-Reference-Format}

\usepackage{tikz}
\usetikzlibrary{shapes.callouts}
\newcommand\remember[2]{\mbox{\tikz[remember picture,baseline,trim left=default,trim right=default]\node(#1)[anchor=base,inner sep=0]{$#2$};}}

% Record frame numbers as PS/PDF page labels
\mode<presentation>
{
  \setbeamertemplate{sidebar left}{\ifpdf
    \thispdfpagelabel{\insertframenumber}\else
    \setcounter{page}{\value{framenumber}}\fi}
}

%include preamble.lhs
%format square x = x "^2"
%format ... = "\dots"
%format ^  = "\,"
%format ^^ = "\;"
%format e1
%format e2
%format e3
%format v1
%format v2
%format v3
%format s1
%format s2
%format s3
%format (alert3 (content)) = "\alert<3>{" content "}"
%format ~> = "\mathinner\rightarrow"
%format (CASES (a) (b) (c)) = "\left\{\begin{matrix}" a "\cr\relax" b "\cr\relax" c "\end{matrix}\right."
\begin{comment}
\begin{code}
main = return ()
\end{code}
\end{comment}
\renewcommand{\plus}{\mathbin{+\!\!+}}
\renewcommand{\bind}{\mathbin{>\!\!>\mkern-6.7mu=}}
\renewcommand{\rbind}{\mathbin{=\mkern-6.7mu<\!\!<}}% suggested by Neil Mitchell
\renewcommand{\sequ}{\mathbin{>\!\!>}}
\makeatletter
\renewcommand{\hscodestyle}{\linespread{1}\selectfont}
\newenvironment{tophscode}
  {\hscodestyle\(%
   \def\PT@@begin{\array[t]{@@{}l@@{}}}%
   \let\PT@@end  \endarray
   \let\PT@@cr   \@@arraycr
   \expandafter\beginpolytable\ignorespaces}
  {\endpolytable\)}
\makeatother
\raggedbottom

\title{Monad 與副作用}
\date{2022-08}
\author{單中杰}
\begin{document}

\maketitle

\begin{frame}{暖身}
純遞迴 \texttt{Tree-1.hs}
\begin{itemize}
    \item 型別→用途→範例→策略→定義→測試 \citep{felleisen-design}
    \item 先盡量把 |sumTree| 跟 |productTree| 寫得相似，\\
          然後才把它們抽象成更一般的、可重複利用的模組
\end{itemize}
解譯器 \texttt{Arith-1.hs}
\begin{itemize}
    \item 隨機測試、property-based testing \citep{claessen-quickcheck}
    \item 進階練習：定義變數 \texttt{Arith-2.hs}
\end{itemize}
\end{frame}

\section{個別的副作用}

\begin{frame}{Accumulator passing}
基本上副作用就是一段程式除了把傳進來的引數變成傳回去的結果以外做的事情。

我們寫程式有時候會直觀想用副作用。印象最原始的是 state（狀態）：
\begin{spec}
result := 0
sumTree (Leaf n)        =  result := result + n;
                           result
sumTree (Branch t1 t2)  =  sumTree t1;
                           sumTree t2
\end{spec}
如此處理 |Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2))| 的方法是 |((0+3)+5)+2|\onslide<1>{ 還是 |3+(5+(2+0))| 還是 |3+(5+2)|？}

\onslide<2>{\texttt{TreeState-1.hs} 用 |sumTree'| 定義 |sumTree|}
\end{frame}

\begin{frame}{State threading}

\begin{spec}
next := 0
relabel (Leaf _)        =  next := next + 1;
                           Leaf next
relabel (Branch t1 t2)  =  Branch (relabel t1) (relabel t2)
\end{spec}
\texttt{TreeState-2.hs}
用 |relabel'| 定義 |relabel|
\begin{spec}
seen := S.empty
unique (Leaf n)        =  if S.member n seen then False
                          else seen := S.insert n seen; True
unique (Branch t1 t2)  =  unique t1 && unique t2
\end{spec}
用 |unique'| 定義 |unique|
（其實也可以用 |unique''| 定義 |unique|，那是比較不副作用、比較能平行化的作法）
\end{frame}

\begin{frame}[standout]
\noindent\includegraphics[width=\textwidth]{say-it}
把心目中的願望講出來，以便實現。所以如果心目中要的是副作用的話，就把副作用的\CJKunderline{意義}講出來。
\end{frame}

\begin{frame}{Local vs global state}
\texttt{UnionFind-1.hs}
\hfill Pointers, references, file system

\begin{tikzpicture}[>=stealth, trim left=-\mathindent,
                    code/.style={anchor=north west, align=left, inner sep=0}]
    \node (testState) at (0,4cm) [code] {$\mathhs\hscodestyle
\begin{spec}
testState :: State
testState = M.fromList
  [  (Key 100, Root 0 "A")
  ,  (Key 101, Link (Key 104))
  ,  (Key 102, Root 1 "C")
  ,  (Key 103, Link (Key 102))
  ,  (Key 104, Root 1 "E")
  ,  (Key 105, Root 0 "F")  ]
\end{spec}
    $};
    \begin{scope}[xshift=15em,yshift=2.5cm]
        \node (100) at (0,1) {100};
        \node (101) at (1,0) {101};
        \node (102) at (2,1) {102};
        \node (103) at (2,0) {103};
        \node (104) at (1,1) {104};
        \node (105) at (3,1) {105};
        \draw [->] (101) -- (104);
        \draw [->] (103) -- (102);
        \draw (-.7,-.5) rectangle (3.7,1.5);
    \end{scope}
    \onslide<1>{\path (testState.south west) ++(0,-\abovedisplayskip) node [code] {$\mathhs\hscodestyle
\begin{spec}
fresh  :: Info -> Key

find   :: Key -> (Key, Rank, Info)

union  :: Key -> Key -> ()
\end{spec}
    $};}
    \onslide<2>{\path (testState.south west) ++(0,-\abovedisplayskip) node [code] {$\mathhs\hscodestyle
\begin{spec}
fresh  :: Info -> State -> (Key, State)

find   :: Key -> State -> (Key, Rank, Info, State)

union  :: Key -> Key -> State -> State
\end{spec}
    $};}
    \onslide<3->{\node at (0,3.5cm) [code, fill=white] {$\mathhs\hscodestyle
\begin{spec}
testState' :: State
testState'  = M.fromList
  [  (Key 100, Root 0 "A")
  ,  (Key 101, Link (Key 104))
  ,  (Key 102, Link (Key 104))
  ,  (Key 103, Link (Key 102))
  ,  (Key 104, Root 2 "E")
  ,  (Key 105, Link (Key 108))
  ,  (Key 106, Link (Key 108))
  ,  (Key 107, Link (Key 106))
  ,  (Key 108, Root 2 "I")  ]
\end{spec}
    $};
    \begin{scope}[xshift=15em]
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
    \end{scope}}
\end{tikzpicture}
\end{frame}

\begin{frame}{State-threading interpreter}
\texttt{ArithState-1.hs}

進階練習：調撥記憶體 \texttt{ArithState-2.hs}
\mathindent=0pt
\begin{spec}
data Expr  =  Lit Int | Add Expr Expr | Mul Expr Expr

           |  New Expr       -- \text{把|Expr|的結果存到一個新allocate的}
                             -- \text{memory cell, 傳回該cell的address}

           |  Get Expr       -- \text{把|Expr|的結果當作一個address,}
                             -- \text{傳回該cell目前的內容}

           |  Put Expr Expr  -- \text{把第一個|Expr|的結果當作一個address,}
                             -- \text{存入第二個|Expr|的結果並傳回}

type State = [Int]           -- \text{記憶體內容}
\end{spec}
這怎麼會有用？
\end{frame}

\begin{frame}{Exception (|Maybe|)}
把中途跳脫的意義講出來
\begin{spec}
data Maybe     a = Nothing  | Just   a

data Either b  a = Left b   | Right  a
\end{spec}
\texttt{TreeMaybe-1.hs}
\begin{itemize}
    \item |decTree| 碰到非正數是錯誤
    \item |productTree| 碰到零有捷徑
\end{itemize}
\texttt{ArithMaybe-1.hs}
\begin{itemize}
    \item 除以零是錯誤
\end{itemize}
正常產生的|Just|需要``threading''
\end{frame}

\begin{frame}{二十一點}
每個數字遇到時都可以選擇要或是不要，但是一旦超過21就爆掉。\\
最後得分有哪些可能？
\[
    11, -1, 11\quad\rightarrow\quad\{-1,0,10,11,21\}
\]
\texttt{TreeNondet-1.hs}
\begin{spec}
blackjack' :: Tree -> Int -> [Int]
blackjack' (Leaf n)        total  =  if total + n > 21 then total
                                     else amb [total, total + n]
blackjack' (Branch t1 t2)  total  =  blackjack' t2 (blackjack' t1 total)
\end{spec}
用 |blackjack'| 定義 |blackjack|
\begin{spec}
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f as = concat (map f as)
\end{spec}
\end{frame}

\begin{frame}[standout]
\noindent\includegraphics[width=\textwidth]{eeaao}
Nondeterminism
\end{frame}

\begin{frame}{覆面算}
\mathindent=0pt
\begin{center}
    \ttfamily\hscodestyle
    \begin{tabular}{r>{\hspace*{4pc}}rr>{\hspace*{4pc}}rr}
           X\textsuperscript{2} &&    SEND &&    TO \\
        +\;Y\textsuperscript{2} && +\;MORE && +\;GO \\
        \cline{1-1} \cline{3-3} \cline{5-5} \vrule width0pt height2.5ex
           Z\textsuperscript{2} &&   MONEY &&   OUT
    \end{tabular}
\end{center}
\begin{overprint}
\onslide<1>
\begin{spec}
concatMap :: (a -> [b]) -> [a] -> [b]

concatMap  (\x -> concatMap  (\y -> concatMap  (\z ->  if square x + square y == square z
                                                       then [(x,y,z)]
                                                       else [])
                                               [0..9])
                             [0..9])
           [0..9]
\end{spec}
\onslide<2>
\begin{spec}
type Digit = Int
digit :: (Digit -> [Answer]) -> Answer

digit (\x -> digit (\y -> digit (\z ->  if square x + square y == square z
                                        then [(x,y,z)]
                                        else [])))
\end{spec}
可以把每一個 loop body 想成一個 |Digit| 的 continuation\\
所以「|digit (\x ->|」好像一個命令
\onslide<3>
\begin{spec}
concatMap  (\d -> concatMap  (\e -> concatMap  (\y ->  if mod (d + e) 10 == y
                                                       then ...
                                                       else [])
                                               ([0..9] \\ [d,e]))
                             ([0..9] \\ [d]))
           [0..9]
\end{spec}
趁早檢查，免得做白工
\onslide<4>
\begin{spec}
type Chosen = [Digit]
digit :: (Digit -> Chosen -> [Answer]) -> Chosen -> [Answer]

digit (\d -> digit (\e -> digit (\y ->  if mod (d + e) 10 == y
                                        then ...
                                        else ^^ \chosen -> [])))
\end{spec}
\texttt{Crypta-1.hs}
\onslide<5>
\begin{spec}
type Chosen = [(Char,Digit)]
digit :: Char -> (Digit -> Chosen -> [Answer]) -> Chosen -> [Answer]

add 'D' 'E' 'Y' ...
\end{spec}
\texttt{Crypta-2.hs} 適合自資料檔讀取新題
\end{overprint}
\end{frame}

\begin{frame}{Nondeterministic interpreter}
\texttt{ArithNondet-1.hs}
\begin{spec}
data Expr = ... | Amb Expr Expr
\end{spec}
\citep{mccarthy-basis}
\end{frame}

\section{Monad}

\begin{frame}[t]{把副作用抽象成monad \hfill\mdseries\citep{moggi-abstract,wadler-monads}}
\small\sethscode{tophscode}\hspace*{-9mm}%
\begin{tikzpicture}[note/.style={rectangle callout, anchor=pointer,
                                 draw=alerted text.fg,
                                 fill=alerted text.bg,
                                 text=alerted text.fg}]
    \matrix [ampersand replacement=\&, anchor=base west, inner sep=0,
             column sep=.8em, row sep=2ex]
    {
        \node (lit) {|eval (Lit v) =|};
    \&
        \onslide<1>{\node (add) {|eval (Add e1 e2) =|};}
        \onslide<2->{\node (mul) {|eval (Mul e1 e2) =|};}
    \&
        \onslide<1>{\node (neg) {|eval (Neg e1) =|};}
        \onslide<2->{\node (if) {|eval (If e1 et ef ^) =|};}
    \\
        \onslide<-2>{\node (lit state) {|\s -> (v, s)|};}
        \onslide<3->{\node (return state) {%
\begin{spec}
alert3 (return v =)
  \s -> (v, s)
\end{spec}
        };}
    \&
        \onslide<1>{\node (add state) {%
\begin{spec}
\s ->  let  (v1, s1)  = eval e1 s
            (v2, s2)  = eval e2 s1
       in   (v1 + v2, s2)
\end{spec}
        };}
        \onslide<2-5>{\node (mul state) {%
\begin{spec}
\s ->  let  (v1, s1)  = eval e1 s
            (v2, s2)  = eval e2 s1
       in   (v1 * v2, s2)
\end{spec}
        };}
    \&
        \onslide<1>{\node (neg state) {%
\begin{spec}
\s ->  let  (v1, s1)  = eval e1 s
       in   (-v1, s1)
\end{spec}
        };}
        \onslide<2-5>{\node (if state) {%
\begin{spec}
\s ->  let (v1, s1)  = eval e1 s
       in eval (if v1  then et
                       else ef ^) s1
\end{spec}
        };}
    \\
        \onslide<-2>{\node (lit maybe) {|Just v|};}
        \onslide<3->{\node (return maybe) {%
\begin{spec}
alert3 (return v =)
  Just v
\end{spec}
        };}
    \&
        \onslide<1>{\node (add maybe) {%
\begin{spec}
case eval e1 of
  Nothing  ->  Nothing
  Just v1  ->  case eval e2 of
                 Nothing  ->  Nothing
                 Just v2  ->  Just (v1 + v2)
\end{spec}
        };}
        \onslide<2-5>{\node (mul maybe) {%
\begin{spec}
case eval e1 of
  Nothing  ->  Nothing
  Just v1  ->  case eval e2 of
                 Nothing  ->  Nothing
                 Just v2  ->  Just (v1 * v2)
\end{spec}
        };}
    \&
        \onslide<1>{\node (neg maybe) {%
\begin{spec}
case eval e1 of
  Nothing  ->  Nothing
  Just v1  ->  Just (-v1)
\end{spec}
        };}
        \onslide<2-5>{\node (if maybe) {%
\begin{spec}
case eval e1 of
  Nothing  ->  Nothing
  Just v1  ->  eval (if v1  then et
                            else ef ^)
\end{spec}
        };}
    \\
        \onslide<-2>{\node (lit nondet) {|[v]|};}
        \onslide<3->{\node (return nondet) {%
\begin{spec}
alert3 (return v =)
  [v]
\end{spec}
        };}
    \&
        \onslide<1>{\node (add nondet) {%
\begin{spec}
concatMap  (\v1 -> map  (\v2 -> v1+v2)
                        (eval e2))
           (eval e1)
\end{spec}
        };}
        \onslide<2->{\node (mul nondet) {%
\begin{spec}
concatMap  (\v1 -> map  (\v2 -> v1*v2)
                        (eval e2))
           (eval e1)
\end{spec}
        };}
    \&
        \onslide<1>{\node (neg nondet) {%
\begin{spec}
map  (\v1 -> -v1)
     (eval e1)
\end{spec}
        };}
        \onslide<2->{\node (if nondet) {%
\begin{spec}
concatMap  (\v1 -> eval (if v1  then et
                                else ef ^))
           (eval e1)
\end{spec}
        };}
    \\
    };
    \begin{scope}[transform canvas={xshift=-.4em, yshift=-1ex}]
        \draw (add.north west) ++ (0,2ex) -- (add nondet.south west);
        \begin{scope}[transform canvas={yshift=2ex}]
            \draw (neg.north west) -- (neg state.north west);
            \onslide<-5>{\draw (neg state.north west) -- (neg nondet.north west);}
        \end{scope}
        \draw (neg nondet.north west) ++ (0,2ex) -- (add nondet.south west -|| neg nondet.south west);
    \end{scope}
    \useasboundingbox;
    \draw (add state.north west) ++ (-2in,1ex) -- ++ (9in,0);
    \draw (add maybe.north west) ++ (-2in,1ex) -- ++ (9in,0);
    \draw (add nondet.north west) ++ (-2in,1ex) -- ++ (9in,0);
    \onslide<4>{
        \begin{scope}[every node/.style={note, callout relative pointer={(-1em,1.2ex)}}]
            \path (return state.base west) ++(2.5em,0) node {|return :: a -> s -> (a,s)|}
                  (return maybe.base west) ++(2.5em,0) node {|return :: a -> Maybe a|}
                  (return nondet.base west)++(2.5em,0) node {|return :: a -> [a]|};
        \end{scope}
    }
    \onslide<5>{
        \path (mul nondet.base west) ++(4.3em,0) node [note, callout relative pointer={(-.5em,1.2ex)}] {|concatMap :: (a -> [b]) -> [a] -> [b]|}
              (mul nondet.north west) ++(9.5em,-.6ex) node [note, callout relative pointer={(-.5em,-1.2ex)}] {%
\begin{spec}
map :: (a -> b) -> [a] -> [b]
map f = concatMap (return . f ^)
\end{spec}
              };
    }
    \onslide<6>{
        \begin{scope}[every node/.style={anchor=base west, inner sep=0, text=alerted text.fg}]
            \node at (mul state.base west) {%
\begin{spec}
concatMap :: (a ~> State ~> (b, State)) ~> (State ~> (a, State)) ~> (State ~> (b, State))
concatMap f m = \s -> let (a, s1) = m s in f a s1
concatMap f m = uncurry f . m
\end{spec}
            };
            \node at (mul maybe.base west) {%
\begin{spec}
concatMap :: (a -> Maybe b) -> Maybe a -> Maybe b
concatMap f Nothing   = Nothing
concatMap f (Just a)  = f a
\end{spec}
            };
        \end{scope}
    }
\end{tikzpicture}
\end{frame}

\begin{frame}{抽象完畢}
\mathindent=0pt
\begin{spec}
eval :: Expr -> M Int
eval (Lit v)      = return v
eval (Add e1 e2)  = concatMap  (\v1 -> concatMap  (\v2 -> return (v1+v2))
                                                  (eval e2))
                               (eval e1)
\end{spec}
\begin{spec}
type M a = CASES (State -> (a, State)) (Maybe a) [a]

return     :: a -> M a                  -- unit, pure, eta $\eta$
concatMap  :: (a -> M b) -> M a -> M b  -- bind, |=<<|, $\cdot^\star$
\end{spec}
\end{frame}

\begin{frame}{Monad laws}
\begin{spec}
return  :: a -> M a
(>>=)   :: M a -> (a -> M b) -> M b

return a >>= k           = k a
m >>= return             = m
m >>= (\a -> k a >>= l)  = (m >>= k) >>= l
\end{spec}
\vspace*{-\belowdisplayskip}
\begin{overprint}
\onslide<1>
檢查具體特例。\hfill\texttt{Laws-1.hs}\hfill
用|Int|以外的型別呢？|[]|以外的monad呢？
\begin{spec}
type M a = [a]
a  = 9                    :: Int
k  = (\n -> [1..n])       :: Int -> M Int
m  = [5,3]                :: M Int
l  = (\n -> [n, n * 10])  :: Int -> M Int
\end{spec}
\onslide<2>
原本的定義：
\begin{spec}
return  :: a -> M a
fmap    :: (a -> b) -> M a -> M b
join    :: M (M a) -> M a
\end{spec}
\end{overprint}
\end{frame}

\begin{frame}[allowframebreaks=1]{References}
\renewcommand\bibsection{}
\renewcommand\bibfont{\hscodestyle\footnotesize}
\bibliography{ccshan}
\end{frame}

\end{document}





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
\texttt{StateIO-1.hs}
\texttt{StateIO-2.hs}

\subsection{State and exception}
\texttt{StateMaybe-1.hs}
\texttt{StateMaybe-2.hs}

\subsection{State and nondeterminism}
\texttt{StateNondet-1.hs}
\texttt{StateNondet-2.hs}

\citep{fischer-purely-jfp}

\subsection{Monad transformers}
\texttt{StateIO-3.hs}
\texttt{StateMaybe-3.hs}
\texttt{StateMaybe-4.hs}
\texttt{StateNondet-3.hs}
\texttt{StateNondet-4.hs}
\citep{liang-interpreter}

\begin{spec}
StateT s IO                    = s -> IO (a, s)

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

\texttt{Diff-1.hs}
\citep{claessen-quickcheck}

\texttt{Diff-2.hs}
|randomParams >>= optimize|

\texttt{Diff-3.hs}
\texttt{Diff-4.hs}
\texttt{Diff-5.hs}
