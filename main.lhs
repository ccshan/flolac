\def~{\nobreakspace{}} % work around Debian bug 487974 and 534458 in texi2dvi
\pagenumbering{arabic}
\PassOptionsToPackage{hyphens}{url}
\documentclass[aspectratio=169,professionalfonts]{beamer}
\usetheme[block=fill]{metropolis}
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
\usepackage{mathtools}
\usepackage{booktabs}
\usepackage{qrcode}

\usepackage{prooftree1}
\proofrulebaseline=1.7ex

\usepackage{natbib}
\citestyle{acmauthoryear}
\bibliographystyle{ACM-Reference-Format}

\usepackage{tikz}
\usetikzlibrary{shapes.callouts}
\tikzset{alerted/.style={draw=alerted text.fg, fill=alerted text.bg, text=alerted text.fg}}
\newcommand\remember[2]{\mbox{\tikz[remember picture,baseline,trim left=default,trim right=default]\node(#1)[anchor=base,inner sep=0]{$#2$};}}
\newcommand<>\inlinenote[4]{%
    \begin{tikzpicture}[baseline=(about.base)]
        \node (about) [inner sep=0] {#1};
        \onslide#5{\node at (about.#2) [overlay, rectangle callout, callout relative pointer={#3}, anchor=pointer, alerted, text depth=.2ex] {#4};}
    \end{tikzpicture}}

% https://tex.stackexchange.com/questions/150737/tikz-callout-positioning-start-of-pointer
\usepackage{ted}
\makeatletter
\newlength{\callout@@move@@pointer@@start@@x}
\newlength{\callout@@move@@pointer@@start@@y}
\pgfkeys{/pgf/.cd,
    callout pointer xshift/.code={\setlength{\callout@@move@@pointer@@start@@x}{#1}},
    callout pointer yshift/.code={\setlength{\callout@@move@@pointer@@start@@y}{#1}}}
\Substitute*[\def\pgf@@lib@@rectanglecallout@@pointer]{\pgf@@lib@@rectanglecallout@@pointer}%
    {\advance\pgf@@x\pgf@@xb
     \advance\pgf@@y\pgf@@yb}
    {\advance\pgf@@x\pgf@@xb
     \advance\pgf@@x\callout@@move@@pointer@@start@@x
     \advance\pgf@@y\pgf@@yb%
     \advance\pgf@@y\callout@@move@@pointer@@start@@y}
\makeatother

% Record frame numbers as PS/PDF page labels
\mode<presentation>
{
  \setbeamertemplate{sidebar left}{\ifpdf
    \thispdfpagelabel{\insertframenumber}\else
    \setcounter{page}{\value{framenumber}}\fi}
}

%include preamble.lhs
%format square x = x "^2"
%format ... = "\dots "
%format ^   = "\,"
%format ^^^ = "\qquad "
%format !!! = "{}"
%format e1
%format e2
%format e3
%format v1
%format v2
%format v3
%format s1
%format s2
%format s3
%format a1
%format a2
%format b1
%format b2
%format c1
%format c2
%format t1
%format t2
%format t1'
%format t2'
%format (remember  (content)) = "\remember{it}{"  content "}"
%format (remember0 (content)) = "\remember{it0}{" content "}"
%format (remember1 (content)) = "\remember{it1}{" content "}"
%format (remember2 (content)) = "\remember{it2}{" content "}"
%format (remember3 (content)) = "\remember{it3}{" content "}"
%format (remember4 (content)) = "\remember{it4}{" content "}"
%format (remember5 (content)) = "\remember{it5}{" content "}"
%format (remember6 (content)) = "\remember{it6}{" content "}"
%format (remember7 (content)) = "\remember{it7}{" content "}"
%format (remember8 (content)) = "\remember{it8}{" content "}"
%format (remember9 (content)) = "\remember{it9}{" content "}"
%format (alert     (content)) = "\alert{"         content "}"
%format (alert3    (content)) = "\alert<3>{"      content "}"
%format ~> = "\mathinner\rightarrow"
%format (CASES (a) (b) (c)) = "\left\{\begin{array}{@{}l@{}}" a "\cr\relax" b "\cr\relax" c "\end{array}\right."
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

\newcommand\exercise[1]{\texttt{\usebeamercolor[fg]{example text}#1.hs}}

\title{Monad 與副作用}
\date{2022-08}
\author{單中杰}
\begin{document}

\begin{frame}[standout]{\inserttitle\hfill\insertauthor\ \insertdate}
    \noindent
    \includegraphics[height=.6\paperheight]{wallace}\hfill
    \raisebox{\depth}{\qrcode[height=.6\paperheight]{https://drive.google.com/drive/folders/1oju5XL5sAlKe4n6T6dXfYpU1a1meVt6t?usp=sharing}}

    \scriptsize\mdseries
    \medskip

    \noindent
    \url{https://drive.google.com/drive/folders/1oju5XL5sAlKe4n6T6dXfYpU1a1meVt6t?usp=sharing}

    \noindent
    \url{https://drive.google.com/drive/folders/1qRhU05xbDVoXqgYLUuBbGYcFavCPDdgy?usp=sharing}
\end{frame}

\begin{frame}{暖身}
純遞迴 \exercise{Tree1}
\begin{itemize}
    \item 型別→用途→範例→策略→定義→測試 \citep{felleisen-design}
    \item 先盡量把 |sumTree| 跟 |productTree| 寫得相似，\\
          然後才把它們抽象成更一般的、可重複利用的模組
\end{itemize}
解譯器 \exercise{Arith1}
\begin{itemize}
    \item 隨機測試、property-based testing \citep{claessen-quickcheck}
    \item 進階練習：定義變數 \exercise{Arith2}
    \item 進階練習：用|Expr|的fold表達|eval|
\end{itemize}
\end{frame}

\section{個別的副作用}

\begin{frame}{Accumulator passing}
基本上副作用(side effect)就是一段程式除了把傳進來的引數變成傳回去的結果以外做的事情。

我們寫程式有時候會直觀想用副作用。印象最原始的是 state（狀態）：
\begin{spec}
result := 0
sumTree (Leaf n)        =  result := result + n;
                           result
sumTree (Branch t1 t2)  =  sumTree t1;
                           sumTree t2
\end{spec}
如此處理 |Branch (Leaf 3) (Branch (Leaf 5) (Leaf 2))| 的方法是 |((0+3)+5)+2|\onslide<1>{ 還是 |3+(5+(2+0))| 還是 |3+(5+2)|？}

\onslide<2>{\exercise{TreeState1} 用 |sumTree'| 定義 |sumTree|}
\end{frame}

\begin{frame}{State threading}

\begin{spec}
next := 0
relabel (Leaf _)        =  next := next + 1;
                           Leaf next
relabel (Branch t1 t2)  =  Branch (relabel t1) (relabel t2)
\end{spec}
\exercise{TreeState2}
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
\exercise{UnionFind1}
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
\exercise{ArithState1}

進階練習：調撥記憶體 \exercise{ArithState2}
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
\exercise{TreeMaybe1}
\begin{itemize}
    \item |decTree| 碰到非正數是錯誤
    \item |productTree| 碰到零有捷徑
\end{itemize}
\exercise{ArithMaybe1}
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
\exercise{TreeNondet1}
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

\begin{frame}{Nondeterministic interpreter}
\exercise{ArithNondet1}
\begin{spec}
data Expr = ... | Amb Expr Expr
\end{spec}
\citep{mccarthy-basis}
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
\vspace{-2ex}
\begin{overprint}
\onslide<-2>
\begin{spec}
concatMap :: (a -> [b]) -> [a] -> [b]

remember0 concatMap  (\x -> remember2 concatMap  (\y -> remember4 concatMap  (\z ->  remember6 (!!! if square x + square y == square z)
                                                                                     then [(x,y,z)] else [])
                                                                             (remember5 [0..9]))
                                                 (remember3 [0..9]))
                     (remember1 [0..9])
\end{spec}
\begin{tikzpicture}[remember picture, overlay]
\only<2>{
    \foreach \nw/\se/\ne in {it0/it1/it2,it2/it3/it4,it4/it5/it6}
        \draw [alerted text.fg, transform canvas={xshift=-1pt,yshift=-1pt}]
            (\nw.north -|| \ne.west) ++(-1pt,2pt)
            -|| (\nw.west ||- \se.south)
            -- (\se.south east) node [anchor=north east, inner sep=0] {好像一個命令}
            -- ++(0,.8\baselineskip)
            -|| cycle;
}
\end{tikzpicture}%
\onslide<3-4>
\begin{spec}
concatMap :: (a -> [b]) -> [a] -> [b]

remember0 concatMap  (\d -> remember2 concatMap  (\e -> remember4 concatMap  (\y ->  remember6 (!!! if mod (d + e) 10 == y)
                                                                                     then ... else [])
                                                                             (remember5 (([0..9] \\ [d,e]))))
                                                 (remember3 (([0..9] \\ [d]))))
                     (remember1 [0..9])
\end{spec}
\begin{tikzpicture}[remember picture, overlay]
\only<4>{
    \foreach \nw/\se/\ne in {it0/it1/it2,it2/it3/it4,it4/it5/it6}
        \draw [alerted text.fg, transform canvas={xshift=-1pt,yshift=-1pt}]
            (\nw.north -|| \ne.west) ++(-1pt,2pt)
            -|| (\nw.west ||- \se.south)
            -- (\se.south east) node [anchor=north east, inner sep=0] {好像一個命令}
            -- ++(0,.8\baselineskip)
            -|| cycle;
}
\end{tikzpicture}%
趁早檢查，免得做白工
\onslide<5>
\begin{spec}
type Digit = Int ^^^ type Chosen = [Digit] ^^^ -- \exercise{Crypta1}
digit :: alert Chosen -> [(Digit, alert Chosen)]

concatMap  (\(d,chosen) ->
             concatMap  (\(e,chosen) ->
                          concatMap  (\(y,chosen) ->  if mod (d + e) 10 == y
                                                      then ... else [])
                                     (digit chosen))
                        (digit chosen))
           (digit chosen)
\end{spec}
\onslide<6>
\begin{spec}
type Digit = Int ^^^ type Chosen = [(alert Char, Digit)] ^^^ -- \exercise{Crypta2}
digit :: alert Char -> Chosen -> [(Digit, Chosen)]

concatMap  (\(carry,chosen) -> ...)
           (add 'D' 'E' 'Y' ...)
\end{spec}
適合自資料檔讀取新題
\end{overprint}
\end{frame}

\section{Monad}

\begin{frame}[t]{把副作用抽象成monad \hfill\mdseries\citep{moggi-abstract,wadler-monads}}
\small\sethscode{tophscode}\hspace*{-9mm}%
\begin{tikzpicture}[note/.style={rectangle callout, anchor=pointer, alerted}]
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
eval :: Expr -> alert M Int
eval (Lit v)      = return v
eval (Add e1 e2)  = concatMap  (\v1 -> concatMap  (\v2 -> return (v1+v2))
                                                  (eval e2))
                               (eval e1)
\end{spec}
先做|eval e1|這個\alert{動作}，再拿結果|u1|去做另一個\alert{動作}……
\begin{spec}
type alert M a = CASES (State -> (a, State)) (Maybe a) [a]

return     :: a -> M a                  -- unit, pure, eta $\eta$
concatMap  :: (a -> M b) -> M a -> M b  -- bind, |=<<|, $\cdot^\star$
\end{spec}
\end{frame}

\begin{frame}{Monad laws}
\mathindent=0pt
\begin{spec}
return  :: a -> M a
(>>=)   :: M a -> (a -> M b) -> M b

return a >>= k           = k a
m >>= return             = m
m >>= \a -> (k a >>= l)  = (m >>= k) >>= l
\end{spec}
\vspace*{-\belowdisplayskip}
\begin{overprint}
\onslide<1>
檢查具體特例。\hfill\exercise{Laws1}\hfill
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

\section{Type classes}

\begin{frame}{動機：很重要所以只說一遍}
\mathindent=0pt
\begin{spec}
elem ::  alert a  ->  [alert a]     ->  Bool
elem     x            []            =   False
elem     x            (y:ys)        =   alert (x == y) || elem x ys
\end{spec}
\pause
\vspace*{-1\abovedisplayskip-1\belowdisplayskip}
\begin{spec}
elemInt  ::  alert Int   ->   [alert Int]   ->  Bool
elemInt      x                []            =   False
elemInt      x                (y:ys)        =   alert eqInt x y || elemInt x ys

elemChar ::  alert Char  ->   [alert Char]  ->  Bool
elemChar     x                []            =   False
elemChar     x                (y:ys)        =   alert eqChar x y || elemChar x ys
\end{spec}
\pause
\vspace*{-1\abovedisplayskip-1\belowdisplayskip}
\begin{spec}
elemBy ::  alert ((a -> a -> Bool))  ->  alert a  ->  [alert a]   ->  Bool
elemBy     (alert eq)                    x            []          =   False
elemBy     (alert eq)                    x            (y:ys)      =   alert eq x y || elemBy eq x ys
\end{spec}
\end{frame}

\begin{frame}{模組的\alt<1>{使用}{提供}者}
\mathindent=0pt
\begin{spec}
type Eq a = a -> a -> Bool
\end{spec}
\vspace*{-1\abovedisplayskip-1\belowdisplayskip}
\begin{overprint}
\onslide<1>
\begin{spec}
lookupBy ::  Eq a  ->  a  ->  [(a, b)]     ->  Maybe b
lookupBy     eq        x      []           =   Nothing
lookupBy     eq        x      ((y,b):ybs)  =   if alert eq x y then Just b
                                               else lookupBy eq x ybs

nubBy ::  Eq a  ->  [a]  ->  [a]
nubBy     eq        xs   =   alert (nubBy' eq) xs []

nubBy' ::  Eq a  ->  [a]  ->  [a]   ->  [a]
nubBy'     eq        []       seen  =   []
nubBy'     eq        (x:xs)   seen  =   if alert (elemBy eq) x seen then nubBy' eq xs seen
                                        else x : nubBy' eq xs (x:seen)
\end{spec}
\onslide<2>
\begin{spec}
eqPair :: Eq a -> Eq b -> Eq (a,b)
eqPair eq_a eq_b (a1,b1) (a2,b2) = eq_a a1 a2 && eq_b b1 b2

eqList :: Eq a -> Eq [a]
eqList eq_a []      []      = True
eqList eq_a (x:xs)  (y:ys)  = eq_a x y && eqList eq_a xs ys
eqList eq_a _       _       = False
\end{spec}
\vspace*{-1\abovedisplayskip-1\belowdisplayskip}
\begin{spec}
eqList (eqPair eqInt eqChar) :: Eq [(Int, Char)]
\end{spec}
\end{overprint}
\end{frame}

\begin{frame}{\alt<-2>{內定的dictionary叫做instance}{\alt<3>{使用method時生成constraint累積成context}{\alt<4>{自動組成新instance}{為了指定內定而建立新wrapper型別}}} \hfill\mdseries\citep{wadler-ad-hoc}}
\mathindent=0pt
\abovedisplayskip=0pt
\belowdisplayskip=0pt
\onslide<2->{%
\begin{spec}
class Eq a where
  (==) :: a -> a -> Bool
\end{spec}
}

\vspace*{-1\baselineskip}
\begin{minipage}{.5\textwidth}
\begin{spec}
instance Eq Int where
  (remember0 ((==))) = eqInt
\end{spec}
\end{minipage}%
\begin{minipage}{.5\textwidth}
\begin{spec}
instance Eq Char where
  (remember1 ((==))) = eqChar
\end{spec}
\end{minipage}

\begin{overprint}
\onslide<3>
\begin{spec}
elem :: alert ((Eq a) =>)  a  ->  [a]     ->  Bool
elem                       x      []      =   False
elem                       x      (y:ys)  =   x (alert (==)) y || alert elem x ys
lookup :: alert ((Eq a) =>)  a  ->  [(a, b)]     ->  Maybe b
lookup                       x      []           =   Nothing
lookup                       x      ((y,b):ybs)  =   if x (alert (==)) y then Just b
                                                     else alert lookup x ybs
\end{spec}
\onslide<4>
\begin{spec}
instance alert ((Eq a, Eq b) =>) Eq (a,b) where
  (a1,b1) == (a2,b2) = a1 (alert (==)) a2 && b1 (alert (==)) b2

instance alert ((Eq a) =>) Eq [a] where
  []      ==  []      =  True
  (x:xs)  ==  (y:ys)  =  x (alert (==)) y && xs (alert (==)) ys
  _       ==  _       =  False
\end{spec}
\onslide<5>
\begin{spec}
newtype Set a = MkSet [a]

instance (Eq a) => Eq (Set a) where
  MkSet xs (remember2 (==)) MkSet ys =  all (\x -> elem x ys) xs &&
                                        all (\y -> elem y xs) ys
\end{spec}
\end{overprint}
\begin{tikzpicture}[remember picture, overlay]
\onslide<2>{
    \path (it0.south) ++(-3em,-2ex) node
        [rectangle callout, callout absolute pointer={(it0.south)}, anchor=north west, alerted,
         callout pointer xshift=-2em]
        {|(==) :: Int -> Int -> Bool|};
    \path (it1.south) ++(-3em,-2ex) node
        [rectangle callout, callout absolute pointer={(it1.south)}, anchor=north west, alerted,
         callout pointer xshift=-1em]
        {|(==) :: Char -> Char -> Bool|};
}
\onslide<5>{
    \path (it2.south) ++(-4em,-4ex) node
        [rectangle callout, callout absolute pointer={(it2.south)}, anchor=north west, alerted,
         callout pointer xshift=-2em]
        {|(==) :: Set a -> Set a -> Bool|};
}
\end{tikzpicture}
\vspace*{-1\baselineskip}
\end{frame}

\begin{frame}{\alt<1>{Default method implementation}{\alt<2>{Class contexts (superclasses)}{There's no type class like |Show| type class}}}
\mathindent=0pt
\begin{spec}
class Eq a where
  (==)  :: a -> a -> Bool
  (/=)  :: a -> a -> Bool

  x  /=  y  =  not  (x  ==  y)
  x  ==  y  =  not  (x  /=  y)
\end{spec}
\pause
\vspace*{-1\abovedisplayskip-1\belowdisplayskip}
\begin{spec}
class (Eq a) => Ord a where
  (<), (<=), (>), (>=) :: a -> a -> Bool

  x  <   y  =  not  (x == y)  &&       (x <= y)
  x  >   y  =  not  (x == y)  &&  not  (x <= y)
  x  >=  y  =       (x == y)  ||  not  (x <= y)
  ...
\end{spec}
\pause
\vspace*{-1\abovedisplayskip-1\belowdisplayskip}
\begin{spec}
class Show a where show :: a -> String ...
\end{spec}
\end{frame}

\begin{frame}{|Monad|是一個type class (constructor class) \hfill\mdseries\citep{jones-functional,jones-system-jfp}}
\mathindent=0pt
\begin{spec}
class Monad m where
  return  :: a -> m a
  (>>=)   :: m a -> (a -> m b) -> m b

newtype State s a = remember MkState {remember0 runState :: s -> (a, s)}

instance Monad (State s) where
  remember1 return a      = MkState (\s ->  (a, s))
  m (remember2 (>>=)) k   = MkState (\s ->  let (a, s') = runState m s
                                            in runState (k a) s')
\end{spec}
\begin{tikzpicture}[remember picture, overlay]
\onslide<2->{
    \path (it.north east) ++(5em,.5ex) node
        [rectangle callout, callout absolute pointer={(it.north east)}, anchor=south west, alerted]
        {|MkState :: (s -> (a, s)) -> State s a|};
    \path (it0.south east) ++(1em,-.5ex) node
        [rectangle callout, callout absolute pointer={(it0.south)}, anchor=north west, alerted]
        {|runState :: State s a -> (s -> (a, s))|};
}
\onslide<3->{
    \path (it2.south) ++(-3em,-5ex) node
        [rectangle callout, callout absolute pointer={(it2.south)}, anchor=north west, alerted,
         callout pointer xshift=-4em]
        {|(>>=) :: State s a -> (a -> State s b) -> State s b|};
    \path (it1.south west) ++(-3em,-4ex) node
        [rectangle callout, callout absolute pointer={(it1.south west)}, anchor=north west, alerted,
         callout pointer xshift=-3em]
        {|return :: a -> State s a|};
}
\end{tikzpicture}

\vfill\hfill
至於|Maybe|與|[]|的|Monad| instances則已有內建
\end{frame}

\begin{frame}{輕鬆實作superclasses}
\mathindent=0pt
\begin{spec}
class Functor m where
  fmap    :: (a -> b) -> m a -> m b

class (Functor m) => Applicative m where
  pure    :: a -> m a
  (<*>)   :: m (a -> b) -> m a -> m b

class (Applicative m) => Monad m where
  return  :: a -> m a
  (>>=)   :: m a -> (a -> m b) -> m b

instance Functor (State s) where fmap = liftM

instance Applicative (State s) where pure = return; (<*>) = ap
\end{spec}
\end{frame}

\begin{frame}{來寫範例吧！}
\exercise{ArithMonad1}

\exercise{ArithMonad2}

\exercise{ArithMonad3}
\end{frame}

\section{Imperative programming}

\begin{frame}{I/O}
\exercise{ArithIO1}
「輸入」、「輸出」是什麼意思呢？

適合用什麼monad來表達呢？
\pause
\begin{spec}
data IO a  =  Return a
           |  Input (Int -> IO a)
           |  Output Int (IO a)
\end{spec}
對程式而言，外界是一個抽象的monad
\vskip\abovedisplayskip
\begin{quote}
    A value of type |IO a| is an ``action'' that, when performed, may do some
    input/output, before delivering a value of type~|a|.

    |type IO a = World -> (a, World)|

    \hfill\citep{peyton-jones-tackling}
\end{quote}
\end{frame}

\begin{frame}{I/O也是有意義的 \hfill\mdseries\citep{peyton-jones-tackling}}
\bigskip
\begin{overprint}
\onslide<-2>
\begin{verbatim}
int main() {
  return putchar(toupper(getchar()));
}
\end{verbatim}
\onslide<3->
\begin{proofrules}
\advance \leftskip -5mm
\advance \rightskip -5mm
\[ \justifies \{\mathbb{E}[|putChar c        |]\} \xrightarrow{\smash!|c|} \{\mathbb{E}[|return ()|]\} \using \text{PUTC} \]
\[ \justifies \{\mathbb{E}[|getChar          |]\} \xrightarrow{\smash?|c|} \{\mathbb{E}[|return c |]\} \using \text{GETC} \]
\[ \justifies \{\mathbb{E}[|return|\:N|>>=|M\,]\} \rightarrow              \{\mathbb{E}[M\;N     \,]\} \using \text{LUNIT} \]
\[ \llbracket M\,\rrbracket = \llbracket V\,\rrbracket \quad M \nequiv V
   \justifies \{\mathbb{E}[M                \,]\} \rightarrow              \{\mathbb{E}[V        \,]\} \using \text{FUN} \]
\end{proofrules}
\medskip
\end{overprint}
\alt<-2>{可譯為}{Semantics以labeled transition在外、denotation在內}
\[\begin{array}{>{{}}c<{{}}ll}
    & \onslide<4->{\{}
      \onslide<-3>{\mathllap{|main|\quad=\quad}}
      \inlinenote<2>{|getChar|}{south}{(0,1.2ex)}{|getChar :: IO Char|}
      {}|>>= \c ->|{}
      \inlinenote<2>{|putChar|}{north}{(0,-1.2ex)}{|putChar :: Char -> IO ()|}
      \;|(|{}
      \inlinenote<2>{|toUpper|}{south}{(0,1.2ex)}{|toUpper :: Char -> Char|}
      \;|c)|
      \onslide<2>{\mathrlap{\alert{\quad|::|\quad???}}}
      \onslide<4->\} \\
    \smash{\xrightarrow{?|'w'|}} & \{|return 'w' >>= \c -> putChar (toUpper c)|\} & \text{(GETC)}  \\
    \rightarrow                  & \{|(\c -> putChar (toUpper c)) 'w'         |\} & \text{(LUNIT)} \\
    \rightarrow                  & \{|putChar 'W'                             |\} & \text{(FUN)}   \\
    \smash{\xrightarrow{!|'W'|}} & \{|return ()                               |\} & \text{(PUTC)}
\end{array}\]
\onslide<4->{為何滿足preservation? progress?}
\end{frame}

\begin{frame}{Do notation}
\mathindent=0pt
\begin{overprint}
\onslide<1>
\begin{spec}
main =  getChar >>= \c ->
        putChar (toUpper c)
\end{spec}
\onslide<2>
\begin{spec}
main =  getChar >>= \c1 ->
        getChar >>= \c2 ->
        putChar (toUpper c1) >>= \() ->
        putChar (toLower c2)
\end{spec}
\onslide<3>
\begin{spec}
main =  getChar >>= \c1 ->
        getChar >>= \c2 ->
        putChar (toUpper c1) (remember (>>))
        putChar (toLower c2)
\end{spec}
\tikz[remember picture,overlay]
    \node at (it.east)
        [rectangle callout, callout relative pointer={(-1em,0)}, anchor=pointer, alerted]
        {\texths\hscodestyle
\begin{spec}
(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n
\end{spec}
};
\onslide<4>
\begin{spec}
main =  getChar >>= \c1 ->
        getChar >>
        putChar (toUpper c1) (remember (>>))
        putChar (toLower c1)
\end{spec}
\tikz[remember picture,overlay]
    \node at (it.east)
        [rectangle callout, callout relative pointer={(-1em,0)}, anchor=pointer, alerted]
        {\texths\hscodestyle
\begin{spec}
(>>) :: (Monad m) => m a -> m b -> m b
m >> n = m >>= \_ -> n
\end{spec}
};
\end{overprint}
\vspace*{-1\belowdisplayskip}
\begin{overprint}
\onslide<1>
\begin{spec}
main =  do  c <- getChar
            putChar (toUpper c)
\end{spec}
\onslide<2>
\begin{spec}
main =  do  c1 <- getChar
            c2 <- getChar
            () <- putChar (toUpper c1)
            putChar (toLower c2)
\end{spec}
\onslide<3>
\begin{spec}
main =  do  c1 <- getChar
            c2 <- getChar
            putChar (toUpper c1)
            putChar (toLower c2)
\end{spec}
\onslide<4>
\begin{spec}
main =  do  c1 <- getChar
            getChar
            putChar (toUpper c1)
            putChar (toLower c1)
\end{spec}
\end{overprint}
\hfill \onslide<2>{有哪些labeled transition sequences?}
\end{frame}

\begin{frame}[t]{Do notation用用看}
\begin{center}
\begin{tabular}{lll}
\toprule
把這個interpreter\dots & 用這個monad\dots & 在這裡寫成do notation: \\
\midrule
\exercise{ArithMonad1} &\remember{state int}{|State Int|} &$\rightarrow$ \exercise{ArithDo1}\\
\exercise{ArithMonad2} &                     |Maybe|      &$\rightarrow$ \exercise{ArithDo2}\\
\exercise{ArithMonad3} &                     |[]|         &$\rightarrow$ \exercise{ArithDo3}\\
\exercise{ArithIO1}    &                     |IO|         &$\rightarrow$ \exercise{ArithDo4}\\
\bottomrule
\end{tabular}
\end{center}
\pause
\tikz[remember picture, overlay]
    \node at (state int.south east)
        [rectangle callout, callout relative pointer={(-2em,5em)}, anchor=pointer, alerted]
        {\texths\hscodestyle
\begin{spec}
import Control.Monad.Trans.State

instance Monad (State s) where ...

runState  :: State s a -> (s -> (a, s))

state     :: (s -> (a, s)) -> State s a
\end{spec}
};
\end{frame}

\begin{frame}{Do notation表達了monad laws的imperative直覺}
\texths\hscodestyle
\begin{minipage}{.48\textwidth}
\begin{block}{Left identity}
\centering\(\begin{array}{rcl}
|return a >>= \x -> k x|
&=& |k a| \\[1ex]
\colorbox{white}{%
\begin{spec}
do  x <- return a
    k x
\end{spec}
}
&=& |k a|
\end{array}\)
\end{block}
\end{minipage}\hfill
\begin{minipage}{.48\textwidth}
\begin{block}{Right identity}
\centering\(\begin{array}{rcl}
|m >>= \x -> return x|
&=& |m| \\[1ex]
\colorbox{white}{%
\begin{spec}
do  x <- m
    return x
\end{spec}
}
&=& |m|
\end{array}\)
\end{block}
\end{minipage}

\vspace{.03\textwidth-\parskip}
\begin{block}{Associativity}
\centering\(\begin{array}{rcl}
|m >>= \a -> (k a >>= \b -> l b)|
&=& |(m >>= \a -> k a) >>= \b -> l b| \\[1ex]
\colorbox{white}{%
\begin{spec}
do  a <- m
    b <- k a
    l b
\end{spec}
}
&=&
\colorbox{white}{%
\begin{spec}
do  b <- do  a <- m
             k a
    l b
\end{spec}
}
\end{array}\)
\end{block}
\end{frame}

\begin{frame}{單一程式可以應用於各種monad}
\begin{spec}
traverse :: (Monad m) => (a -> m b) -> [a] -> m [b]  -- 又名|mapM|
traverse f []      = return []
traverse f (a:as)  = do  b   <- f a
                         bs  <- traverse f as
                         return (b:bs)
\end{spec}
有什麼用呢？
\begin{spec}
renumber  "hello"  = [0,1,2,3,4]

choices   [2,3]    = [[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]]

dec       [2,5,3]  = Just [1,4,2]
dec       [2,0,3]  = Nothing
\end{spec}
再多找一些用途！
\exercise{Traverse1}
\end{frame}

\begin{frame}{單一程式可以應用於各種monad}
\begin{spec}
data Tree = Leaf Int | Branch Tree Tree
  deriving (Eq, Show)

traverseTree :: (Monad m) => (Int -> m Int) -> Tree -> m Tree
traverseTree f (Leaf n)        = do  n' <- f n
                                     return (Leaf n')
traverseTree f (Branch t1 t2)  = do  t1' <- traverseTree f t1
                                     t2' <- traverseTree f t2
                                     return (Branch t1' t2')
\end{spec}
有什麼用呢？
\exercise{Traverse1}

很多資料結構只要提供|traverse|就是用途很廣的API了。
\end{frame}

\begin{frame}[t]{自己的迴圈自己寫}
\exercise{Loops1}
\begin{enumerate}
\item |forever action = action >> forever action| 型別為何？
\item 用|forever|寫一個一直讀一行（用|getLine|）然後馬上寫出（用|putStrLn|）的程式。
\item |iterateM_ f x = f x >>= iterateM_ f| 型別為何？
\item 用|iterateM_|寫一個一直讀數字然後顯示累計總和的程式。
\item |forever (getChar >>= putChar)|有哪些labeled transition sequences?
\item |iterateM_ (\c -> putChar c >> getChar) 'X'|有哪些labeled transition sequences?
\end{enumerate}
\end{frame}

\begin{frame}[t]{自己的迴圈自己寫}
\exercise{Loops2}
\begin{enumerate}
\item 定義|replicateM_ :: (Monad m) => Int -> m a -> m ()|
      使得|replicateM_ n action|的意思是把|action|重複|n|遍。有什麼用？
\item 定義|for :: (Monad m) => Int -> Int -> (Int -> m a) -> m ()|
      使得|for from to f|的意思是做從|f from|到|f to|的一系列動作。有什麼用？
\item 定義|while :: (Monad m) => m Bool -> m a -> m ()|
      使得|while cond action|的意思是重複做|action|直到|cond|的結果成為|False|為止。有什麼用？
\end{enumerate}
\end{frame}

\begin{frame}{兩種monad的定義可以互相轉換}
\exercise{Join1}
\begin{center}
\begin{tikzpicture}[>=stealth]
    \node (old) at (0,3) [anchor=west] {\texths\hscodestyle
\begin{spec}
return  :: a -> m a
fmap    :: (a -> b) -> m a -> m b
join    :: m (m a) -> m a
\end{spec}
    };
    \node (new) at (0,0) [anchor=west] {\texths\hscodestyle
\begin{spec}
return  :: a -> m a
(>>=)   :: m a -> (a -> m b) -> m b
\end{spec}
    };
    \draw [->, bend right] (old) to node [align=left,anchor=east] {用|fmap|和|join|\\定義|>>=|} (new);
    \draw [->, bend right] (new) to node [align=left,anchor=west] {用|return|和|>>=|\\定義|fmap|和|join|} (old);
\end{tikzpicture}
\end{center}
\end{frame}

\section{組合副作用}

\begin{frame}{邊state邊IO}
\mathindent=0pt
\begin{spec}
newtype StateIO s a = MkStateIO {runStateIO :: s -> IO (a, s)}
\end{spec}
\exercise{StateIO1}
\begin{itemize}
\item 完成|instance Monad (StateIO s)|
\item 新語法：|do|的中間是可以穿插|let|的
\end{itemize}
\exercise{StateIO2}
\begin{itemize}
\item 提供|change|這個operation以便state動作
\item 提供|lift|這個operation以便IO動作
\item 新語法：|(+ n)|就是|\s -> s + n|的意思
\end{itemize}
\end{frame}

\begin{frame}[t]{邊state邊exception}
\exercise{StateMaybe1} $=$ \exercise{StateMaybe2}
\begin{itemize}
\item |puzzle1|和|puzzle2|應該怎樣？
\item 定義|newtype M a|並完成|instance Monad M|
\item 提供|get|和|put|這兩個operation以便state動作
\item 提供|divide|這個operation以便exception動作
\item 找兩組不同的解法！
\end{itemize}
\end{frame}

\begin{frame}[t]{邊state邊nondeterminism}
\exercise{StateNondet1} $=$ \exercise{StateNondet2}
\begin{itemize}
\item |puzzle1|和|puzzle2|應該怎樣？
\item 定義|newtype M a|並完成|instance Monad M|
\item 提供|get|和|put|這兩個operation以便state動作
\item 提供|amb|這個operation以便nondeterminism動作
\item 找兩組不同的解法！
\end{itemize}
\exercise{Crypta3}
邁向logic programming \citep{fischer-purely-jfp}
\end{frame}

\begin{frame}{Parsing \hfill\mdseries\citep{hutton-monadic-jfp}}
|type Parser a = String -> [(a, String)]|\hfill
\smash[b]{\usebeamercolor[fg]{block body}\fboxsep=1.5ex\colorbox{bg}{\begin{tabular}[t]{@@{}l@@{}}
    \makebox[0pt][r]{``}A parser for things\\
    is a function from strings\\
    to lists of pairs\\
    of things and strings.''
\end{tabular}}}

|parse (many1 number) "12345"|

\bigskip
\exercise{Parsing1}
把|tree|這個|Tree|的parser寫完

\exercise{Parsing2}
寫一個parser combinator把「某種項目的parser」以及「某種隔間的parser」組合成為「一串項目的parser」，叫做|sepby1|

\exercise{Parsing3}
把|expr|這個parser加上減法的syntactic sugar
\end{frame}

\begin{frame}{Probability \hfill\mdseries\citep{ramsey-stochastic}}
|type Prob a = [(a, Float)]|

\bigskip
\exercise{Prob1}
定義|die|擲骰子

\exercise{Prob2}
用|M|（也就是|Data.Map|模組）裡的|M.toList|以及|M.fromListWith|定義|coalesce|

\exercise{Prob3}
在|countL|或|countR|內部呼叫|coalesce|，使得|coalesce (countL 100 0.5)|或|coalesce (countR 100 0.5)|變得很快

\exercise{Prob4}
不僅用|coalesce|，也得用|let|或|where|，使得|coalesce (countL 100 0.5)|和|coalesce (countR 100 0.5)|\textbf{都}很快
\end{frame}

\begin{frame}{Conditional probability \hfill\mdseries\citep{shan-exact}}
\begin{equation*}
    \mathbb{P}(A, B)
    \quad=\quad
    \begin{array}[t]{@@{}l@@{~}l@@{}}
        |do| & a \leftarrow \mathbb{P}(A) \\
             & b \leftarrow \mathbb{P}(B \mid A=a) \\
             & |return (a,b)|
    \end{array}
    \quad=\quad
    \begin{array}[t]{@@{}l@@{~}l@@{}}
        |do| & b \leftarrow \mathbb{P}(B) \\
             & a \leftarrow \mathbb{P}(A \mid B=b) \\
             & |return (a,b)|
    \end{array}
\end{equation*}

\bigskip
\exercise{Prob5}
丟一枚一元銅板、一枚五元銅板。
\begin{enumerate}
\item 看到頭像的機率為何？\\若看到頭像，則一元銅板是頭像的機率為何？
\item 沒有頭像的機率為何？\\若沒有頭像，則一元銅板是頭像的機率為何？
\end{enumerate}
\end{frame}

\begin{frame}{有無窮多種monad}
\begin{center}
\begin{tikzpicture}[>=stealth, x=4pc, y=2.75pc]
    \begin{scope}[every node/.style={anchor=base}]
        \node at ( 0, 0) (a) {|a|};
        \node at ( 1, 1) {|Maybe a|};
        \node at (-1, 1) {|[a]|};
        \node at ( 1,-1) {|s -> (a, s)|};
        \node at (-1,-1) {|IO a|};
        \onslide<2->{
        \node at ( 2, 0) {|s -> Maybe (a, s)|};
        \node at (-2, 0) {|[IO a]|};
        \node at ( 0, 2) {|[Maybe a]|};
        \node at ( 0,-2) {|s -> IO (a, s)|};
        \node at ( 2, 2) {|s -> (Maybe a, s)|};
        \node at ( 2,-2) {|s -> IO (Maybe a, s)|};
        \node at (-2, 2) {|s -> [(a, s)]|};
        \node at (-2,-2) {|s -> (IO a, s)|};
        }
    \end{scope}
    \alt<1>{\foreach \angle in {45,135,225,315}}{\foreach \angle in {15,45,...,345}}
        \draw [->] (a) +(\angle:.8pc) -- ++(\angle:2.4pc);
\end{tikzpicture}
\end{center}
\hfill\onslide<2->{哪兩個不行？}
\end{frame}

\begin{frame}
Monad transformers \citep{liang-interpreter}
\begin{itemize}
\item 把任一個monad「|m|」加一層功能，變成另一個monad「|t m|」
\item 例如|t = StateT Int, MaybeT, ...|\pause{| :: (Type -> Type) -> (Type -> Type)|}
\item 不一定commutative
\end{itemize}
Monads
\begin{itemize}
\item 把任一個type「|a|」加上副作用，變成\\「產生|a|結果的computation/action」的type「|m a|」
\item 例如|m = State Int, Maybe, [], IO, ... :: Type -> Type|
\item 其他type constructors例如|(,), (->) :: Type -> Type -> Type|
\end{itemize}
Types
\begin{itemize}
\item 有value進駐(inhabit)的
\item 例如|Int, Bool, Char, Int->Int->Bool, ... :: Type|
\end{itemize}
\end{frame}

\begin{frame}{Composing monad transformers}
\mathindent=0pt
\savecolumns
\begin{spec}
StateT :: Type -> (Type -> Type) -> (Type -> Type)

StateT s m a         = s -> m (a, s)

StateT S IO s        = s -> IO (a, s)

StateT s [] a        = s -> [(a, s)]

StateT s Identity a  = s -> (a, s)

Identity a           = a
\end{spec}
\vspace*{-1\belowdisplayskip-1\baselineskip}
\begin{overprint}
\onslide<2>
\restorecolumns
\begin{spec}
MaybeT :: (Type -> Type) -> (Type -> Type)

MaybeT m a           = m (Maybe a)

StateT s (MaybeT Identity) a  = ???

MaybeT (StateT s Identity) a  = ???
\end{spec}
\onslide<3>
\begin{spec}
newtype StateT s  m a = MkStateT  {runStateT  :: s -> m (a, s)}

newtype MaybeT    m a = MkMaybeT  {runMaybeT  :: m (Maybe a)}

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a
\end{spec}
\end{overprint}
\end{frame}

\begin{frame}[t]{Monad transformers用用看}
\bigskip
\begin{columns}[b]
\begin{column}{.4\textwidth}
\exercise{StateIO3}
\begin{itemize}
\item 使用共用的|lift|
\item \makebox[0pt][l]{使用共用的|modify|和|get|來定義|change|}
\end{itemize}
\exercise{StateMaybe3}
\begin{itemize}
\item 使用共用的|empty|或|lift|\CJKecglue\mbox{來定義|divide|}
\end{itemize}
\exercise{StateMaybe4}
\begin{itemize}
\item 使用共用的|lift|
\item 使用共用的|empty|\CJKecglue\mbox{來定義|divide|}
\end{itemize}
\end{column}
\begin{column}{.55\textwidth}
\exercise{StateNondet3}
\begin{itemize}
\item 使用共用的|empty|（或|lift|）以及|<||>|\CJKecglue\mbox{來定義|amb|}
\end{itemize}
\exercise{StateNondet4}
\begin{itemize}
\item 使用共用的|lift|
\item 使用共用的|empty|以及|<||>|\CJKecglue\mbox{來定義|amb|}
\end{itemize}
\end{column}
\end{columns}
\end{frame}

\section{自動微分}

\begin{frame}{機器學習}
學習 \texttt{Linear.hs} →

學習一個神經元 \texttt{Perceptron.hs} →

學習神經網路 \texttt{Network.hs}

\bigskip
為了自動調整參數們，所以需要把loss相對於參數們微分
\end{frame}

\begin{frame}[allowframebreaks=1]{自動微分 \hfill\mdseries\citep{krawiec-provably}}
\begin{enumerate}
\item 把loss表達為|Expr|，其中每個參數是一個|Var| \\
      \exercise{Diff1} 把|eval|寫完，增加|Pow|與|Exp|的cases
\item 用dual number進行forward automatic differentiation \\
      （為每個參數都得重跑一遍） \\
      用|instance Arbitrary Expr|產生隨機|Expr|用來測試 \citep{claessen-quickcheck} \\
      用|traverse|產生隨機|Params|用來起始學習過程 \\
      {\footnotesize|randomParams perceptronLoss >>= iterateM_ (optimize 999 perceptronLoss)|} \\
      {\footnotesize|randomParams networkLoss >>= iterateM_ (optimize 999 networkLoss)|} \\
      \exercise{Diff2} 把|eval2|寫完，增加|Pow|與|Exp|的cases
\item 增加慣性(|Inertia|)加速學習 \\
      \exercise{Diff3} 把|randomParams, stepParams, optimize|裡處裡慣性的部份寫完
\item 一次對所有參數微分：|(Float, Delta) = (Float, M.Map Name Float)| \\
      \exercise{Diff4} 用|dScale|把|eval3|寫完，增加|Pow|與|Exp|的cases
\item 把|Delta|用一個有|DLet|的語法樹表示\\
      用state monad以preserve sharing!\\
      \exercise{Diff5} 用|deltaLet|和|DScale|把|eval4|寫完，增加|Pow|與|Exp|的cases
\end{enumerate}
\end{frame}

\begin{frame}[allowframebreaks=1]{References}
\renewcommand\bibsection{}
\renewcommand\bibfont{\hscodestyle\footnotesize}
\bibliography{ccshan}
\end{frame}

\end{document}
