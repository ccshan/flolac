all: ln.pdf main.pdf

main.pdf: preamble.tex Tree.tex Arith.tex TreeState.tex UnionFind.tex ArithState.tex TreeMaybe.tex ArithMaybe.tex TreeNondet.tex SendMoreMoney.tex ArithNondet.tex ArithMonad.tex ArithIO.tex ArithDo.tex Traverse.tex StateIO.tex StateMaybe.tex StateNondet.tex Diff.tex

%.tex: %.lhs preamble.lhs
	runhaskell $<
	lhs2TeX --poly $< > $@

%.pdf: %.tex
	texi2dvi --pdf --batch $<
