all: ln.pdf main.pdf Tree-1.hs Arith-1.hs Arith-2.hs

main.pdf: preamble.tex TreeState.tex UnionFind.tex ArithState.tex TreeMaybe.tex ArithMaybe.tex TreeNondet.tex SendMoreMoney.tex ArithNondet.tex ArithMonad.tex ArithIO.tex ArithDo.tex Traverse.tex StateIO.tex StateMaybe.tex StateNondet.tex Diff.tex

%-1.hs: %.hs
	runhaskell -cpp -DSTEP=1 -DSOLUTION $<
	cpp -traditional-cpp -P -DSTEP=1 $< -o - | perl -00 -pe '' > $@

%-2.hs: %.hs
	runhaskell -cpp -DSTEP=2 -DSOLUTION $<
	cpp -traditional-cpp -P -DSTEP=2 $< -o - | perl -00 -pe '' > $@

%.tex: %.lhs preamble.lhs
	runhaskell $<
	lhs2TeX --poly $< > $@

%.pdf: %.tex
	texi2dvi --pdf --batch $<
