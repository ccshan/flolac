all: ln.pdf main.pdf Tree-1.hs Arith-1.hs Arith-2.hs TreeState-1.hs TreeState-2.hs UnionFind-1.hs ArithState-1.hs ArithState-2.hs TreeMaybe-1.hs ArithMaybe-1.hs TreeNondet-1.hs Crypta-1.hs Crypta-2.hs ArithNondet-1.hs ArithMonad-1.hs ArithMonad-2.hs ArithMonad-3.hs ArithIO-1.hs ArithDo-1.hs ArithDo-2.hs ArithDo-3.hs ArithDo-4.hs Traverse-1.hs

main.pdf: preamble.tex StateIO.tex StateMaybe.tex StateNondet.tex Diff.tex

ArithIO-1.hs: ArithIO.hs
	runhaskell -cpp -DSTEP=1 -DSOLUTION $< | grep ^4$
	cpp -traditional-cpp -P -DSTEP=1 $< -o - | perl -00 -pe '' > $@

%-1.hs: %.hs
	runhaskell -cpp -DSTEP=1 -DSOLUTION $< | grep ^True$
	cpp -traditional-cpp -P -DSTEP=1 $< -o - | perl -00 -pe '' > $@

%-2.hs: %.hs
	runhaskell -cpp -DSTEP=2 -DSOLUTION $< | grep ^True$
	cpp -traditional-cpp -P -DSTEP=2 $< -o - | perl -00 -pe '' > $@

%-3.hs: %.hs
	runhaskell -cpp -DSTEP=3 -DSOLUTION $< | grep ^True$
	cpp -traditional-cpp -P -DSTEP=3 $< -o - | perl -00 -pe '' > $@

%-4.hs: %.hs
	runhaskell -cpp -DSTEP=3 -DSOLUTION $< | grep ^True$
	cpp -traditional-cpp -P -DSTEP=3 $< -o - | perl -00 -pe '' > $@

%.tex: %.lhs preamble.lhs
	runhaskell $<
	lhs2TeX --poly $< > $@

%.pdf: %.tex
	texi2dvi --pdf --batch $<
