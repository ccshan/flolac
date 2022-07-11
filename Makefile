all: ln.pdf main.pdf Tree-1.hs Arith-1.hs Arith-2.hs TreeState-1.hs TreeState-2.hs UnionFind-1.hs ArithState-1.hs ArithState-2.hs TreeMaybe-1.hs ArithMaybe-1.hs TreeNondet-1.hs Crypta-1.hs Crypta-2.hs ArithNondet-1.hs ArithMonad-1.hs ArithMonad-2.hs ArithMonad-3.hs ArithIO-1.hs ArithDo-1.hs ArithDo-2.hs ArithDo-3.hs ArithDo-4.hs Traverse-1.hs StateIO-1.hs StateIO-2.hs StateIO-3.hs StateMaybe-1.hs StateMaybe-2.hs StateMaybe-3.hs StateMaybe-4.hs StateNondet-1.hs StateNondet-2.hs StateNondet-3.hs StateNondet-4.hs

main.pdf: preamble.tex Diff.tex

ArithIO-1.hs: ArithIO.hs ArithIO.out
	runhaskell -cpp -DSTEP=1 -DSOLUTION $< < ArithIO.in | diff - ArithIO.out
	cpp -traditional-cpp -P -DSTEP=1 $< -o - | perl -00 -pe '' > $@

StateIO-1.hs: StateIO.hs StateIO.out
	runhaskell -cpp -DSTEP=1 -DSOLUTION $< < StateIO.in | diff - StateIO.out
	cpp -traditional-cpp -P -DSTEP=1 $< -o - | perl -00 -pe '' > $@

StateIO-2.hs: StateIO.hs StateIO.out
	runhaskell -cpp -DSTEP=2 -DSOLUTION $< < StateIO.in | diff - StateIO.out
	cpp -traditional-cpp -P -DSTEP=2 $< -o - | perl -00 -pe '' > $@

StateIO-3.hs: StateIO.hs StateIO.out
	runhaskell -cpp -DSTEP=3 -DSOLUTION $< < StateIO.in | diff - StateIO.out
	cpp -traditional-cpp -P -DSTEP=3 $< -o - | perl -00 -pe '' > $@

StateMaybe-1.hs: StateMaybe.hs StateMaybe-1.out
	runhaskell -cpp -DSTEP=1 -DSOLUTION $< | diff - StateMaybe-1.out
	cpp -traditional-cpp -P -DSTEP=1 $< -o - | perl -00 -pe '' > $@

StateMaybe-2.hs: StateMaybe.hs StateMaybe-2.out
	runhaskell -cpp -DSTEP=2 -DSOLUTION $< | diff - StateMaybe-2.out
	cpp -traditional-cpp -P -DSTEP=2 $< -o - | perl -00 -pe '' > $@

StateMaybe-3.hs: StateMaybe.hs StateMaybe-3.out
	runhaskell -cpp -DSTEP=3 -DSOLUTION $< | diff - StateMaybe-3.out
	cpp -traditional-cpp -P -DSTEP=3 $< -o - | perl -00 -pe '' > $@

StateMaybe-4.hs: StateMaybe.hs StateMaybe-4.out
	runhaskell -cpp -DSTEP=4 -DSOLUTION $< | diff - StateMaybe-4.out
	cpp -traditional-cpp -P -DSTEP=4 $< -o - | perl -00 -pe '' > $@

StateNondet-1.hs: StateNondet.hs StateNondet-1.out
	runhaskell -cpp -DSTEP=1 -DSOLUTION $< | diff - StateNondet-1.out
	cpp -traditional-cpp -P -DSTEP=1 $< -o - | perl -00 -pe '' > $@

StateNondet-2.hs: StateNondet.hs StateNondet-2.out
	runhaskell -cpp -DSTEP=2 -DSOLUTION $< | diff - StateNondet-2.out
	cpp -traditional-cpp -P -DSTEP=2 $< -o - | perl -00 -pe '' > $@

StateNondet-3.hs: StateNondet.hs StateNondet-3.out
	runhaskell -cpp -DSTEP=3 -DSOLUTION $< | diff - StateNondet-3.out
	cpp -traditional-cpp -P -DSTEP=3 $< -o - | perl -00 -pe '' > $@

StateNondet-4.hs: StateNondet.hs StateNondet-4.out
	runhaskell -cpp -DSTEP=4 -DSOLUTION $< | diff - StateNondet-4.out
	cpp -traditional-cpp -P -DSTEP=4 $< -o - | perl -00 -pe '' > $@

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
	runhaskell -cpp -DSTEP=4 -DSOLUTION $< | grep ^True$
	cpp -traditional-cpp -P -DSTEP=4 $< -o - | perl -00 -pe '' > $@

%.tex: %.lhs preamble.lhs
	runhaskell $<
	lhs2TeX --poly $< > $@

%.pdf: %.tex
	texi2dvi --pdf --batch $<
