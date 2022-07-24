export PDFLATEX = xelatex

EXERCISES = \
	Tree-1.hs \
	Arith-1.hs Arith-2.hs \
	TreeState-1.hs TreeState-2.hs \
	UnionFind-1.hs \
	ArithState-1.hs ArithState-2.hs \
	TreeMaybe-1.hs \
	ArithMaybe-1.hs \
	TreeNondet-1.hs \
	Crypta-1.hs Crypta-2.hs \
	ArithNondet-1.hs \
	Laws-1.hs \
	ArithMonad-1.hs ArithMonad-2.hs ArithMonad-3.hs \
	ArithIO-1.hs \
	ArithDo-1.hs ArithDo-2.hs ArithDo-3.hs ArithDo-4.hs \
	Traverse-1.hs \
	Loops-1.hs \
	Join-1.hs \
	StateIO-1.hs StateIO-2.hs StateIO-3.hs \
	StateMaybe-1.hs StateMaybe-2.hs StateMaybe-3.hs StateMaybe-4.hs \
	StateNondet-1.hs StateNondet-2.hs StateNondet-3.hs StateNondet-4.hs \
	Diff-1.hs Diff-2.hs Diff-3.hs Diff-4.hs Diff-5.hs

DIST = $(EXERCISES:%=dist/%)

SOLUTIONS = $(EXERCISES:%=solutions/%)

all: ln.pdf main.pdf $(SOLUTIONS) $(DIST)

push: $(DIST)
	rclone sync --progress dist google:FLOLAC/2022/習題/monad

clean:
	rm -rf comment.cut preamble.tex $(foreach f,ln main,$(foreach ext,aux bbl blg log nav out ptb snm toc tex pdf,$(f).$(ext))) $(SOLUTIONS) $(DIST)

# Solution testing with %.in and %.out

solutions/%-1.hs: %.hs %.in %.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ < $(filter %.in,$^) | diff - $(filter %.out,$^)

solutions/%-2.hs: %.hs %.in %.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ < $(filter %.in,$^) | diff - $(filter %.out,$^)

solutions/%-3.hs: %.hs %.in %.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ < $(filter %.in,$^) | diff - $(filter %.out,$^)

# Solution testing with %-STEP.out

solutions/%-1.hs: %.hs %-1.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

solutions/%-2.hs: %.hs %-2.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

solutions/%-3.hs: %.hs %-3.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

solutions/%-4.hs: %.hs %-4.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

# Solution testing with just QuickCheck

solutions/%-1.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%-2.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%-3.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%-4.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%-5.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

# Solution masking

dist/%-1.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%-2.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%-3.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%-4.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%-5.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*-,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

# lhs2TeX

main.pdf: preamble.tex

%.tex: %.lhs preamble.lhs
	runhaskell $<
	lhs2TeX --poly $< > $@

%.pdf: %.tex
	texi2dvi --pdf --batch $<
