export PDFLATEX = xelatex
export BIBTEX = bibtex -min-crossrefs=9999

EXERCISES = \
	Tree1.hs \
	Arith1.hs Arith2.hs \
	TreeState1.hs TreeState2.hs \
	UnionFind1.hs \
	ArithState1.hs ArithState2.hs \
	TreeMaybe1.hs \
	ArithMaybe1.hs \
	TreeNondet1.hs \
	ArithNondet1.hs \
	Crypta1.hs Crypta2.hs Crypta3.hs \
	Laws1.hs \
	ArithMonad1.hs ArithMonad2.hs ArithMonad3.hs \
	ArithIO1.hs \
	ArithDo1.hs ArithDo2.hs ArithDo3.hs ArithDo4.hs \
	Traverse1.hs \
	Loops1.hs Loops2.hs \
	Join1.hs \
	StateIO1.hs StateIO2.hs StateIO3.hs \
	StateMaybe1.hs StateMaybe2.hs StateMaybe3.hs StateMaybe4.hs \
	StateNondet1.hs StateNondet2.hs StateNondet3.hs StateNondet4.hs \
	Diff1.hs Diff2.hs Diff3.hs Diff4.hs Diff5.hs

DIST = $(EXERCISES:%=dist/%)

SOLUTIONS = $(EXERCISES:%=solutions/%)

all: ln.pdf main.pdf $(SOLUTIONS) $(DIST) Linear Perceptron Network

Linear Perceptron Network:: %: %.hs Graphics.hs Diff5.hs
	ghc -O2 --make -o $@ $<

push: $(DIST)
	rclone sync --progress dist google:FLOLAC/2022/習題/monad

clean:
	rm -rf comment.cut preamble.tex $(foreach f,ln main,$(foreach ext,aux bbl blg log nav out ptb snm toc tex pdf,$(f).$(ext))) $(SOLUTIONS) $(DIST) *.o *.hi *.dyn_o *.dyn_hi

# Solution testing with %.in and %.out

solutions/%1.hs: %.hs %.in %.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ < $(filter %.in,$^) | diff - $(filter %.out,$^)

solutions/%2.hs: %.hs %.in %.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ < $(filter %.in,$^) | diff - $(filter %.out,$^)

solutions/%3.hs: %.hs %.in %.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ < $(filter %.in,$^) | diff - $(filter %.out,$^)

# Solution testing with %-STEP.out

solutions/%1.hs: %.hs %1.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

solutions/%2.hs: %.hs %2.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

solutions/%3.hs: %.hs %3.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

solutions/%4.hs: %.hs %4.out
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | diff - $(filter %.out,$^)

# Solution testing with just QuickCheck

solutions/%1.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%2.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%3.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%4.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

solutions/%5.hs: %.hs
	mkdir -p solutions
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) -DSOLUTION $< -o - | perl -00 -pe '' > $@
	runhaskell $@ | grep ^True$

# Solution masking

dist/%1.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%2.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%3.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%4.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

dist/%5.hs: %.hs
	mkdir -p dist
	cpp -traditional-cpp -P -DSTEP=$(subst $*,,$(basename $(@F))) $< -o - | perl -00 -pe '' > $@

# lhs2TeX

main.pdf: preamble.tex

%.tex: %.lhs preamble.lhs
	runhaskell $<
	lhs2TeX --poly $< > $@

%.pdf: %.tex
	texi2dvi --pdf --batch $<
	#$(PDFLATEX) --batch $<
