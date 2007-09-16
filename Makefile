.PHONY: all bootstrap clean redo
TRANSLATOR=guile translator.scm
all: smile.scm new-translator.scm interpreter.scm shell.scm

smile.scm: smile-use-modules.smile stdlib.smile lexer.smile parser.smile load-smile.smile dollar.smile
	rm -f $@
	for i in $^; do cat $$i  | grep -v '^load' | $(TRANSLATOR) >> $@;  done

new-translator.scm: translator.smile smile.scm
	echo -e '#!/usr/bin/guile -s' > $@
	echo '!#' >> $@
	cat $< | $(TRANSLATOR) >> $@
	chmod +x $@

standalone-translator.scm: translator.smile smile.scm
	echo -e '#!/usr/bin/guile -s' > $@
	echo '!#' >> $@
	cat smile.scm >> $@
	cat $< | grep -v '^load' | $(TRANSLATOR) >> $@
	chmod +x $@

interpreter.scm: interpreter.smile smile.scm
	echo -e '#!/usr/bin/guile -s' > $@
	echo '!#' >> $@
	cat $< | $(TRANSLATOR) >> $@
	chmod +x $@

shell.scm: shell.smile smile.scm
	echo -e '#!/usr/bin/guile -s' > $@
	echo '!#' >> $@
	cat $< | $(TRANSLATOR) >> $@
	chmod +x $@

bootstrap: standalone-translator.scm
	make clean
	cp $< translator.scm
	chmod +x translator.scm
	make all

clean:
	rm -f smile.scm new-translator.scm interpreter.scm shell.scm

redo: clean all
