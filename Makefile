.PHONY: all bootstrap clean redo
TRANSLATOR=guile translator.scm
all: smile.scm new-translator.scm interpreter.scm shell.scm

smile.scm: stdlib.smile dollar.smile smile-use-modules.smile lexer.smile parser.smile
	rm -f $@
	for i in $^; do cat $$i  | grep -v '^load' | $(TRANSLATOR) >> $@;  done
	echo "(set-current-module ( resolve-module '(guile)))"

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
	cp translator.scm translator_backup.scm
	cp $< translator.scm
	chmod +x translator.scm
	make all

clean:
	rm -f smile.scm new-translator.scm interpreter.scm shell.scm standalone-translator.scm

redo: clean all
