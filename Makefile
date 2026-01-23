all: util.import.scm json-query.import.scm jq

%.import.scm: %.scm
	csc -s -J $^

jq: jq.scm json-query.scm util.scm util.import.scm json-query.import.scm
	compile_jq.sh $^

clean:
	rm -f *.so *.import.scm *.link *.o jq
	rm -rf build

deps:
	sudo chicken-install srfi-1 srfi-13 srfi-34 srfi-180 srfi-193 vector-lib
