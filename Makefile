.PHONY: all clean deploy

SOURCE_DIR=src
RESOURCE_DIR=docs
EXERCISES=${RESOURCE_DIR}/exercises.html
JAVASCRIPT_DIR=${RESOURCE_DIR}/js/elm
MODULI=ProjectArithmeticExpressions
MINIFIED_TARGETS=$(MODULI:%=${JAVASCRIPT_DIR}/%.min.js)

all: ${MINIFIED_TARGETS} ${EXERCISES}
	@echo "finished"

${EXERCISES}: $(shell find exercises -type f)
	find exercises -name '*.md' | sort | xargs cat | sed 's/^#[^#].*$$/\n&/g' | pandoc -s -f markdown -t html --metadata-file exercises/meta.yml -o $@

${JAVASCRIPT_DIR}/%.min.js: ${JAVASCRIPT_DIR}/%.js $(shell find src -type f)
	uglifyjs $< --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | uglifyjs --mangle --output $@
	
${JAVASCRIPT_DIR}/%.js: ${SOURCE_DIR}/%.elm
	elm make $< --optimize --output $@

clean:
	rm -rf ${JAVASCRIPT_DIR}
	rm ${EXERCISES}
