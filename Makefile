
.PHONY: lint docs format

lint:
	@cppcheck --quiet --enable=all -I lib include modules test

docs:
	@doxygen Doxyfile

format:
	@find lib include modules test apps \
		\( -name '*.h' -or -name '*.hpp' -or -name '*.cpp' \) -print0 | \
		xargs -0 "clang-format" -i

build/:
	@mkdir build/
	@cd build && cmake \
		-DCMAKE_BUILD_TYPE=Debug \
		-DCMAKE_INSTALL_PREFIX:PATH=. \
		.. && make install

clean:
	@rm -rf build

check: build/
	@guile -s test/scheme/patterns.scm
	@guile -s test/scheme/sequencer.scm
	@guile -s test/scheme/rhythms.scm
