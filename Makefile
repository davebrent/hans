.PHONY: lint docs format check

BUILD_DIR=build
SRC_DIRS=lib include modules test apps

all: $(BUILD_DIR)

$(BUILD_DIR):
	@mkdir $(BUILD_DIR)
	@cd $(BUILD_DIR) && cmake \
		-DCMAKE_BUILD_TYPE=Debug \
		-DCMAKE_INSTALL_PREFIX:PATH=. \
		.. && make install

check: $(BUILD_DIR)
	@cd $(BUILD_DIR) && ./test/unit/hans-unittest
	@guile -s test/scheme/patterns.scm
	@guile -s test/scheme/sequencer.scm
	@guile -s test/scheme/rhythms.scm

lint:
	@cppcheck --enable=all --quiet -I include/ $(SRC_DIRS)

docs:
	@doxygen Doxyfile

format:
	@find $(SRC_DIRS) \( -name '*.h' -or \
					-name '*.hpp' -or \
					-name '*.cpp' \) -print0 | \
		xargs -0 "clang-format" -i

clean:
	@rm -rf $(BUILD_DIR)
