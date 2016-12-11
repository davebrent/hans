.PHONY: check lint format clean

BUILD_DIR=build
SRC_DIRS=src include plugins test

all: $(BUILD_DIR)

$(BUILD_DIR):
	@mkdir $(BUILD_DIR)
	@cd $(BUILD_DIR) && cmake -DCMAKE_BUILD_TYPE=Release .. && make

check: $(BUILD_DIR)
	@find test/scm/*.scm -exec guile {} \;
	@cd $(BUILD_DIR) && ./test/unit/hans-unittest

lint:
	@cppcheck --quiet --enable=warning,performance -I include $(SRC_DIRS)

format:
	@find $(SRC_DIRS) \( -name '*.h' -or \
					-name '*.hpp' -or \
					-name '*.cpp' \) -print0 | \
		xargs -0 "clang-format" -i

clean:
	@rm -rf $(BUILD_DIR)
