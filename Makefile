.PHONY: check lint develop format clean

BUILD_DIR=build
SRC_DIRS=src include test

all: $(BUILD_DIR)

$(BUILD_DIR):
	@mkdir $(BUILD_DIR)
	@cd $(BUILD_DIR) && cmake -DCMAKE_BUILD_TYPE=Debug .. && make

check: $(BUILD_DIR)
	@cd $(BUILD_DIR) && ./test/hans-test

lint:
	@cppcheck --quiet --enable=warning,performance -I include $(SRC_DIRS)

develop: $(BUILD_DIR)
	@while inotifywait -e modify -r ./src ./include ./test ; do \
		pushd $(BUILD_DIR) && make ; popd ; \
	done;

format:
	@find $(SRC_DIRS) \( -name '*.h' -or \
					-name '*.hpp' -or \
					-name '*.cpp' \) -print0 | \
		xargs -0 "clang-format" -i

clean:
	@rm -rf $(BUILD_DIR)
