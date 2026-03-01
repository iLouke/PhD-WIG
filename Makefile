# Makefile
.PHONY: all run test clean

ifeq ($(OS),Windows_NT)
RM_OUTPUT_DIR = if exist output rmdir /S /Q output
else
RM_OUTPUT_DIR = rm -rf output/
endif

# Default action: Build and Run
all: run

# Run fpm
run:
	fpm run

# Test fpm
test:
	fpm test

# Custom Clean: Removes fpm build AND your output folder
clean:
	fpm clean
	$(RM_OUTPUT_DIR)
	@echo "Project cleaned (build + output)."