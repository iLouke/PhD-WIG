# Makefile
.PHONY: all run clean

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
	rm -rf output/
	@echo "Project cleaned (build + output)."