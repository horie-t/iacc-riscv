CC=riscv64-unknown-elf-gcc

TARGET=stst

$(TARGET): stst.s runtime.c
	$(CC) -o $@ $^

run: $(TARGET)
	spike pk $<

.PHONY: clean
clean:
	rm -rf $(TARGET) stst.s stst.out *~
