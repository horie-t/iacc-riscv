CC=riscv64-unknown-elf-gcc

TARGET=stst

$(TARGET): stst.s runtime.c
	$(CC) -Wno-int-to-pointer-cast -T link.ld -o $@ $^

run: $(TARGET)
	spike pk $<

.PHONY: clean
clean:
	rm -rf $(TARGET) stst.s stst.out *~
