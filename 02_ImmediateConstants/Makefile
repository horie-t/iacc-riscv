CC=riscv64-unknown-elf-gcc
SC=kawa

TARGET=stst

$(TARGET): stst.s runtime.c
	$(CC) -o $@ $^

stst.s: compiler.scm
	$(SC) --script $< > $@

run: $(TARGET)
	spike pk $<

.PHONY: clean
clean:
	rm -rf $(TARGET) stst.s *~
