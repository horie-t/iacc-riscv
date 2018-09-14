CC=riscv64-unknown-elf-gcc
SC=kawa
SCFLAGS=--warn-undefined-variable=no --warn-invoke-unknown-method=no

TARGET=stst

$(TARGET): stst.s runtime.c
	$(CC) -o $@ $^

stst.s: compiler.scm
	$(SC) $(SCFLAGS) --script $< > $@

run: $(TARGET)
	spike pk $<

.PHONY: clean
clean:
	rm -rf $(TARGET) stst.s *~
