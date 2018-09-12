# An Incremental Approach to Compiler Construction for RISC-V

『[An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) (少しずつ進めるコンパイラ作成)』のRISC-V向けにやってみるリポジトリです。

この論文は、SchemeでSchemeのx86向けのコンパイラを、24ステップに分けて作成していくものです。

本リポジトリでは、これをRISC-V向けのコンパイラとして作成していきます。

各ステップの詳細は[Wikiページ](https://github.com/horie-t/iacc-riscv/wiki)に記載します。

## 開発環境の構築

Ubuntu 18.04を対象に記述しています。

### RISC-V Toolsのインストール

依存パッケージのインストール

```
sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev libusb-1.0-0-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config libexpat-dev
```

[RISC-V Toolsのリポジトリ](https://github.com/riscv/riscv-tools)から、ソース・コードをcloneし、インストールする。

```
git clone --recursive https://github.com/riscv/riscv-tools.git
cd riscv-tools
export RISCV=/usr/local/share/riscv   # /usr/local/share/riscvにインストールする場合
./build.sh
./build-rv32ima.sh
```

## Spikeの実行

Spikeエミュレータを使用すると、printf等が使えるので、Spikeを使用します。

「Hello, world!」プログラムのコンパイル

```
echo -e '#include <stdio.h>\n int main(void) { printf("Hello world!\\n"); return 0; }' > hello.c
riscv64-unknown-elf-gcc -o hello hello.c
```

実行方法

```
spike pk hello
```

## x86での実施例

https://github.com/namin/inc

で公開されています。
