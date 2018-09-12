# An Incremental Approach to Compiler Construction for RISC-V

『[An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf) (少しずつ進めるコンパイラ作成)』のRISC-V向けにやってみるリポジトリです。

この論文は、SchemeでSchemeのx86向けのコンパイラを、24ステップに分けて作成していくものです。

本リポジトリでは、これをRISC-V向けのコンパイラとして作成していきます。

## 開発環境の構築

Ubuntu 18.04を対象に記述しています。

### RISC-V Toolsのインストール

依存パッケージのインストール

```
sudo apt-get install autoconf automake autotools-dev curl libmpc-dev libmpfr-dev libgmp-dev libusb-1.0-0-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev device-tree-compiler pkg-config libexpat-dev

[RISC-V Toolsのリポジトリ](https://github.com/riscv/riscv-tools)から、ソース・コードをcloneし、インストールする。
```

```
git clone --recursive https://github.com/riscv/riscv-tools.git
cd riscv-tools
export RISCV=/usr/local/share/riscv   # /usr/local/share/riscvにインストールする場合
./build.sh
./build-rv32ima.sh
```

### QEMUのインストール

依存パッケージのインストール

```
sudo apt-get install pkg-config libglib2.0-dev zlib1g-dev libpixman-1-dev
```

[RISC-V のQENUのリポジトリ](https://github.com/riscv/riscv-qemu)から、ソース・コードをcloneし、インストールする。(本家の方にもRISC-Vはあるのだが、SiFiveのハードはサポートされていない)


```
git clone --recursive https://github.com/riscv/riscv-qemu.git
cd riscv-qemu
./configure \
    --target-list=riscv64-softmmu,riscv32-softmmu,riscv64-linux-user,riscv32-linux-user
make -j4
sudo make install
```

## QEMUの実行

hello のELF実行ファイルがある場合。終了するには、「Ctrl-a」, 「x」をタイプします。

```
qemu-system-riscv32 -nographic -machine sifive_e300  -kernel hello
```

-nographicオプション起動時のターミナルコマンド

| Ctrl-a h | ヘルプを表示 |
| Ctrl-a x | エミュレータを終了 |
| Ctrl-a s | ディスクのデータをファイルに保存 |
| Ctrl-a t | タイムスタンプを表示 |
| Ctrl-a b | breadを送信 |
| Ctrl-a c | コンソールとモニタを切り替え |
| Ctrl-a a | Ctrl-aを送信 |
