# An Incremental Approach to Compiler Construction for RISC-V

『[An Incremental Approach to Compiler Construction](https://github.com/namin/inc/blob/master/docs/paper.pdf?raw=true) (少しずつ進めるコンパイラ作成)』のRISC-V向けにやってみるリポジトリです。元のリポジトリは[こちら](https://github.com/namin/inc)です。

この論文は、SchemeでSchemeのx86向けのコンパイラを、24ステップに分けて作成していくものです。論文だけでは難しいので、[チュートリアル](https://github.com/namin/inc/blob/master/docs/tutorial.pdf?raw=true)があります。

本リポジトリでは、これをRISC-V向けのコンパイラとして作成していきます。

各ステップの詳細の解説は[Wikiページ](https://github.com/horie-t/iacc-riscv/wiki)に記載します。

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

### Scheme処理系のインストール

Schemeの処理系であれば、なんでもいいのですが、ここでは、[Kawa](http://www.gnu.org/software/kawa/index.html)を使う事にします。

依存パッケージのインストール

Kawaは、Java上で動くのでJavaをインストールしておきます。(JREでもいいはずですが、JDKを入れています)

```
sudo apt install openjdk-8-jdk
```

Kawaの[最新版](ftp://ftp.gnu.org/pub/gnu/kawa/kawa-latest.zip)をダウンロードして展開します。

```
cd /usr/local/share
sudo mkdir kawa
cd kawa
sudo wget ftp://ftp.gnu.org/pub/gnu/kawa/kawa-latest.zip
sudo unzip kawa-latest.zip    # 執筆時点では、kawa-3.0が最新版でした
```

~/.bashrc ファイルに以下を追記
```bash
export KAWA_HOME=/usr/local/share/kawa/kawa-3.0
export PATH=$KAWA_HOME/bin:$PATH
```

Emacsを使っている場合は、以下を設定しています。

```elisp
;;;; For Kawa
(setq scheme-program-name "/usr/bin/java -cp /usr/local/share/kawa/kawa-3.0/lib/kawa.jar kawa.repl --full-tailcalls --warn-undefined-variable=no --warn-invoke-unknown-method=no --no-inline --output-format readable-scheme -s")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)
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

### Spikeのデバッグ・モード実行

Spike実行時に、 -d オプションを付けるとデバッグ・モードで実行できる。

```bash
spike -d pk hello
```

run(rと省略可)を以下のようにすれば、指定した命令数だけ実行できる。

```
書式:
run [命令数]

例:
: run 1
```

regコマンドで、現在のレジスタの値が表示される
```
書式:
reg <CPUコア番号>

例:
: reg 0
zero: 0x0000000000000000  ra  : 0x0000000000000000  sp  : 0x0000000000000000  gp  : 0x0000000000000000  
tp  : 0x0000000000000000  t0  : 0x0000000000000000  t1  : 0x0000000000000000  t2  : 0x0000000000000000  
s0  : 0x0000000000000000  s1  : 0x0000000000000000  a0  : 0x0000000000000000  a1  : 0x0000000000000000  
a2  : 0x0000000000000000  a3  : 0x0000000000000000  a4  : 0x0000000000000000  a5  : 0x0000000000000000  
a6  : 0x0000000000000000  a7  : 0x0000000000000000  s2  : 0x0000000000000000  s3  : 0x0000000000000000  
s4  : 0x0000000000000000  s5  : 0x0000000000000000  s6  : 0x0000000000000000  s7  : 0x0000000000000000  
s8  : 0x0000000000000000  s9  : 0x0000000000000000  s10 : 0x0000000000000000  s11 : 0x0000000000000000  
t3  : 0x0000000000000000  t4  : 0x0000000000000000  t5  : 0x0000000000000000  t6  : 0x0000000000000000
```

現在のプログラム・カウンタ・レジスタの値を表示するには、以下の通り実行する。

```
書式:
pc <CPUコア番号>

例:
: pc 0
0x0000000000001000
```

指定したアドレスまで、実行を進めるには、以下の通り実行する。

```
書式:
until pc <CPUコア番号> <アドレス>

例:
: until pc 0 1004
```

コマンドの一覧を以下に記載します。[]は省略可。

| 書式 | 説明 |
|---|----|
| reg \<core\\> [reg]                | \<core\> CPUコアの [reg] レジスタ(省略時は全て)を表示します。 |
| fregs \<core\> \<reg\>              | \<core\> の \<reg\> 単精度レジスタを表示します。 |
| fregd \<core\> \<reg\>              | \<core\> の \<reg\> 倍精度レジスタを表示します。 |
| pc \<core\>                       | \<core\> の現在のプログラム・カウンタ(Program Counter. PC)を表示します。 |
| mem \<hex addr\>                  | \<hex addr\> の16進数アドレスの物理メモリの内容を表示します。 |
| str \<hex addr\>                  | \<hex addr\> から始まるNUL終端のC言語文字列を表示します。 |
| until reg \<core\> \<reg\> \<val\>    | \<core\> の \<reg\> が \<val\>の値になったらストップします。 |
| until pc \<core\> \<val\>           | \<core\> のPCが \<val\> になったらストップします。 |
| until mem \<addr\> \<val\>          | \<addr\>アドレスのメモリが \<val\> になったらストップします。 |
| while reg \<core\> \<reg\> \<val\>    | \<core\> の \<reg\> が \<val\> の間だけ実行します。 |
| while pc \<core\> \<val\>           | \<core\> のPCが \<val\> の間だけ実行します。 |
| while mem \<addr\> \<val\>          | \<addr\> のメモリが \<val\> の間だけ実行します。 |
| run [count]                     | うるさく実行再開します。(CTRL+C, もしくは [count] 数の命令実行完了まで) |
| r [count]                       | run の別名 |
| rs [count]                      | 静かに(Silent)実行再開します。(CTRL+C, もしくは [count] 数の命令実行完了まで) |
| quit                            | シミュレーションを終了します。 |
| q                               | quit の別名 |
| help                            | この表を表示 |
| h                               | help の別名 |

実行ファイルの逆アセンブルの結果と見比べながら、デバッグ実行します。

```
$ riscv64-unknown-elf-objdump - hello | less


hello:     ファイル形式 elf64-littleriscv


セクション .text の逆アセンブル:

00000000000100b0 <_start>:
   100b0:       00003197                auipc   gp,0x3
   100b4:       6c018193                addi    gp,gp,1728 # 13770 <__global_pointer$>
   100b8:       82818513                addi    a0,gp,-2008 # 12f98 <_edata>
   100bc:       8b018613                addi    a2,gp,-1872 # 13020 <_end>
   100c0:       8e09                    sub     a2,a2,a0
   100c2:       4581                    li      a1,0

(中略)

000000000001019a <main>:
   1019a:       1141                    addi    sp,sp,-16
   1019c:       e406                    sd      ra,8(sp)
   1019e:       e022                    sd      s0,0(sp)
   101a0:       0800                    addi    s0,sp,16
   101a2:       67c9                    lui     a5,0x12
   101a4:       fd878513                addi    a0,a5,-40 # 11fd8 <__errno+0xa>
   101a8:       20a000ef                jal     ra,103b2 <puts>
   101ac:       4781                    li      a5,0
   101ae:       853e                    mv      a0,a5
   101b0:       60a2                    ld      ra,8(sp)
   101b2:       6402                    ld      s0,0(sp)
   101b4:       0141                    addi    sp,sp,16
   101b6:       8082                    ret
```

## 開発作業

各ステップの作業については、[Wikiページ](https://github.com/horie-t/iacc-riscv/wiki)を参照してください。
