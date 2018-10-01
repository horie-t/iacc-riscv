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

## 開発作業

各ステップの作業については、[Wikiページ](https://github.com/horie-t/iacc-riscv/wiki)を参照してください。
