Nanoha
===

# Overview

Nanoha, a USI shogi(japanese-chess) playing engine derived from Stockfish, a UCI chess playing engine.

「なのは」は、USIに対応した将棋プログラムです。
　強いチェスプログラムのStockfishをベースに作成しています。

# 動作環境
* ビルドには64bitのWindowsとVisual Studio2017が必要です。
* メモリは4GBもあれば十分です。
* CPUは速いほうがいいです。

# コンパイル方法
・Visual Studio Community 2017 で確認しています

  1. x64 Native tools Command Prompt for VS 2017を起動します。
  2. カレントディレクトリをソース展開したところにします。
  3. nmake -f Makefile.vs とすれば、コンパイルできます。

  ※Cygwin や MSYS2 でも make build とすれば、コンパイルできると思います。

# 使用条件および免責等
GPL V3に従います。

なのはを再配布するときは、GPL V3に従って行ってください。 
バグは内在していると思いますので、ご指摘いただければありがたいです。
 (なるべくやさしくお願いします)

# 謝辞
* ベースとなったStockfish開発者Marco Costalba, Joona Kiiski, Tord Romstadに感謝します。
* Apery開発者の平岡拓也さんに感謝します。
* gpsfish を参考にしました。TeamGPS各位に感謝します。
* Woodyさんのブログ記事もとても参考にしています。
* れさぴょんも参考にしています。開発者の池さんに感謝します。

