# Nanoha, a USI shogi(japanese-chess) playing engine derived from Stockfish, a UCI chess playing engine.
# Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
# Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
# Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad
# Copyright (C) 2014-2017 Kazuyuki Kawabata
#
# Nanoha is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Nanoha is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

EXE = NanohaWCSC27a.exe
PGD = NanohaWCSC27a.pgd
PGOLOG = NanohaWCSC27a_prof.txt
#EVAL_VER=EVAL_NANO
#EVAL_OBJ=evaluate.obj
#EVAL_VER=EVAL_MINI
#EVAL_OBJ=evaluate.obj
#EVAL_VER=EVAL_APERY
#EVAL_OBJ=evaluate_apery.obj
EVAL_VER=EVAL_TWIG
EVAL_OBJ=evaluate_twig.obj

USE_DFPN=no

OBJS = mate1ply.obj misc.obj timeman.obj $(EVAL_OBJ) position.obj \
	 tt.obj main.obj \
	 movegen.obj search.obj uci.obj movepick.obj thread.obj ucioption.obj \
	 benchmark.obj book.obj \
	 shogi.obj mate.obj problem.obj

CC=cl
LD=link

# Compile Options
#
# Visual C++オプション
#
# /D_CRT_SECURE_NO_WARNINGS
#                   secureな関数を使っていないときの警告を出さない
# /Zc:forScope      スコープ ループに標準 C++ を適用する
# /Wall             警告をすべて有効にする
# /GS[-]            セキュリティ チェックを有効にする
# /favor:<blend|AMD64|EM64T> 最適化するプロセッサ
# /GL[-]            リンク時のコード生成を行う
# /RTCs             スタック フレーム ランタイム チェック
# /RTCu             初期化されていないローカル変数のチェック
# -DNDEBUG 
FLAGS = -DNDEBUG -D$(EVAL_VER) -DNANOHA -DCHK_PERFORM  \
	-DOLD_LOCKS /favor:AMD64 /EHsc /D_CRT_SECURE_NO_WARNINGS \
	 /GL /Zc:forScope

!IF "$(USE_DFPN)" == "yes"
FLAGS = $(FLAGS) -DUSE_DFPN
OBJS = $(OBJS) SearchMateDFPN.obj
!ENDIF

#CXXFLAGS=$(FLAGS) /MT /W4 /Wall /nologo /Od /GS /RTCsu
CXXFLAGS=$(FLAGS) /MD /W3 /nologo /Ox /Ob2 /GS- /Gm /Zi
#CXXFLAGS=$(FLAGS) /MD /W3 /nologo /Od /GS /Gs /Zi /RTCsu
LDFLAGS=/NOLOGO /STACK:16777216,32768 /out:$(EXE) /LTCG /DEBUG
PGOLDFLAGS1=/NOLOGO /STACK:33554432,32768 /out:$(EXE) /LTCG:PGI
PGOLDFLAGS2=/NOLOGO /STACK:33554432,32768 /out:$(EXE) /LTCG:PGO


all: $(EXE)

$(EXE) : $(OBJS)
	$(LD) $(LDFLAGS) $(OBJS) User32.lib

.cpp.obj :
	$(CC) $(CXXFLAGS) /c $*.cpp

clean :
	del /q *.obj
	del /q *.idb
	del /q *.pdb
	del /q *.pgc
	del /q *.pgd
	del /q *.suo
	del    $(PGOLOG)
	del    $(EXE)

pgo: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) bench 128 1 8
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-mate1: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) mate1
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-genmove: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) genmove
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-eval: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) eval
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

pgo-moves: $(OBJS)
	$(LD) $(PGOLDFLAGS1) $(OBJS) User32.lib
	$(EXE) moves
	pgomgr /merge $(PGD)
	pgomgr /summary $(PGD) > $(PGOLOG)
	$(LD) $(PGOLDFLAGS2) $(OBJS) User32.lib

prof-clean:
	del /q *.pgc
	del /q *.pgd
	del    $(PGOLOG)
	del    $(EXE)

pack:
	mkdir src
	copy *.cpp src
	copy *.h src
	copy Makefile src
	copy Makefile.vs src
	copy *.txt src
