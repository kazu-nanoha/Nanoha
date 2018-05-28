/*
  Nanoha, a USI shogi(japanese-chess) playing engine derived from Stockfish, a UCI chess playing engine.
  Copyright (C) 2004-2008 Tord Romstad (Glaurung author)
  Copyright (C) 2008-2015 Marco Costalba, Joona Kiiski, Tord Romstad
  Copyright (C) 2015-2017 Marco Costalba, Joona Kiiski, Gary Linscott, Tord Romstad
  Copyright (C) 2014-2018 Kazuyuki Kawabata

  Nanoha is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Nanoha is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#define DPawn         static_cast<int>( 100 * 0.9)
#define DLance        static_cast<int>( 350 * 0.9)
#define DKnight       static_cast<int>( 450 * 0.9)
#define DSilver       static_cast<int>( 550 * 0.9)
#define DGold         static_cast<int>( 600 * 0.9)
#define DBishop       static_cast<int>( 950 * 0.9)
#define DRook         static_cast<int>(1100 * 0.9)
#define DProPawn      static_cast<int>( 600 * 0.9)
#define DProLance     static_cast<int>( 600 * 0.9)
#define DProKnight    static_cast<int>( 600 * 0.9)
#define DProSilver    static_cast<int>( 600 * 0.9)
#define DHorse        static_cast<int>(1050 * 0.9)
#define DDragon       static_cast<int>(1550 * 0.9)
#define DKing         15000

