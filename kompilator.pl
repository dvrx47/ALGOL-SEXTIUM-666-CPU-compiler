/***********************************************
*
*		Autor: Mikołaj Kowalik
*	    Prolog wersja: 6. 6. 4.
*
***********************************************/

/* 		  STRUKTURA PROGRAMU

	0   +--------------------+
		|   	rsp			 |
	1	+--------------------+
	V   |      Zmienne       |
	    ......................
	V   |	 Kod Programu    |
	    ......................
	    
	    ......................
	^   | 	   Stos          |
  ffff  +--------------------+
*/

algol16( Source, SextiumBin ) :-
	leksuj( Source, [_,_,_|T] ),
	wczytaj_zmienne( 1, T, Zmienne, X, Offset ),
	wczytaj_instrukcje( X, Instrukcje),
	OffsetDec is Offset - 1,
	dorobOffset(OffsetDec, ZeroSpace ),
	Res = [0xffff, ZeroSpace|Tail],
	koduj( Offset, Instrukcje, Zmienne, Tail ),
	flatten( Res, SextiumBin ).
	
koduj(_, [tokEnd|_], _, [0x9100, 0x0] ) :- !.
koduj(_, [], _, []) :- !.

koduj(X, [[tokRead|T] | Reszta ], Zmienne, Out) :- !,
	kodujRead( T, KodHex, Zmienne, Length ),
	K is Length+X, Out = [KodHex| Out1 ],
	koduj(K, Reszta, Zmienne, Out1 ).

koduj(X, [[tokWrite|Arytmetyczne] | Reszta], Zmienne, Out) :- !,
	kodujWrite(X, Arytmetyczne, KodHex, Zmienne, Length ),
	NowyOffset is Length+X, Out = [KodHex | Out1],
	koduj( NowyOffset, Reszta, Zmienne, Out1 ).
	
koduj(X, [[tokLabel(Var)|Arytmetyczne] | Reszta], Zmienne, Out) :- !,
	kodujWyrazenie(X, Arytmetyczne, KodHex, Zmienne, Length ),
	adresZmiennej(Var, Zmienne, Adres),
	NowyOffset is Length+X+7, Out = [KodHex, [0x9425, 0xffff, 0x9453, Adres, 0x9493, 0x0, 0xffff]| Out1],
	koduj( NowyOffset, Reszta, Zmienne, Out1 ).
	
koduj(AktualnyOffset, [[ tokIf, Wyr_Logiczne, Instrukcje, ElseInstrukcje ]| Reszta], Zmienne, Out) :- !,
	kodujWyrazenieLogiczne( AktualnyOffset, Wyr_Logiczne, Zmienne, HexWyrazenia, OffsetWyrazenia ), 
	OffsetDlaI1 is AktualnyOffset + OffsetWyrazenia + 6,
	kodujInstrukcjeZlozona( OffsetDlaI1, Instrukcje, Zmienne, HexI1, OffsetI1 ),
	OffsetDlaElse is OffsetDlaI1 + OffsetI1+2,
	kodujInstrukcjeZlozona( OffsetDlaElse, ElseInstrukcje, Zmienne, HexElse, OffsetElse ),
	zrobJumpDlaIf(HexJump, AktualnyOffset, OffsetWyrazenia, OffsetI1),
	NowyOffset is OffsetDlaI1 + OffsetI1 + OffsetElse + 5,
	Out = [ [HexWyrazenia, HexJump, HexI1, [0x9800, NowyOffset],HexElse, [0x9493, 0x0, 0xffff]] |Out1],
	koduj( NowyOffset, Reszta, Zmienne, Out1 ).
	
koduj( AktualnyOffset, [[tokWhile, Logiczne, Instrukcje] | Reszta], Zmienne, Out ) :- !,
	kodujWyrazenieLogiczne( AktualnyOffset, Logiczne, Zmienne, HexLogiczne, OffsetLogiczne ),
	OffsetDlaInstrukcji is AktualnyOffset + OffsetLogiczne + 4,
	kodujInstrukcjeZlozona(OffsetDlaInstrukcji, Instrukcje, Zmienne, HexInstrukcji, OffsetOdInstrukcji),
	NowyOffset is OffsetDlaInstrukcji + OffsetOdInstrukcji + 2,
	jumpDlaWhile( NowyOffset, HexJump ),
	Out = [ [HexLogiczne, HexJump, HexInstrukcji, [0x9800, AktualnyOffset] ] | Out1],
	koduj(NowyOffset, Reszta, Zmienne, Out1).
	

jumpDlaWhile(NoHex, [0x9424, 0xffff, 0x9460, NoHex]).
	
	
kodujInstrukcjeZlozona( AO, Instrukcje, Zmienne, KodHex, Offset ) :-
	koduj(AO, Instrukcje, Zmienne, KodHex),
	flatten(KodHex, ForOffset),
	length(ForOffset, Offset).	
	
zrobJumpDlaIf([0x9594, 0x1, 0xffff, 0x2b59, AdresProgramu, 0x4570], AktualnyOffset, OffsetWyrazenia, OffsetI1 ) :-
	AdresProgramu is AktualnyOffset + OffsetWyrazenia + OffsetI1 + 8.
	
kodujWyrazenieLogiczne(AktualnyOffset, WyrazenieRaw, Zmienne, [Out, 0x9493, 0x0, 0xffff], Offset) :-
	phrase(wyrazenie_logiczne(Wyrazenie), WyrazenieRaw),
	onp(Wyrazenie, WyrazenieONP),
	kodujWyrazenie(AktualnyOffset, WyrazenieONP, Out, Zmienne, 0, OffsetRaw),
	Offset is OffsetRaw + 3.

kodujRead([tokLabel(T)|_], [0x9491, X, 0x1, 0x3000], Zmienne, 4) :- adresZmiennej(T, Zmienne, X).

kodujWrite(_, [tokNumber(N)], [0x9500, N, 0x9100, 0x2], _, 4 ) :- !.
kodujWrite(_, [tokLabel(Var)], [0x9425, Adres, 0x9100, 0x2], Zmienne, 4) :- !, adresZmiennej(Var, Zmienne, Adres).

kodujWrite(AktualnyOffset, Wyrazenie, [HexWyrazenia, [0x9425, 0xffff, 0x9100, 0x2, 0x9493, 0x0, 0xffff] ], Zmienne, Length ) :- 
	kodujWyrazenie(AktualnyOffset, Wyrazenie, HexWyrazenia, Zmienne, LengthTemp),
	Length is LengthTemp+7.

adresZmiennej(X,[zmienna(X,H)|_], H ) :- !.
adresZmiennej(X,[_|T], Res) :- adresZmiennej(X, T, Res).

dorobOffset(0, [] ) :- !.
dorobOffset(N, Offset) :- Offset = [0x0|Off], K is N-1, dorobOffset( K, Off ).


kodujWyrazenie(AO, Wyrazenie, HexWyrazenia, Zmienne, Length) :-
	 phrase( wyrazenie(Drzewo), Wyrazenie ),
	 onp(Drzewo, ListaOnp),
	 kodujWyrazenie(AO, ListaOnp, HexWyrazenia, Zmienne, 0, Length).


	 
kodujWyrazenie(_, [], [], _, TempLength, TempLength) :- !.

kodujWyrazenie(AktualnyOffset, [tokOr|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaOr is AktualnyOffset+TempLength,
	orZeStosu(OffsetDlaOr, KodHex, Offset),
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokAnd|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaAnd is AktualnyOffset+TempLength,
	andZeStosu(OffsetDlaAnd, KodHex, Offset),
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokGeq|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaLt is AktualnyOffset+TempLength,
	ltZeStosu(OffsetDlaLt, KodHex0, Offset0),
	negacjaZeStosu(KodHex1, Offset1 ),
	HexWyrazenia = [KodHex0, KodHex1|HexDalszy], 
	NewLength is Offset0 + Offset1 + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokGt|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaLeq is AktualnyOffset+TempLength,
	leqZeStosu(OffsetDlaLeq, KodHex0, Offset0),
	negacjaZeStosu(KodHex1, Offset1 ),
	HexWyrazenia = [KodHex0, KodHex1|HexDalszy], 
	NewLength is Offset0 + Offset1 + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokLeq|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaLeq is AktualnyOffset+TempLength,
	leqZeStosu(OffsetDlaLeq, KodHex, Offset),
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokLt|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaLt is AktualnyOffset+TempLength,
	ltZeStosu(OffsetDlaLt, KodHex, Offset),
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokNeq|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaPorownania is AktualnyOffset+TempLength,
	porownajZeStosu(OffsetDlaPorownania, KodHex0, Offset0),
	negacjaZeStosu(KodHex1, Offset1 ),
	HexWyrazenia = [KodHex0, KodHex1|HexDalszy], 
	NewLength is Offset0 + Offset1 + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokEq|T], HexWyrazenia, Zmienne, TempLength, Length) :- !, 
	OffsetDlaPorownania is AktualnyOffset+TempLength,
	porownajZeStosu(OffsetDlaPorownania, KodHex, Offset),
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset + TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokNot|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	negacjaZeStosu(KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokNumber(N)|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	liczbaNaStos(N, KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).
	
kodujWyrazenie(AktualnyOffset, [tokLabel(Var)|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	zmiennaNaStos(Var, KodHex, Zmienne, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).
	
kodujWyrazenie(AktualnyOffset, [tokPlus|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	dodajZeStosu(KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokMinus|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	odejmijZeStosu(KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).
	
kodujWyrazenie(AktualnyOffset, [tokTimes|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	pomnozZeStosu(KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).

kodujWyrazenie(AktualnyOffset, [tokDiv|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	dzielZeStosu(KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).
	
kodujWyrazenie(AktualnyOffset, [tokMod|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	moduloZeStosu(KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).
	
kodujWyrazenie(AktualnyOffset, [unMinus|T], HexWyrazenia, Zmienne, TempLength, Length) :- !,
	unMinusZeStosu(KodHex, Offset), 
	HexWyrazenia = [KodHex|HexDalszy], 
	NewLength is Offset+TempLength,
	kodujWyrazenie(AktualnyOffset, T, HexDalszy, Zmienne, NewLength, Length).
	
orZeStosu(AO, [0x9594, 0x1, 0x0, 0x2a34, 
			    0x24a4, 0x52a5, 0x95b5, 0x1, 
			    0x9457, Fail, 0x9594, 0x1,
			    0x0, 0x2a49, 0x1, 0x3980, End,
			    0x9594, 0x1, 0x0, 0x2a49, 0x0, 0x3000], 23) :-
			    	End is AO + 23,
			    	Fail is AO + 17.
	
andZeStosu(AO, [0x9594, 0x1, 0x0, 0x2a34, 
			    0x24a4, 0x52a5, 0x95b5, 0x2, 
			    0x9457, Fail, 0x9594, 0x1,
			    0x0, 0x2a49, 0x0001, 0x3980, End,
			    0x9594, 0x1, 0x0, 0x2a49, 0x0, 0x3000], 23) :-
			    	End is AO + 23,
			    	Fail is AO + 17.
	
negacjaZeStosu( [0x9594, 0x1, 0x0, 0x2a42, 0x52bb, 0x59a3, 0x0001], 7).

porownajZeStosu(AO, [0x9594, 0x0001, 0x0, 0x2a34, 0x24a4, 0x52b5, 0x9456, Adres1, 0x5493, 0x0, 0x9800, AdresEnd, 0x5493, 0x1], 14) :- 
	Adres1 is AO + 12,
	AdresEnd is AO + 14.
	
leqZeStosu(AO, [0x9594, 0x1, 0x0, 0x2a34,
			   0x24a4, 0x5249, 0x2, 0x5d4d,
			   0x545b, 0x4946, Eq, 0x4947,
			   Lt, 0x9594, 0x1, 0x0,
			   0x2a43, 0x9800, End, 0x9594, 0x1,
			   0x0, 0x2a45, 0x3980, End, 0x9594,
			   0x1, 0x0, 0x2424, 0xa452, 0x0b59,
			    Lt, 0x4576, 0x5980, Nlt], 35) :-
					Eq is AO + 25,
					End is AO + 35,
					Lt is AO + 19,
					Nlt is AO + 13.
	
ltZeStosu(AO, [0x9594, 0x1, 0x0, 0x2a34,
			   0x24a4, 0x5249, 0x2, 0x5d4d,
			   0x545b, 0x4946, Eq, 0x4947,
			   Lt, 0x9594, 0x1, 0x0,
			   0x2a43, 0x9800, End, 0x9594, 0x1,
			   0x0, 0x2a45, 0x3980, End, 0x9594,
			   0x1, 0x0, 0x2424, 0xa452, 0x0b59,
			    Lt, 0x4575, 0x9800, Nlt], 35) :-
					Eq is AO + 25,
					End is AO + 35,
					Lt is AO + 19,
					Nlt is AO + 13.	
	
unMinusZeStosu( [0x9594, 0x1, 0x0, 0x2a42, 0x52bb, 0x3000], 6).

dodajZeStosu(   [0x9594, 0x1, 0x0, 0x2a42, 0x4a45, 0x2a39, 0x1, 0x54b5, 0x9453, 0x0], 10).
odejmijZeStosu( [0x9594, 0x1, 0x0, 0x2a42, 0x4a45, 0x2b39, 0x1, 0x54b5, 0x9453, 0x0], 10).
pomnozZeStosu(  [0x9594, 0x1, 0x0, 0x2a42, 0x4a45, 0x2c39, 0x1, 0x54b5, 0x9453, 0x0], 10).
dzielZeStosu(   [0x9594, 0x1, 0x0, 0x2a42, 0x4a45, 0x2d39, 0x1, 0x54b5, 0x9453, 0x0], 10).
moduloZeStosu(  [0x9594, 0x1, 0x0, 0x2a42, 0x4a45, 0x2d59, 0xfff0, 0x5e39, 0x1, 0x54b5, 0x9453, 0x0000], 12).
	
zmiennaNaStos(Var, [0x9425, Adres, 0x9424, 0x0, 0x5300, 0x954b, 0x1, 0x5945, 0x0, 0x3000], Zmienne, 10) :- adresZmiennej(Var, Zmienne, Adres).
liczbaNaStos(N, [0x9424, 0x0, 0x9300, N, 0x954b, 0x1, 0x5945, 0x0, 0x3000], 9).
	
onp( tokLabel(Var), [tokLabel(Var)] ) :- !.
onp( tokNumber(Const), [tokNumber(Const)] ) :- !.

onp( not(Relacyjne), Out ) :- !, onp(Relacyjne, Lista), flatten( [Lista, tokNot], Out ).
onp( and(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokAnd], Out).
onp( or(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokOr], Out).

onp( rowne(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokEq], Out).
onp( rozne(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokNeq], Out).
onp( '<'(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokLt], Out).
onp( '<='(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokLeq], Out).
onp( '>'(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokGt], Out).
onp( '>='(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokGeq], Out).

onp( unMinus(X), [X, unMinus] ) :- !. 
onp( plus(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokPlus], Out). 
onp( minus(L, R), Out) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokMinus], Out).
onp( mult(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokTimes], Out).
onp( divide(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokDiv], Out).
onp( modulo(L, R), Out ) :- !, onp(L, Lewe), onp(R, Prawe), flatten( [Lewe, Prawe, tokMod], Out).

wczytaj_instrukcje( [tokEnd|_], [tokEnd]):- !.

wczytaj_instrukcje( [tokRead|T], X ) :- !, 
	instrukcja_read( T, Out, Rest), 
	X=[ [tokRead|Out] |Ogon], 
	wczytaj_instrukcje( Rest, Ogon ).
	
wczytaj_instrukcje( [tokWrite|T], X) :- !, 
	instrukcja_write(T, Out, Rest), 
	X=[ [tokWrite|Out] |Ogon], 
	wczytaj_instrukcje( Rest, Ogon ).
	
wczytaj_instrukcje( [tokLabel(N)|T], X) :- !, 
	instrukcja_assign(T, Out, Rest), 
	X=[ [tokLabel(N)|Out] |Ogon], 
	wczytaj_instrukcje( Rest, Ogon ).
	
wczytaj_instrukcje( [tokIf|T], X) :- !, 
	instrukcja_If(T, Logiczne, I1, I2, Rest), 
	X=[ [ tokIf, Logiczne, I1, I2 ] |Ogon], 
	wczytaj_instrukcje( Rest, Ogon ).
	
wczytaj_instrukcje( [tokWhile|T], X) :- !, 
	instrukcja_While(T, Logiczne, I1, Rest), 
	X=[ [tokWhile, Logiczne, I1] |Ogon], 
	wczytaj_instrukcje( Rest, Ogon ).

instrukcja_If( T, Logiczne, I1, I2, Rest ) :- instrukcja_If(0, T, Logiczne, I1, I2, Rest ).

instrukcja_If( 0, [tokThen|Program], [], I1, I2, Rest ) :- !, instrukcja_If(1, Program, _, I1, I2, Rest ).
instrukcja_If( 0, [A|Program], Logiczne, I1, I2, Rest ) :- !, 
	Logiczne = [A|Logiczne2],
	instrukcja_If( 0, Program, Logiczne2, I1, I2, Rest ).

instrukcja_If( 1, [tokFi, tokSColon|Program], _, [], [], Program ) :- !.	
instrukcja_If( 1, [tokFi|Program], _, [], [], Program ) :- !.
instrukcja_If( 1, [tokElse|Program], _, [], I2, Rest ) :- !,
	instrukcja_If(2, Program, _, _, I2, Rest ).

instrukcja_If( 1, [tokLabel(Z)|Program], _, I1, I2, Rest ) :- !,
	instrukcja_assign(Program, ListaAssign, Program2 ),
	I1 = [ [tokLabel(Z)| ListaAssign] | Tail ],
	instrukcja_If(1, Program2, _, Tail, I2, Rest ).
	
instrukcja_If(1, [tokRead| Program], _, I1, I2, Rest ) :- !,
	instrukcja_read( Program, ReadList, Program2 ),
	I1 = [ [tokRead|ReadList] | Tail],
	instrukcja_If( 1, Program2, _, Tail, I2, Rest ).
	
instrukcja_If(1, [tokWrite|Program], _, I1, I2, Rest ):- !,
	instrukcja_write( Program, WriteList, Program2 ),
	I1 = [ [tokWrite|WriteList] | Tail ],
	instrukcja_If(1, Program2, _, Tail, I2, Rest).
	
instrukcja_If( 1, [tokIf|Program], _, I1, I2, Rest ) :- !,
	instrukcja_If( Program, Logiczne, Instrukcje1, Instrukcje2, Program2 ),
	I1 = [ [tokIf, Logiczne, Instrukcje1, Instrukcje2] | Tail],
	instrukcja_If(1, Program2, _, Tail, I2, Rest ).

instrukcja_If(1, [tokWhile|Program], _, I1, I2, Rest ) :- !,
	instrukcja_While(Program, Logiczne, Instrukcje, Program2),
	I1 = [ [tokWhile, Logiczne, Instrukcje] | Tail ],
	instrukcja_If(1, Program2, _, Tail, I2, Rest).

instrukcja_If( 2, [tokFi, tokSColon|Program], _, _, [], Program ) :- !.
instrukcja_If( 2, [tokFi| Program], _, _, [], Program ) :- !.

instrukcja_If( 2, [tokLabel(Z)|Program], _, _, I2, Rest ) :- !,
	instrukcja_assign(Program, ListaAssign, Program2 ),
	I2 = [ [tokLabel(Z)| ListaAssign] | Tail ],
	instrukcja_If(2, Program2, _, _, Tail, Rest ).
	
instrukcja_If(2, [tokRead| Program], _, _, I2, Rest ) :- !,
	instrukcja_read( Program, ReadList, Program2 ),
	I2 = [ [tokRead|ReadList] | Tail],
	instrukcja_If( 2, Program2, _, _, Tail, Rest ).
	
instrukcja_If(2, [tokWrite|Program], _, _, I2, Rest ):- !,
	instrukcja_write( Program, WriteList, Program2 ),
	I2 = [ [tokWrite|WriteList] | Tail ],
	instrukcja_If(2, Program2, _, _, Tail, Rest).
	
instrukcja_If( 2, [tokIf|Program], _, _, I2, Rest ) :- !,
	instrukcja_If( Program, Logiczne, Instrukcje1, Instrukcje2, Program2 ),
	I2 = [ [tokIf, Logiczne, Instrukcje1, Instrukcje2] | Tail],
	instrukcja_If(2, Program2, _, _, Tail, Rest ).
	
instrukcja_If(2, [tokWhile|Program], _, _, I2, Rest ) :- !,
	instrukcja_While(Program, Logiczne, Instrukcje, Program2),
	I2 = [ [tokWhile, Logiczne, Instrukcje] | Tail ],
	instrukcja_If(2, Program2, _, _, Tail, Rest).

instrukcja_While(Program, Logiczne, I1, Rest) :- instrukcja_While(0, Program, Logiczne, I1, Rest ).

instrukcja_While(0, [tokDo|Program], [] , I1, Rest ) :- !, instrukcja_While( 1, Program, _, I1, Rest ).
instrukcja_While(0, [A|Program], Logiczne, I1, Rest ) :- !, Logiczne = [A|Logiczne2], instrukcja_While(0, Program, Logiczne2, I1, Rest ).

instrukcja_While( 1, [tokDone, tokSColon|Program], _, [], Program ) :- !.
instrukcja_While( 1, [tokDone|Program], _, [], Program ) :- !.

instrukcja_While( 1, [tokLabel(Z) | Program ], _, I1, Rest ) :- !, 
	instrukcja_assign(Program, ListaAssign, Program2),
	I1 = [ [tokLabel(Z)| ListaAssign] | Tail ],
	instrukcja_While( 1, Program2, _, Tail, Rest ).
	
instrukcja_While( 1, [tokRead | Program], _, I1, Rest ) :- !,
	instrukcja_read(Program, ListRead, Program2),
	I1 = [ [tokRead | ListRead ] | Tail ],
	instrukcja_While( 1, Program2, _, Tail, Rest).
	
instrukcja_While( 1, [tokWrite | Program ], _, I1, Rest ) :- !,
	instrukcja_write(Program, ListWrite, Program2),
	I1 = [ [ tokWrite | ListWrite ] | Tail ],
	instrukcja_While(1, Program2, _, Tail, Rest).
	
instrukcja_While( 1, [tokIf|Program], _, I1, Rest ) :- !,
	instrukcja_If( Program, Logiczne, Instrukcje1, Instrukcje2, Program2 ),
	I1 = [ [tokIf, Logiczne, Instrukcje1, Instrukcje2] | Tail],
	instrukcja_While(1, Program2, _, Tail, Rest ).
	
instrukcja_While( 1, [tokWhile|Program], _, I1, Rest ) :- !,
	instrukcja_While(Program, Logiczne, Instrukcje, Program2),
	I1 = [ [tokWhile, Logiczne, Instrukcje] | Tail ],
	instrukcja_While( 1, Program2, _, Tail, Rest ).

instrukcja_read( [tokSColon|Program], [], Program ) :- !.
instrukcja_read( [tokEnd|Program], [], [tokEnd|Program] ) :- !.
instrukcja_read( [tokDone|Program], [], [tokDone|Program] ) :- !.
instrukcja_read( [tokFi|Program], [], [tokFi|Program] ) :- !.
instrukcja_read( [tokElse|Program], [], [tokElse|Program] ) :- !.
instrukcja_read( [A|B], X, Program ) :- X=[A|Y], instrukcja_read( B, Y, Program ).

instrukcja_write( [tokSColon|Program], [], Program ) :- !.
instrukcja_write( [tokEnd|Program], [], [tokEnd|Program] ) :- !.
instrukcja_write( [tokDone|Program], [], [tokDone|Program] ) :- !.
instrukcja_write( [tokFi|Program], [], [tokFi|Program] ) :- !.
instrukcja_write( [tokElse|Program], [], [tokElse|Program] ) :- !.
instrukcja_write( [A|B], X, Program ) :- X=[A|Y], instrukcja_write( B, Y, Program ).

instrukcja_assign( [tokSColon|Program], [], Program ) :- !.
instrukcja_assign( [tokEnd|Program], [], [tokEnd|Program] ) :- !.
instrukcja_assign( [tokDone|Program], [], [tokDone|Program] ) :- !.
instrukcja_assign( [tokFi|Program], [], [tokFi|Program] ) :- !.
instrukcja_assign( [tokElse|Program], [], [tokElse|Program] ) :- !.
instrukcja_assign( [tokAssgn|B], X, Program ) :- !, instrukcja_assign( B, X, Program ).
instrukcja_assign( [A|B], X, Program ) :- X=[A|Y], instrukcja_assign( B, Y, Program ).
	
wczytaj_zmienne(Offset, [tokBegin|Y], [], Y, Offset) :- !.
wczytaj_zmienne( N, [ tokLabel(Z) | T ], X, Res, Offset) :- !, 
	X=[ zmienna(Z, N)| Ogon], 
	K is N+1, 
	wczytaj_zmienne(K, T, Ogon, Res, Offset).
wczytaj_zmienne(N, [_|T], X, Y, Offset) :- wczytaj_zmienne(N, T, X, Y, Offset).

leksuj(Source,Out) :-
	phrase(lexer(Raw), Source),
	usunKomentarze(0, Raw, Out).

lexer(Tokens) -->
   white_space,
   (  (  ":=",      !, { Token = tokAssgn }
      ;  ";",       !, { Token = tokSColon }
      ;	 "(*",      !, { Token = tokLComen }
      ;  "*)",      !, { Token = tokRComen }
      ;  "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "+",       !, { Token = tokPlus }
      ;  "-",       !, { Token = tokMinus }
      ;  "*",       !, { Token = tokTimes }
      ;  "=",       !, { Token = tokEq }
      ;  "<>",      !, { Token = tokNeq }
      ;  "<=",      !, { Token = tokLeq }
      ;  "<",       !, { Token = tokLt }
      ;  ">=",      !, { Token = tokGeq }
      ;  ">",       !, { Token = tokGt }
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L, Id),
            {  member((Id, Token), [ (and, tokAnd),
            						 (begin, tokBegin),
            						 (call, tokCall),
                                     (div, tokDiv),
                                     (do, tokDo),
                                     (done, tokDone),
                                     (else, tokElse),
                                     (end, tokEnd),
                                     (fi, tokFi),
                                     (if, tokIf),
                                     (local, tokLocal),
                                     (mod, tokMod),
                                     (not, tokNot),
                                     (or, tokOr),
                                     (procedure, tokProcedure),
                                     (program, tokProgram),
                                     (read, tokRead),
                                     (return, tokReturn),
                                     (then, tokThen),
                                     (value, tokValue),
                                     (while, tokWhile),
                                     (write, tokWrite)]),
               !
            ;  Token = tokLabel(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([95|T]) -->
   [95], !, alphanum(T).
alphanum([39|T]) -->
   [39], !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

usunKomentarze(0,[],[]) :- !.
usunKomentarze(N, [tokUnknown|T], Out) :- !, usunKomentarze(N, T, Out).
usunKomentarze(0,[A|T], Out) :- A\=tokLComen, !, Out=[A|Out2], usunKomentarze(0,T, Out2).
usunKomentarze(_,[tokLComen|T],Out) :- !, usunKomentarze(1, T, Out).
usunKomentarze(_,[tokRComen|T], Out) :- !, usunKomentarze(0, T, Out).
usunKomentarze(N, [_|T], Out ) :- usunKomentarze(N, T, Out).

wyrazenie(Expr) -->
   skladnik(Summand), wyrazenie(Summand, Expr).

wyrazenie(Acc, Expr) -->
   additive_op(Op), !, skladnik(Summand),
      { Acc1 =.. [Op, Acc, Summand] }, wyrazenie(Acc1, Expr).
wyrazenie(Acc, Acc) -->
   [].

skladnik(Expr) -->
   czynnik(Factor), skladnik(Factor, Expr).

skladnik(Acc, Expr) -->
   multiplicative_op(Op), !, czynnik(Factor),
      { Acc1 =.. [Op, Acc, Factor] }, skladnik(Acc1, Expr).
skladnik(Acc, Acc) -->
   [].

czynnik(Expr) -->
	  [tokMinus], !, { Expr = unMinus(Dalej) }, czynnik(Dalej)
   ;  [tokLParen], !, wyrazenie(Expr), [tokRParen]
   ;  [tokNumber(N)], !, { Expr = tokNumber(N) }
   ;  [tokLabel(Var)], { Expr = tokLabel(Var) }.

additive_op(plus) -->
   [tokPlus], !.
additive_op(minus) -->
   [tokMinus].

multiplicative_op(mult) -->
   [tokTimes], !.
multiplicative_op(divide) -->
   [tokDiv], !.
multiplicative_op(modulo) -->
   [tokMod].

wyrazenie_logiczne(Bool) -->
   disjunct(Disjunct), wyrazenie_logiczne(Disjunct, Bool).

wyrazenie_logiczne(Acc, Bool) -->
   [tokOr], !, disjunct(Disjunct),
      { Acc1 =.. [or, Acc, Disjunct] }, wyrazenie_logiczne(Acc1, Bool).
wyrazenie_logiczne(Acc, Acc) -->
   [].

disjunct(Disjunct) -->
   conjunct(Conjunct), disjunct(Conjunct, Disjunct).

disjunct(Acc, Disjunct) -->
   [tokAnd], !, conjunct(Conjunct),
      { Acc1 =.. [and, Acc, Conjunct] }, disjunct(Acc1, Disjunct).
disjunct(Acc, Acc) -->
   [].

conjunct(Conjunct) -->
   (  [tokLParen], wyrazenie_logiczne(Conjunct), [tokRParen], !
   ;  [tokNot], !, conjunct(NotConjunct),
         { Conjunct = not(NotConjunct) }
   ;  wyrazenie(LExpr), rel_op(Op), wyrazenie(RExpr),
         { Conjunct =.. [Op, LExpr, RExpr] }
   ).

rel_op(rowne) -->
   [tokEq], !.
rel_op(rozne) -->
   [tokNeq], !.
rel_op('<') -->
   [tokLt], !.
rel_op('<=') -->
   [tokLeq], !.
rel_op('>') -->
   [tokGt], !.
rel_op('>=') -->
   [tokGeq].
