% Jose Pereira, ist1103252

:- [codigo_comum].
:- [puzzles_publicos].


% ------------------------------------------------------------------------------
% extrai_ilhas_linha(N_L, Linha, Ilhas)
% Ilhas eh a lista ordenada cujos Eos sao as ilhas da linha Linha,
% representada pelo numero N_L.
% ------------------------------------------------------------------------------

extrai_ilhas_linha(N_L,Linha,Ilhas):-
    extrai_ilhas_linha_aux(N_L,Linha,0, Ilhas).

extrai_ilhas_linha_aux(_,[],_,[]).

extrai_ilhas_linha_aux(N_L,[P|R],I, RAux):- % I eh usada para contar o numero da coluna.
    P==0, % Caso nao seja uma ilha (P eh 0).
    NovoI is I+1,
    extrai_ilhas_linha_aux(N_L,R,NovoI, RAux).

extrai_ilhas_linha_aux(N_L,[P|R], I, [ilha(P,(N_L,NovoI))|RAux]):-
    P\=0, % Caso seja uma ilha (P nao eh 0).
    NovoI is I+1,
    extrai_ilhas_linha_aux(N_L,R,NovoI, RAux).


% ------------------------------------------------------------------------------
% ilhas(Puz, Ilhas)
% Ilhas eh a lista ordenada cujos elementos sao as ilhas de Puz.
% ------------------------------------------------------------------------------

ilhas(Puz, Ilhas) :- 
    ilhas_aux(Puz, IlhasAux, 0),
    flatten(IlhasAux, Ilhas). % Transforma IlhasAux (lista de listas) numa unica lista. 

ilhas_aux([], [], _).

ilhas_aux([P|R], [Ilha | IlhasAux], I) :- % I eh usada para contar o numero da linha.
    N_L is I +1, 
    extrai_ilhas_linha(N_L, P, Ilha),
    ilhas_aux(R, IlhasAux, N_L).


% ------------------------------------------------------------------------------
% vizinhas(Ilhas, Ilha, Vizinhas)
% Vizinhas eh a lista ordenada cujos elementos sao as ilhas vizinhas de Ilhas.
% ------------------------------------------------------------------------------

linha_igual(Linha, Ilha) :- % Verifica se uma ilha esta na mesma linha que Ilha.
    Ilha = ilha(_, (L, _)),
    L == Linha.

coluna_igual(Coluna, Ilha) :-  % Verifica se uma ilha esta na mesma coluna que Ilha.
    Ilha = ilha(_, (_, C)),
    C == Coluna.

tem_coordenada_igual(Ilha, PontencialVizinha) :-  % Uma ilha esta na mesma coluna ou ilha que Ilha.
    PontencialVizinha = ilha(_,(Linha,_)), linha_igual(Linha, Ilha) ;
    PontencialVizinha = ilha(_,(_,Coluna)), coluna_igual(Coluna, Ilha).

obtem_primeiro([], []). % Obtem o primeiro elemento de uma lista.
obtem_primeiro([P|_], P).

obtem_ultimo([], []). % Obtem o ultimo elemento de uma lista.
obtem_ultimo(L, Ult) :- last(L, Ult).


vizinhas(Ilhas, Ilha, Vizinhas) :-
    % Obtem as ilhas na mesma coluna ou linha que Ilha.
    include(tem_coordenada_igual(Ilha), Ilhas, Vizinhas1),
    % Remove a propria Ilha.
    findall(IlhasVizinhas, (member(IlhasVizinhas, Vizinhas1), IlhasVizinhas \== Ilha), Vizinhas2),

    % Obtemos as ilhas a esquerda, direita, cima e baixo da Ilha.
    findall(IlhasEsquerda, (member(IlhasEsquerda, Vizinhas2),
        Ilha = ilha(_, (_, C1)), IlhasEsquerda =  ilha(_, (_, C2)), C2 < C1 ), VizinhasEsquerda),

    findall(IlhasDireita, (member(IlhasDireita, Vizinhas2), 
        Ilha = ilha(_, (_, C1)), IlhasDireita =  ilha(_, (_, C2)), C2 > C1 ), VizinhasDireita),

    findall(IlhasCima, (member(IlhasCima, Vizinhas2), 
        Ilha = ilha(_, (L1, _)), IlhasCima =  ilha(_, (L2, _)), L2 < L1 ), VizinhasCima),

    findall(IlhasBaixo, (member(IlhasBaixo, Vizinhas2), 
        Ilha = ilha(_, (L1, _)), IlhasBaixo =  ilha(_, (L2, _)), L2 > L1 ), VizinhasBaixo),

    % Obtemos as ilhas vizinhas da Ilha.
    obtem_primeiro(VizinhasBaixo, B),
    obtem_primeiro(VizinhasDireita, D),
    obtem_ultimo(VizinhasCima, C),
    obtem_ultimo(VizinhasEsquerda, E),

    % Transforma uma lista de listas numa lista de ilhas vizinhas.
    flatten([[C], [E], [D], [B]], Vizinhas).


% ------------------------------------------------------------------------------
% estado(Ilhas, Estado)
% Estado eh a lista ordenada cujos elementos sao as entradas referentes a cada 
% uma das ilhas de Ilhas.
% ------------------------------------------------------------------------------

estado(Ilhas, Estado) :-
    estado_aux(Ilhas, Ilhas, Estado).

estado_aux(Ilhas, [P | R], [[P, Vizinhas, []] | Res]) :- % Cria as entrada.
    vizinhas(Ilhas, P, Vizinhas),
    estado_aux(Ilhas, R, Res).

estado_aux(_, [], []). % Condicao de paragem.


% ------------------------------------------------------------------------------
% posicoes_entre(Pos1, Pos2, Posicoes)
% Posicoes eh a lista ordenada de posicoes entre Pos1 e Pos2.
% ------------------------------------------------------------------------------

posicoes_entre(Pos1, Pos2, Posicoes) :-
    % Caso Pos1 e Pos2 estejam na mesma coluna, e Pos1 numa linha mais acima de Pos2.
    Pos1 = (L1, C1), Pos2 = (L2, C2), C1 == C2, L1 =< L2 , 
    posicoes_entre_L(Pos1, Pos2, PosicoesAux),
    findall(Pos, (member(Pos, PosicoesAux), Pos \= Pos1, Pos \= Pos2), Posicoes) ;

    % Caso Pos1 e Pos2 estejam na mesma coluna, e Pos1 numa linha mais abaixo Pos2.
    Pos1 = (L1, C1), Pos2 = (L2, C2), C1 == C2, L1 > L2 , 
    posicoes_entre_L(Pos2, Pos1, PosicoesAux),
    findall(Pos, (member(Pos, PosicoesAux), Pos \= Pos1, Pos \= Pos2), Posicoes) ;

    % Caso Pos1 e Pos2 estejam na mesma linha, e Pos1 numa linha mais acima de Pos2.
    Pos1 = (L1, C1), Pos2 = (L2, C2), L1 == L2, C1 =< C2 , 
    posicoes_entre_C(Pos1, Pos2, PosicoesAux),
    findall(Pos, (member(Pos, PosicoesAux), Pos \= Pos1, Pos \= Pos2), Posicoes);

    % Caso Pos1 e Pos2 estejam na mesma linha, e Pos1 numa linha mais abaixo de Pos2.
    Pos1 = (L1, C1), Pos2 = (L2, C2), L1 == L2, C1 > C2 , 
    posicoes_entre_C(Pos2, Pos1, PosicoesAux),
    findall(Pos, (member(Pos, PosicoesAux), Pos \= Pos1, Pos \= Pos2), Posicoes);

    % Caso Pos1 e Pos2 nao estejam na mesma coluna ou linha.
    false.

posicoes_entre_L(Pos,Pos,[Pos]).
posicoes_entre_L(Pos1,Pos2,[Pos1|R]) :-
    Pos1 = (L1, C),
    Pos2 = (L2, C),
    L1 =< L2,
    NextL is L1+1, % Obter proxima linha.
    posicoes_entre_L((NextL, _),Pos2,R).
    

posicoes_entre_C(Pos,Pos,[Pos]).
posicoes_entre_C(Pos1,Pos2,[Pos1|R]) :-
    Pos1 = (L, C1),
    Pos2 = (L, C2),
    C1 =< C2,
    NextC is C1+1, % Obter proxima coluna.
    posicoes_entre_C((_, NextC),Pos2,R).


% ------------------------------------------------------------------------------
% cria_ponte(Pos1, Pos2, Ponte)
% Ponte eh a ponte entre Pos1 e Pos2.
% ------------------------------------------------------------------------------

cria_ponte(Pos1, Pos2, Ponte) :- % Caso Pos1 esteja a esquerda de Pos2.
    Pos1 = (L1, _),
    Pos2 = (L2, _),
    L1 < L2,
    
    Ponte = ponte(Pos1,Pos2).

cria_ponte(Pos1, Pos2, Ponte) :- % Caso Pos1 esteja a direita de Pos2.
    Pos1 = (L1, _),
    Pos2 = (L2, _),
    L1 > L2,
    Ponte = ponte(Pos2,Pos1).

cria_ponte(Pos1, Pos2, Ponte) :- % Caso Pos1 esteja em cima de Pos2. 
    Pos1 = (_, C1),
    Pos2 = (_, C2),
    C1 =< C2,
    Ponte = ponte(Pos1,Pos2).

cria_ponte(Pos1, Pos2, Ponte) :- % Caso Pos1 esteja em baixo de Pos2.
    Pos1 = (_, C1),
    Pos2 = (_, C2),
    C1 > C2,
    Ponte = ponte(Pos2,Pos1).


% ------------------------------------------------------------------------------
% caminho_livre(Pos1, Pos2, Posicoes, I, Vz)
% Verifica se a adicao da ponte(Pos1, Pos2) map faz que I e Vz deixem de ser
% vizinhas.
% ------------------------------------------------------------------------------

caminho_livre(Pos1, Pos2, Posicoes, I, Vz) :-
    I = ilha(_, (L1, C1)),
    Vz = ilha(_, (L2, C2)),
    caminho_livre_aux(Pos1, Pos2, (L1, C1), (L2, C2), Posicoes).

caminho_livre_aux(Pos1, Pos2, PosI, PosVz, _) :- % Caso Pos1 e Pos2 sejam I e Vz.
    Pos1 == PosI,
    Pos2 == PosVz;
    Pos1 == PosVz,
    Pos2 == PosI.

caminho_livre_aux(_, _, (L1, C1), (L2, C2), Posicoes) :-
    posicoes_entre((L1, C1), (L2, C2), PosicoesVizinhas),
    % Verificar se a ponte entre Pos1 e Pos2 nao tem nenhuma
    % posicao em comum com a ponte entre I e Vz.
    findall(Pos, (member(Pos, Posicoes), member(Pos, PosicoesVizinhas)), PosComum),
    length(PosComum, Len),
    Len == 0.


% ------------------------------------------------------------------------------
% actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, Entrada, Nova_Entrada)
% Nova_Entrada eh igual a Entrada, excepto no que diz respeito ah lista de ilhas
% vizinhas; esta eh atualizada, removendo as ilhas que deixaram de ser vizinhas,
% apos a adicao da ponte.
% ------------------------------------------------------------------------------

actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes, [Ilha, Vizinhas , R], Nova_Entrada) :-
    include(caminho_livre(Pos1, Pos2, Posicoes, Ilha), Vizinhas, Vizinhas_Atualizadas),
    Nova_Entrada = [Ilha, Vizinhas_Atualizadas , R].


% ------------------------------------------------------------------------------
% actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado)
% Novo_estado eh o estado que se obtem de Estado apos a atualizacao
% das ilhas vizinhas de cada uma das suas entradas.
% ------------------------------------------------------------------------------

actualiza_vizinhas_apos_pontes(Estado, Pos1, Pos2, Novo_estado) :-
    posicoes_entre(Pos1, Pos2, Posicoes),
    maplist(actualiza_vizinhas_entrada(Pos1, Pos2, Posicoes), Estado, Novo_estado).


% ------------------------------------------------------------------------------
% ilhas_terminadas(Estado, Ilhas_term)
% Ilhas_term eh a lista de ilhas que ja tem todas as pontes associadas.
% ------------------------------------------------------------------------------

ilhas_terminadas_aux([], []).

% Caso seja uma ilha terminada.
ilhas_terminadas_aux([[Ilha, _, Pontes] | R], [Ilha | Ilhas_Term]) :-
    eh_terminada(Ilha, Pontes),
    ilhas_terminadas_aux(R, Ilhas_Term).

% Caso seja uma ilha nao terminada.
ilhas_terminadas_aux([[Ilha, _, Pontes] | R], Ilhas_Term) :-
    nao_eh_terminada(Ilha, Pontes),
    ilhas_terminadas_aux(R, Ilhas_Term).

eh_terminada(Ilha, Pontes) :- % Verificar se a ilha esta terminada.
    Ilha = ilha(N_Pontes,_),
    length(Pontes, Len),
    N_Pontes == Len.

nao_eh_terminada(Ilha, Pontes) :- % Verificar se a ilha nao esta terminada.
    Ilha = ilha(N_Pontes,_),
    length(Pontes, Len),
    N_Pontes \== Len.

ilhas_terminadas(Estado, Ilhas_term) :-
    ilhas_terminadas_aux(Estado, Ilhas_term).


% ------------------------------------------------------------------------------
% tira_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Nova_entrada eh a entrada resultante de remover as ilhas de
% Ilhas_term, da lista de ilhas vizinhas de entrada.
% ------------------------------------------------------------------------------

tira_ilhas_terminadas_entrada(Ilhas_term, [Ilha, Vizinhas, Pontes], Nova_entrada) :-
    % Encontrar as ilhas comuns em Vizinhas e Ilhas_term.
    findall(Ilha1, (member(Ilha1, Vizinhas),member(Ilha1, Ilhas_term)), Comum),
    % Remover de Vizinhas as ilhas Comum.
    findall(Ilha2, (member(Ilha2, Vizinhas), \+ member(Ilha2, Comum)), Vizinhas_removidas),
    Nova_entrada = [Ilha, Vizinhas_removidas, Pontes].
    

% ------------------------------------------------------------------------------
% tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Novo_estado eh o estado resultante de aplicar o predicado
% tira_ilhas_terminadas_entrada a cada uma das entradas de Estado.
% ------------------------------------------------------------------------------

tira_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(tira_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).


% ------------------------------------------------------------------------------
% marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada)
% Nova_entrada eh a entrada obtida de Entrada da seguinte forma:
% Se a ilha de Entrada pertencer a Ilhas_term, o numer de pontes desta eh
% substituido por 'X'; em caso contrario Nova_entrada eh igual a Entrada
% ------------------------------------------------------------------------------

marca_ilha(Ilha, IlhaMarcada) :-
    Ilha = ilha(_,(L1, C1)),
    IlhaMarcada = ilha('X',(L1, C1)).

marca_ilhas_terminadas_entrada(Ilhas_term, Entrada, Nova_entrada) :-
    % Caso nao haja ilhas a marcar.
    Entrada = [Ilha, _, _],
    findall(X, (member(X, Ilhas_term), X == Ilha), Comum),
    length(Comum, Len),
    Len == 0,
    Nova_entrada = Entrada ;

    % Caso haja ilhas a marcar.
    Entrada = [Ilha, Vizinhas, Pontes],
    findall(X, (member(X, Ilhas_term), X == Ilha), Comum),
    length(Comum, Len),
    Len \== 0,
    Comum = [ IlhaParaMarcar | _],
    marca_ilha(IlhaParaMarcar, Marcada),
    Nova_entrada = [Marcada, Vizinhas, Pontes].


% ------------------------------------------------------------------------------
% marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado)
% Novo_estado eh o estado resultante de aplicar o predicado
% marca_ilhas_terminadas_entrada a cada uma das entradas de Estado.
% ------------------------------------------------------------------------------

marca_ilhas_terminadas(Estado, Ilhas_term, Novo_estado) :-
    maplist(marca_ilhas_terminadas_entrada(Ilhas_term), Estado, Novo_estado).

    
% ------------------------------------------------------------------------------
% junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado)
% Novo_estado eh o estado que se obtem por adicao de Num_pontes pontes
% entre Ilha1 e Ilha2.
% ------------------------------------------------------------------------------

adiciona_pontes_aux(_,_,_, [], []).

adiciona_pontes_aux(Ponte, Ilha1, Ilha2, [ Entrada | R], [ [Ilha1, Vz, [Ponte]] |EstadoComPonte]) :-
    Entrada = [Ilha , Vz , _],
    Ilha == Ilha1,
    adiciona_pontes_aux(Ponte, Ilha1, Ilha2, R, EstadoComPonte).

adiciona_pontes_aux(Ponte, Ilha1, Ilha2, [ Entrada | R], [ [Ilha2, Vz, [Ponte]] |EstadoComPonte]) :-
    Entrada = [Ilha , Vz , _],
    Ilha == Ilha2,
    adiciona_pontes_aux(Ponte, Ilha1, Ilha2, R, EstadoComPonte).

adiciona_pontes_aux(Ponte, Ilha1, Ilha2, [ Entrada | R], EstadoComPonte) :-
    Entrada = [Ilha , _ , _],
    Ilha \== Ilha1,
    Ilha \== Ilha2,
    adiciona_pontes_aux(Ponte, Ilha1, Ilha2, R, EstadoComPonte).

adiciona_pontes(Ponte, Ilha1, Ilha2, Estado, EstadoComPonte) :-
    adiciona_pontes_aux(Ponte, Ilha1, Ilha2, Estado, EstadoComPonte).



substitui_entradas_pontes(Estado, PontesAdicionadas, EstadoComPontes) :-
    maplist(substitui_entradas_pontes_aux(PontesAdicionadas), Estado, EstadoComPontes).

substitui_entradas_pontes_aux(PontesAdicionadas, Entrada, EntradaNova) :-
    Entrada = [Ilha, _, _],
    member([Ilha, _, _], PontesAdicionadas),
    findall(X, (member(X,PontesAdicionadas), X = [Ilha, _ , _]), EntradaNova).

substitui_entradas_pontes_aux(PontesAdicionadas, Entrada, EntradaNova) :-
    Entrada = [Ilha, _, _],
    \+ member([Ilha, _, _], PontesAdicionadas),
    EntradaNova = Entrada.


mesma_ilha_ponte_aux(_, [], []).

mesma_ilha_ponte_aux(EntradaSemPonte, [P | R], EntradaComPonte) :-
    EntradaSemPonte = [Ilha, _, _],
    member(Ilha, P),
    mesma_ilha_ponte_aux(EntradaSemPonte, R, EntradaComPonte).


mesma_ilha_ponte(EntradaSemPonte, EntradasComPontes, EntradaComPonte) :-
    EntradaSemPonte = [Ilha, _, _],
    mesma_ilha_ponte_aux(Ilha, EntradasComPontes, EntradaComPonte).

chama(E, P1, P2, E2) :-
    actualiza_vizinhas_apos_pontes(E, P1, P2, E2).

junta_pontes(Estado, Num_pontes, Ilha1, Ilha2, Novo_estado) :-
    Ilha1 = ilha(_, Pos1),
    Ilha2 = ilha(_, Pos2),
    cria_ponte(Pos1, Pos2, Ponte),
    adiciona_pontes(Ponte, Ilha1, Ilha2, Estado, PontesAdicionadas),
    substitui_entradas_pontes(Estado, PontesAdicionadas, EstadoComPontes),
    chama(EstadoComPontes, Pos1, Pos2, EstadoActualizado),
    writeln(EstadoActualizado).