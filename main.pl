:- [codigo_comum, puzzles_publicos].

%-------------------------------------------------------------------------------
%                combinacoes_soma(N, Els, Soma, Combs)
% combinacoes_soma(N, Els, Soma, Combs) significa que Combs eh uma lista
% ordenada com todas as combinacao dos elementos de Els, N a N, com soma Soma
%-------------------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Soma)), C),
    sort(C, Combs).

%-------------------------------------------------------------------------------
%                permutacoes_soma(N, Els, Soma, Perms)
% permutacoes_soma(N, Els, Soma, Perms) significa que Perms eh uma lista
% ordenada com todas as permutacoes das combinacoes dos elementos de Els, N a N,
% com soma Soma
%-------------------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms) :-
    combinacoes_soma(N, Els, Soma, Combs),
    findall(Perm, (member(Comb, Combs), permutation(Comb, Perm)), P),
    sort(P, Perms).

%-------------------------------------------------------------------------------
%                espaco_fila(Fila, Esp, H_V)
% espaco_fila(Fila, Esp, H_V) significa que Esp e um espaco de Fila, sendo que
% Fila eh uma linha ou coluna de um puzzle e H_V eh um dos atomos h ou v,
% conforme se trate de uma fila horizontal ou vertical, respetivamente
%-------------------------------------------------------------------------------
espaco_fila(Fila, espaco(Soma, Vars), H_V) :-
    append([Prefix, Vars, Suffix], Fila),
    Vars \= [],
    maplist(var, Vars),
    % Obter soma da fila
    ((H_V = h, last(Prefix, [_, Soma])) ; (H_V = v, last(Prefix, [Soma, _]))),
    nonvar(Soma),
    (([X|_] = Suffix, nonvar(X)) ; [] = Suffix). % Bloqueado a esquerda, ou fim

%-------------------------------------------------------------------------------
%                espacos_fila(H_V, Fila, Espacos)
% espacos_fila(H_V, Fila, Espacos) significa que Espacos eh a lista de todos os
% espacos de Fila, sendo que Fila eh uma linha ou coluna de um puzzle e H_V eh
% um dos atomos h ou v, conforme se trate de uma fila horizontal ou vertical,
% respetivamente
%-------------------------------------------------------------------------------
espacos_fila(H_V, Fila, Espacos) :-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), X) -> Espacos = X ; Espacos = [].

%-------------------------------------------------------------------------------
%                espacos_puzzle(Puzzle, Espacos)
% espacos_puzzle(Puzzle, Espacos) significa que Espacos eh a lista de todos os
% espacos de Puzzle, em que Puzzle eh um puzzle
%-------------------------------------------------------------------------------
espacos_puzzle(Puzzle, Espacos) :-
    maplist(espacos_fila(h), Puzzle, X),
    mat_transposta(Puzzle, PuzzleTrans),
    maplist(espacos_fila(v), PuzzleTrans, Y),
    flatten([X, Y], Espacos).

%-------------------------------------------------------------------------------
%                espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) significa que Esps_com eh
% a lista de espacos com variaveis em comum com Esp, exceptuando Esp. Os espacos
% em Esps_com aparecem pela mesma ordem que aparecem em Espacos
%-------------------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    espacos_com_posicoes_comuns(Espacos, Esp, Esps_com, []).
espacos_com_posicoes_comuns([], _, Esps_com, Esps_com).
espacos_com_posicoes_comuns([EspX|Espacos], EspY, Esps_com, Acc) :-
    EspX \== EspY,
    espaco(_,VarsX) = EspX,
    espaco(_,VarsY) = EspY,
    member(X, VarsX),
    member(Y, VarsY),
    (X == Y, var(X)) ->
        append(Acc, [EspX], NewAcc),
        espacos_com_posicoes_comuns(Espacos, EspY, Esps_com, NewAcc) ;
        espacos_com_posicoes_comuns(Espacos, EspY, Esps_com, Acc).

%-------------------------------------------------------------------------------
%                permutacoes_soma_espacos(Espacos, Perms_soma)
% permutacoes_soma_espacos(Espacos, Perms_soma) significa que Perms_soma eh a
% lista de listas de 2 elementos, em que o 1 elemento eh um espaco e o 2 eh a
% lista ordenada de permutacoes cuja soma eh igual a soma do espaco
%-------------------------------------------------------------------------------
permutacoes_soma_espacos([], []).
permutacoes_soma_espacos([Esp|Espacos], [[Esp,Perm]|Perms_soma]) :-
    Esp = espaco(Soma, Vars),
    length(Vars, N),
    permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perm),
    permutacoes_soma_espacos(Espacos, Perms_soma).

%-------------------------------------------------------------------------------
%                permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) significa que Perm
% eh uma permutacao possivel para o espaco Esp, em que Espacos eh uma lista de
% espacos e Perms_soma eh uma lista de listas tal como obtida pelo predicado
% permutacoes_soma_espacos/2
%-------------------------------------------------------------------------------
permutacao_possivel_espaco(PermX, EspX, Espacos, Perms_soma) :-
    espacos_com_posicoes_comuns(Espacos, EspX, Esps_com),
    member([EspX, Perms_soma_X], Perms_soma),
    member(PermX, Perms_soma_X),
    espaco(_, VarsX) = EspX,

    forall(member(EspY, Esps_com), (        
        member([EspY, Perms_soma_Y], Perms_soma),
        member(PermY, Perms_soma_Y),
        espaco(_, VarsY) = EspY,

        nth1(Xi, VarsX, X),
        nth1(Yi, VarsY, Y),
        X == Y,
        nth1(Xi, PermX, V),
        nth1(Yi, PermY, V)
    )).


%-------------------------------------------------------------------------------
%        permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) significa
% que Perms_poss eh uma lista de 2 elementos em que o primeiro eh a lista de
% variaveis de Esp e o segundo eh a lista ordenada de permutacoes possiveis para
% o espaco Esp, em que Espacos e uma lista de espacos e Perms_soma eh uma lista
% de listas tal como obtida pelo predicado permutacoes_soma_espacos/2
%-------------------------------------------------------------------------------
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) :-
    findall(P, permutacao_possivel_espaco(P, Esp, Espacos, Perms_soma), Perms),
    Esp = espaco(_, Vars),
    Perms_poss = [Vars, Perms].

%-------------------------------------------------------------------------------
%        permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) significa que
% Perms_poss_esps eh uma lista de permutacoes possiveis, em que Espacos eh uma
% lista de espacos
%-------------------------------------------------------------------------------
permutacoes_possiveis_espacos(Espacos, Ppe) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    maplist(permutacoes_possiveis_espaco(Espacos, Perms_soma), Espacos, Ppe).

%-------------------------------------------------------------------------------
%        numeros_comuns(Lst_Perms, Numeros_comuns)
% numeros_comuns(Lst_Perms, Numeros_comuns) significa que Numeros_comuns eh uma
% lista de pares (pos, numero), significando que todas as listas de Lst_Perms
% contem o numero numero na posicao pos, em que Lst_Perms eh uma lista de
% permutacoes
%-------------------------------------------------------------------------------
numeros_comuns(Lst_Perms, Numeros_comuns) :-
    findall((I,X), foreach(member(P,Lst_Perms), nth1(I, P, X)), Numeros_comuns).

%-------------------------------------------------------------------------------
%        atribui_comuns(Perms_Possiveis)
% atribui_comuns(Perms_Possiveis) em que Perms_Possiveis eh uma lista de
% permutacoes possiveis, atualiza esta lista atribuindo a cada espaco numeros
% comuns a todas as permutacoes possiveis para esse espaco
%-------------------------------------------------------------------------------
atribui_comuns(Perms_Possiveis) :-
    findall([Vars,Ps], (
        member([Vars,Ps], Perms_Possiveis),
        numeros_comuns(Ps, NC),
        foreach(member((I,X), NC), nth1(I, Vars, X))
    ), Perms_Possiveis).
    
%-------------------------------------------------------------------------------
%        retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) em que
% Perms_Possiveis eh uma lista de permutacoes possiveis, significa que
% Novas_Perms_Possiveis eh o resultado de tirar permutacoes impossiveis de
% Perms_Possiveis
%-------------------------------------------------------------------------------
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    maplist(retira_impossiveis_aux, Perms_Possiveis, Novas_Perms_Possiveis).
retira_impossiveis_aux([Vars, Perms1], [Vars, Perms2]) :-
    findall(P, (
        member(P, Perms1),
        forall(nth1(I, Vars, V), (
            nonvar(V) -> nth1(I, P, V) ; true
        ))
    ), Perms2).

%-------------------------------------------------------------------------------
%        simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis) em que Perms_Possiveis
% eh uma lista de permutacoes possiveis, significa que Novas_Perms_Possiveis
% eh o resultado de simplificar Perms_Possiveis, isto eh, aplicar-lhe os
% predicados atribui_comuns e retira_impossiveis, por esta ordem, ate nao haver
% mais alteracoes
%-------------------------------------------------------------------------------
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, X),
    X \== Perms_Possiveis ->
        simplifica(X, Novas_Perms_Possiveis) ;
        Novas_Perms_Possiveis = Perms_Possiveis.
