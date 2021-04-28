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
    bagof(X, (member(X, VarsX), bagof(Y, (member(Y, VarsY), X == Y), _)), _) ->
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
    Esp = espaco(Sum, Vars),
    length(Vars, N),
    permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Sum, Perm),
    permutacoes_soma_espacos(Espacos, Perms_soma).