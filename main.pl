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
