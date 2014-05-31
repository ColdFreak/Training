-module(qsort).
-export([qsort/1]).

qsort([]) -> [];
qsort([Pivot|T]) ->
	qsort([X|| X <- T, X<Pivot]) 
			++ [Pivot] ++
	qsort([Y|| Y <- T, Y>Pivot]).
		
