-module(erlplex).
-author("Graeme Defty <graeme.defty@gmail.com>").
%-export([file/1, file/2, bootstrap/0]).
-include("../include/erlplex.hrl").
-compile([export_all]).

%% @doc Creates a new simplex from the evaluation function and some intial points
%%      The function must take one vector and return the value at that point.
%% @spec create_simplex(F::fun/1(),Data::[vector[]]) -> simplex()

create_simplex(Func,Data)->
    Points = [ #point{value=Func(D), data=D} || D<-Data],
    #simplex{func=Func,points=lists:sort    (fun(A,B)->A#point.value > B#point.value end, Points)}.

%% @doc Operates on a simplex using the Nelder-Mead algorithm
%%      and returns the solution simplex.
%% @spec solve(S::simplex()) -> simplex()
%% @spec solve(S::simplex(),Precision::float) -> simplex()
%% @spec solve(S::simplex(),Precision::float,Limit::integer) -> simplex()
%%
%% precision specifies the acceptable error interval on the result (default 10^-6)
%% Limit specifies the maximum number of Nelder-Mead iterations (default 1000)

solve(S)                -> solve(S,0.0000001).
solve(S,Precision)      -> solve(S,Precision,1000).
solve(S,Precision,Limit)-> steps(S, Precision, Limit).

steps(S, _, 0) -> S;
steps(S, Precision, Steps) ->
    NewS = step(S),
    case (hd(NewS#simplex.points))#point.value - (lists:last(NewS#simplex.points))#point.value < Precision of
      true -> 
        NewS;
      false -> 
        steps(NewS, Precision, Steps-1)
    end.

step(S) ->
    CoG = average(tl(S#simplex.points), S#simplex.func),
    do_reflect(S, CoG).

do_reflect(S, CoG) ->
    Ref = reflect(hd(S#simplex.points),CoG,S#simplex.func),
    case Ref#point.value < (lists:last(S#simplex.points))#point.value of
      true -> 
        Ext = extend(hd(S#simplex.points),CoG,S#simplex.func),
        case Ext#point.value < Ref#point.value of
          true ->
            S#simplex{points=lists:merge(fun(A,B)->A>B end,[Ext],tl(S#simplex.points))};
          false ->
            S#simplex{points=lists:merge(fun(A,B)->A>B end,[Ref],tl(S#simplex.points))}
        end;
      false -> 
        do_contract(S, CoG)
    end.

do_contract(S,CoG) ->
    Cont = contract(hd(S#simplex.points),CoG,S#simplex.func),
    case Cont#point.value < (hd(S#simplex.points))#point.value of
      true ->
        S#simplex{points=lists:merge(fun(A,B)->A>B end,[Cont],tl(S#simplex.points))};
      false ->
        do_shrink(S)
    end.

do_shrink(S) -> 
    Points = lists:reverse(S#simplex.points),
    New_points = [hd(Points)|[average([hd(Points),Point],S#simplex.func) || Point <- tl(Points)]],
    S#simplex{points=lists:sort(fun(A,B)->A#point.value>B#point.value end, New_points)}.

%% @doc Reflects one point in a second one.
%%      If A is the vector of the point to be reflected in the point with vector B
%%      then the resultant reflected point is B+(B-A) or 2B-A
%% @spec reflect(A::point(),B::point()) -> point()

reflect(A, B, Func) ->
    Location = v_sum([v_scale(B#point.data,2),v_scale(A#point.data,-1)]),
    #point{value=Func(Location),data=Location}.


%% @doc Extend one point in a second one.
%%      This operation is like reflect, but goes twice the distance 
%%      past the reflection point
%%      i.e. here the resultant reflected point is B+2(B-A) or 3B-2A
%% @spec reflect(A::point(),B::point()) -> point()

extend(A, B, Func) ->
    Location = v_sum([v_scale(B#point.data,3),v_scale(A#point.data,-2)]),
    #point{value=Func(Location),data=Location}.


%% @doc Contracts to either the C1 point or the C2 point.
%%      These are represented by B-(B-A)/2 or B+(B-A)/2 respectively 
%%      i.e. (B+A)/2 and (3B-A)/2
%% @spec contract(A::point(),B::point()) -> point()

contract(A, B, Func) ->
    Loc1 = v_sum([v_scale(B#point.data,0.5),v_scale(A#point.data,0.5)]),
    Val1 = Func(Loc1),
    Loc2 = v_sum([v_scale(B#point.data,1.5),v_scale(A#point.data,-0.5)]),
    Val2 = Func(Loc2),
    case Val1 < Val2 of
      true ->   #point{value=Val1,data=Loc1};
      false ->  #point{value=Val2,data=Loc2}
    end.


%% @doc Shrinks a list of points towards  common point.
%%      i.e. produces a list whereeach of the points is replaced by the 
%%      average of that point and the specified point
%% @spec contract(A::point(),[B::point()]) -> [point()]

shrink(A, Points, Func) ->
    [average([A,Point], Func) || Point <- Points].


%% @doc Calculates the average of a set of points
%% @spec average(Filename::string()) -> ok

average(Points,Func) ->
%    io:format("Averaging ~p~n",[Points]),
    Data = v_sum([v_scale(Point#point.data, 1 / length(Points)) || Point <- Points]),
    #point{value=Func(Data),data=Data}.


%% @doc Sums a list of vectors (each represented by a list of co-ordinates).
%% @spec add([List::list()]) -> list()

v_sum(Vectors) ->
    lists:foldl(
            fun (X,Y) ->
                case Y of
                    []  ->  X; 
                    _   ->  lists:zipwith(fun (A,B) -> A+B end,X,Y)
                 end
            end,
        [], Vectors).


%% @doc Scales a vector by a scaling factor.
%% @spec scale(List::list(), Factor::float) -> list()

v_scale(Vector, Factor) ->
    [Factor*Val || Val<-Vector].
