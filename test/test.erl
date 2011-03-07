-module(test).
-include("../include/erlplex.hrl").
-include_lib("eunit/include/eunit.hrl").

% ----------- helpers ------------
val(Vector)     -> lists:foldl(fun (A,B) -> A+B end, 0, Vector).
val2(Vector)    -> lists:foldl(fun (A,B) -> B-A end, 0, Vector).

sumlist(A)      -> lists:foldl(fun(X,Acc)-> X+Acc end, 0, A).
dist(A,B)       -> math:sqrt(sumlist(lists:zipwith(fun (X,Y)-> (X-Y)*(X-Y) end,A,B))).

% ------------ tests -------------
helpers_test_() ->
  [
  % sumlist
    ?_assertEqual(6, sumlist([1,2,3])),
    ?_assertEqual(0, sumlist([])),
  % dist
    ?_assertEqual(5.0, dist([1,5],[4,9])),

    ?_assert(true)
  ].

v_sum_test_() ->
  [
    ?_assertEqual([13,8,9],
                erlplex:v_sum([[1,2,3],
                             [9,4,5],
                             [3,2,1]])),
    ?_assert(true)
  ].

v_scale_test_() ->
  [
    ?_assertEqual([1.2,2.4,4.8],
                erlplex:v_scale([1,2,4], 1.2)),
    ?_assert(true)
  ].


average_test_() ->
  [
    ?_assertEqual(#point{value=12.0,data=[5.0,3.0,4.0]},
                erlplex:average([#point{value=0,data=[1,2,3]},
                                 #point{value=0,data=[9,4,5]}],
                                 fun val/1)),
    ?_assert(true)
  ].

reflect_test_() ->
  [
    ?_assertEqual(#point{value=30,data=[17,6,7]},
                erlplex:reflect(#point{value=0,data=[1,2,3]},
                                #point{value=0,data=[9,4,5]},
                                fun val/1)),
    ?_assert(true)
  ].

extend_test_() ->
  [
    ?_assertEqual(#point{value=-42,data=[25,8,9]},
                erlplex:extend(#point{value=0,data=[1,2,3]},
                               #point{value=0,data=[9,4,5]},
                               fun val2/1)),
    ?_assert(true)
  ].

contract_test_() ->
  [
    ?_assertEqual(#point{value=12.0,data=[5.0,3.0,4.0]},
                erlplex:contract(#point{value=0,data=[1,2,3]},
                               #point{value=0,data=[9,4,5]},
                               fun val/1)),
    ?_assertEqual(#point{value=-24.0,data=[13.0,5.0,6.0]},
                erlplex:contract(#point{value=0,data=[1,2,3]},
                               #point{value=0,data=[9,4,5]},
                               fun val2/1)),
    ?_assert(true)
  ].

shrink_test_() ->
  [
    ?_assertEqual([#point{value=12.0,data=[5.0,3.0,4.0]},
                    #point{value=10.0,data=[4.0,4.0,2.0]}],
                erlplex:shrink(#point{value=0,data=[1,2,3]},
                               [#point{value=0,data=[9,4,5]},
                                #point{value=0,data=[7,6,1]}],
                               fun val/1)),
    ?_assert(true)
  ].


func10     ([X])   -> abs(10-X).
func_5_7   ([X,Y]) -> abs(5-X) * abs(7-Y).
func_76_99 ([X,Y]) -> abs(76-X) + abs(99-Y).

step_test() ->
    Func10   = fun func10/1,
    Func_5_7 = fun func_5_7/1,
    % reflection
    ?assertEqual(#simplex{points=[#point{value=5,data=[5]},#point{value=1.0,data=[9.0]}],
                           func=Func10},
        erlplex:step(  #simplex{points=[#point{value=9,data=[1]},#point{value=5,data=[5]}],
                        func=Func10})),
    % extension
    ?assertEqual(#simplex{points=[#point{value=7,data=[3]},#point{value=3.0,data=[7.0]}],
                           func=Func10},
        erlplex:step(  #simplex{points=[#point{value=9,data=[1]},#point{value=7,data=[3]}],
                        func=Func10})),
    % contraction-1
    ?assertEqual(#simplex{points=[#point{value=2,data=[12]},#point{value=1.0,data=[9.0]}],
                           func=Func10},
        erlplex:step(  #simplex{points=[#point{value=4,data=[6]},#point{value=2,data=[12]}],
                        func=Func10})),
    % contraction-2
    ?assertEqual(#simplex{points=[#point{value=2,data=[8]},#point{value=1.0,data=[11.0]}],
                           func=Func10},
        erlplex:step(  #simplex{points=[#point{value=8,data=[2]},#point{value=2,data=[8]}],
                        func=Func10})),
    % shrink FIXME how do I get a shrink condition?
    ?assertEqual(#simplex{points=[ #point{value=3.0,data=[6.0,10.0]},
                                    #point{value=1,data=[9,5]},
                                    #point{value=1,data=[1,9]}],
                                func=Func_5_7},
        erlplex:step(  #simplex{points=[#point{value=9,data=[3,1]},
                                        #point{value=1,data=[9,5]},
                                        #point{value=1,data=[1,9]}],
                                func=Func_5_7})),
    ?assert(true).

steps_1_test() ->
    Func10   = fun func10/1,
    ?assertEqual(#simplex{points=[ #point{value=6.0,data=[4.0]},
                                    #point{value=2.0,data=[8.0]}],
                          func=Func10},
        erlplex:steps(  #simplex{points=[#point{value=9,data=[1]},
                                         #point{value=8,data=[2]}],
                                 func=Func10},
                        0.0000000000001,
                        2)),
    ?assert(true).

steps_2_test() ->
    Func_76_99 = fun func_76_99/1,
    Target = [76,99],
    Result = erlplex:steps(#simplex{points=[#point{value=161,data=[3,11]},
                                            #point{value=159,data=[9,7]},
                                            #point{value=150,data=[3,22]}],
                                    func=Func_76_99},
                           0.0000000000001,
                           3000),
    ?assert((hd(Result#simplex.points))#point.value < 0.0000001),
    ?assert(dist(Target,(hd(Result#simplex.points))#point.data) < 0.0000001),
    ?assert(dist(Target,(lists:last(Result#simplex.points))#point.data) < 0.0000001),
    ?assert(true).

new_simplex_test() ->
    Func_76_99   = fun func_76_99/1,
    ?assertEqual(#simplex{func=Func_76_99, points=[#point{value=13,data=[70,92]},
                                                   #point{value=10,data=[75,90]},
                                                   #point{value=7,data=[79,95]}]},
                 erlplex:create_simplex(Func_76_99, [[75,90],[70,92],[79,95]])),
    % FIXME need to test that all data points have the same number of dimensions?
    ?assert(true).

solve_test() ->
    Func_76_99   = fun func_76_99/1,
    Target = [76,99],
    Plex = erlplex:create_simplex(Func_76_99, [[75,90],[70,92],[79,95]]),
    Result = erlplex:solve(Plex),
    ?assert((hd(Result#simplex.points))#point.value < 0.0000001),
    ?assert(dist(Target,(hd(Result#simplex.points))#point.data) < 0.0000001),
    ?assert(dist(Target,(lists:last(Result#simplex.points))#point.data) < 0.0000001),
    ?assert(true).


