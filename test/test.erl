-module(test).
-include("../include/erlplex.hrl").
-include_lib("eunit/include/eunit.hrl").

val(Vector)     -> lists:foldl(fun (A,B) -> A+B end, 0, Vector).
val2(Vector)    -> lists:foldl(fun (A,B) -> B-A end, 0, Vector).


% ------------ tests -------------
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


step_test_() ->
  [
    % reflection
    ?_assertEqual(#simplex{points=[#point{value=5,data=[5]},#point{value=1.0,data=[9.0]}]},
        erlplex:step(  #simplex{points=[#point{value=9,data=[1]},#point{value=5,data=[5]}]},
                        fun ([X]) -> abs(10-X) end)),
    % extension
    ?_assertEqual(#simplex{points=[#point{value=7,data=[3]},#point{value=3.0,data=[7.0]}]},
        erlplex:step(  #simplex{points=[#point{value=9,data=[1]},#point{value=7,data=[3]}]},
                        fun ([X]) -> abs(10-X) end)),
    % contraction-1
    ?_assertEqual(#simplex{points=[#point{value=2,data=[12]},#point{value=1.0,data=[9.0]}]},
        erlplex:step(  #simplex{points=[#point{value=4,data=[6]},#point{value=2,data=[12]}]},
                        fun ([X]) -> abs(10-X) end)),
    % contraction-2
    ?_assertEqual(#simplex{points=[#point{value=2,data=[8]},#point{value=1.0,data=[11.0]}]},
        erlplex:step(  #simplex{points=[#point{value=8,data=[2]},#point{value=2,data=[8]}]},
                        fun ([X]) -> abs(10-X) end)),
    % shrink FIXME how do I get a shrink condition?
    ?_assertEqual(#simplex{points=[ #point{value=2,data=[8,3]},
                                    #point{value=1,data=[9,4]},
                                    #point{value=1,data=[9,2]}]},
        erlplex:step(  #simplex{points=[#point{value=9,data=[3,1]},
                                        #point{value=1,data=[9,5]},
                                        #point{value=1,data=[1,9]}]},
                        fun ([X,Y]) -> abs(5-X) * abs(7-Y) end)),
    ?_assert(true)
  ].

steps_1_test() ->
    ?assertEqual(#simplex{points=[ #point{value=6.0,data=[4.0]},
                                    #point{value=2.0,data=[8.0]}]},
        erlplex:steps(  #simplex{points=[#point{value=9,data=[1]},
                                         #point{value=8,data=[2]}]},
                        fun ([X]) -> abs(10-X) end,
                        2)),
    ?assert(true).

steps_2_test() ->
    Target = [76,99],
    Result = erlplex:steps(#simplex{points=[#point{value=161,data=[3,11]},
                                            #point{value=159,data=[9,7]},
                                            #point{value=150,data=[3,22]}]},
                           fun ([X,Y]) -> abs(76-X) + abs(99-Y) end,
                           3000),
    ?assertEqual(#point{value=0,data=Target},hd(Result#simplex.points)),
    ?assertEqual(#point{value=0,data=Target},lists:last(Result#simplex.points)),
    ?assert(true).

