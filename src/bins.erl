%% Author: eschneef
%% Created: Sep 19, 2008
%% Description: This module takes some number of arbitrarily sized objects 
%% and places them in some arbitrary number of container. The method somewhat 
%% naïve; it places the largest object in the container with the most open 
%% space until that is no longer enough space in any container to hold any 
%% of the remaining objects. It reads the file 'bins.txt' in the local 
%% directory to get information about the objects and containers. The first 
%% line of the file is the number of containers, the second is the capacity 
%% of the containers, the following lines are the sizes of each object to be 
%% placed.

-module(bins).

-export([start/0]).

start() ->
    Fprint = fun(Bins, Boxes) ->
                     lists:foreach(
                       fun({bin, BinId, Rem, BoxList}) -> 
                               [Box|List] = lists:reverse(BoxList),
                               io:fwrite("id: ~w, rem: ~w, boxes: ~w", [BinId, Rem, Box]),
                               lists:foreach(fun(X) -> io:fwrite(", ~w", [X]) end, List),
                               io:nl()
                       end, Bins),
                     case Boxes of
                         [] ->
                             ok;
                         Boxes ->
                             io:fwrite("remaining ~w boxes ~w~n", [length(Boxes), Boxes])
                     end
             end,
    
    case file:open("bins.txt", [read]) of
        {ok, Fid} ->
            {Bins, Boxes} = parse(Fid),
            file:close(Fid),
            case pack(length(Boxes), Bins, [], Boxes) of
                {failed, B, R} ->
                    io:fwrite("Too many boxes~n", []),
                    Fprint(B, R);
                {done, B} ->
                    io:fwrite("done~n", []),
                    Fprint(B, []);
                Other ->
                    io:fwrite("Other ~w~n", [Other])
            end;
        {error, Error} ->
            {error, Error}
    end.

parse(Fid) ->
    {ok, [Bins]} = io:fread(Fid, '', "~d"),
    {ok, [Size]} = io:fread(Fid, '', "~d"),
    {lists:map(fun(N) -> {bin, N, trunc(Size), []} end, lists:seq(1, Bins)), 
     lists:reverse(lists:sort(get_boxes(Fid, [])))}.

get_boxes(Fid, Boxes) ->
    case io:fread(Fid, '', "~d") of
        eof ->
            Boxes;
        {error, {fread, integer}} ->
            Boxes;
        {ok, [Box]} ->
            get_boxes(Fid, [Box|Boxes]);
        Other ->
            io:fwrite("~w~n", [Other])
    end.

pack(0, OldBins, NewBins, Boxes) ->
    {failed, lists:keysort(2, OldBins ++ NewBins), Boxes};
pack(_, OldBins, NewBins, []) ->
    {done, lists:keysort(2, OldBins ++ NewBins)};
pack(N, [], Bins, Boxes) ->
    pack(N, lists:reverse(lists:keysort(3, Bins)), [], Boxes);
pack(N, OldBins = [{bin, BinId, Remaining, In}|Bins], NewBins, AllBoxes = [Box|Boxes]) ->
    case lists:member(Remaining, AllBoxes) of
        true ->
            pack(N, Bins, [{bin, BinId, 0, [Remaining|In]}|NewBins], AllBoxes -- [Remaining]);
        false ->
            case Remaining - Box of
                Space when Space < 0 ->
                    pack(N-1, OldBins, NewBins, rotate(AllBoxes));
                Space ->
                    pack(N, Bins, [{bin, BinId, Space, [Box|In]}|NewBins], Boxes)
            end
    end.

rotate([H|T]) -> T ++ [H].
